{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Runner
    ( run
    , Configuration(..)
    ) where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe (fromMaybe, isJust)
import Data.Time.Clock
import System.Random
import Data.Binary
import Data.Binary.Orphans
import Data.Typeable
import GHC.Generics
import qualified System.Exit as SE (die)

import Nodes

data Configuration = Configuration
  { sendFor :: Int
  , gracePeriod :: Int
  , seed :: Int
  } deriving (Show, Eq)

processName = "main"

data ProcessState = ProcessState
  { knownPeers :: [NodeId]
  , m :: [RealMsg] -- FIXME
  } deriving (Show, Eq)

data RealMsg = RealMsg
   { sentAt :: UTCTime -- the \tau function
   , payload :: Double
   } deriving (Show, Eq, Generic, Typeable)
instance Binary RealMsg

data StopSending = StopSending deriving (Show, Eq, Generic, Typeable)
instance Binary StopSending

randomRealMsg :: RandomGen g => g -> UTCTime -> (RealMsg, g)
randomRealMsg g sentAt = (msg, nextG) where
  (v, nextG) = random g
  msg = RealMsg
    { sentAt = sentAt
    , payload = 1.0 - v -- has to ∈ (0,1] (while `v` ∈ [0,1))
    }

calculateTuple :: [RealMsg] -> (Int, Double)
calculateTuple msgs =
  (len, score)
    where
      len = length msgs
      sorted = sortOn sentAt msgs
      indexed = zip [1..] sorted
      products = (\(i,m_i) -> i * (payload m_i)) <$> indexed
      score = sum products

sameElements :: (Eq a) => [a] -> [a] -> Bool -- FIXME: use Set
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

receiveNewPeers :: ProcessState -> [NodeId] -> Process ProcessState -- FIXME: use reader
receiveNewPeers currentState newPeers = do
  let merged = nub $ (knownPeers currentState) ++ newPeers
  say $ "received new peers: " ++ show newPeers
  unless (sameElements merged (knownPeers currentState)) $ do -- if someone new is found,
    say $ "  sending my new peers further (got " ++ show (length merged) ++ ")"
    notifyPeersOfPeers merged                                 -- tell everyone of the fact
  say $ "waiting with knownPeers = " ++ show merged
  return ProcessState { knownPeers = merged
                      , m = m currentState } -- FIXME: use lens

receiveRealMessage :: ProcessState -> RealMsg -> Process ProcessState
receiveRealMessage currentState msg = do
  --say $ "got " ++ show msg
  return ProcessState { knownPeers = knownPeers currentState
                      , m = msg:(m currentState) }

-- Send all my known peers to all my known peers (but myself).
notifyPeersOfPeers :: [NodeId] -> Process ()
notifyPeersOfPeers peers = do
  myself <- getSelfNode
  forM_ (filter (/= myself) peers) $ \p -> nsendRemote p processName peers

childSender :: RandomGen g => g -> [NodeId] -> Process ()
childSender rng peers = do
  now <- liftIO $ getCurrentTime
  let (msg, nextRng) = randomRealMsg rng now
  forM_ peers $ \p -> nsendRemote p processName msg
  -- check mailbox for peer updates and stop signal in a non-blocking manner
  newPeers    <- expectTimeout 0 :: Process (Maybe [NodeId])
  stopSending <- expectTimeout 0 :: Process (Maybe StopSending)
  unless (isJust stopSending) $
    childSender nextRng (fromMaybe peers newPeers)

-- Will send the given `msg` to `rcpt` at `time`.
timer :: Serializable a => UTCTime -> a -> ProcessId -> Process ()
timer time msg rcpt = do
  now <- liftIO $ getCurrentTime
  let inThisManyUs = round $ (*1000000) $ diffUTCTime time now
  liftIO $ threadDelay inThisManyUs
  send rcpt msg

killer :: UTCTime -> Process ()
killer at = do
  getSelfPid >>= spawnLocal . timer at ()
  _ <- expect :: Process ()
  liftIO $ SE.die "killer: time is up" -- spec says “«program» is killed”

receiveAllRemainingMessagesDuringGrace = True -- I’m not sure if that’s what the spec says

process :: RandomGen g => g -> UTCTime -> UTCTime -> [NodeId] -> Process ()
process rng stopSendingAt killAt initialPeers = do

  ------------- spec-0: sending messages -------------

  myself <- getSelfPid
  register processName myself
  spawnLocal $ killer killAt -- potentially, will execute spec-2: killing

  say $ "my initialPeers (of length " ++ show (length initialPeers) ++ ") = " ++ show initialPeers

  let initialState = ProcessState { knownPeers = initialPeers, m = [] }
  notifyPeersOfPeers $ knownPeers initialState

  localSender <- spawnLocal $ childSender rng initialPeers

  -- timers
  forM_ [myself, localSender] $ spawnLocal . timer stopSendingAt StopSending

  let loop0 state = do -- FIXME: use `until`?
       (nextState,continue) <- receiveWait
          [ match $ \(m :: StopSending) -> do
              say "got StopSending!"
              return (state,False)
          , match $ \m -> (,True) <$> receiveRealMessage state m
          , match $ \m -> (,True) <$> receiveNewPeers    state m ]
       if continue
         then loop0 nextState
         else return nextState

  preGraceState <- loop0 initialState

  say $ "pre-grace: I’ve got " ++ show (length $ m preGraceState) ++ " messages so far."

  ------------- spec-1: grace period -------------

  let loop1 state = do
        -- receive with timeout == 0 — just grab what already is in there
        nextState <- receiveTimeout 0 [ match $ receiveRealMessage state ]
        case nextState of
          Nothing -> return state
          Just ns -> loop1 ns

  finalState <- if receiveAllRemainingMessagesDuringGrace
                then loop1 preGraceState
                else return preGraceState

  say $ "peri-grace: I’ve got " ++ show (length $ m finalState) ++ " messages in the final state."

  liftIO $ putStrLn $ show $ calculateTuple $ m finalState

-- Now, we need to somehow provide *different* RNGs to consecutive
-- processes. To not pollute the configuration module (Nodes) with the
-- RNG concept, let’s provide an infinite list of processes to
-- Nodes.runOnNodes. Each process will then have a different RNG, as
-- defined below.

run :: Configuration -> IO ()
run unsafeConf = do
  now <- getCurrentTime
  let stopSendingAt   = addSec now $ sendFor conf
  let killAt = addSec stopSendingAt $ gracePeriod conf
  runOnNodes $ fmap (\p -> p stopSendingAt killAt) processesWithRngs
    where
      processesWithRngs = map (\rng -> process rng) rngs
      rngs = iterate (snd . split) firstRng
      firstRng = mkStdGen $ seed conf
      addSec a b = addUTCTime (fromInteger $ toInteger $ b) a
      conf = sanitize unsafeConf
      sanitize (Configuration a b c) = Configuration (abs a) (abs b) (abs c)
