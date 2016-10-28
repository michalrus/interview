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

-- |For the sake of simplicity, let’s treat a node as having only one
-- process with this name (at least one which other nodes communicate
-- with).
processName = "main"

-- |How frequently to search for new peer nodes in the network
findNewPeersEvery = 2 * 1000 * 1000 -- µs

-- |Should we receive ALL of the remaining messages during the grace
-- period? I’m not really sure if that’s what the spec says.
receiveAllRemainingMessagesDuringGrace = True

data ProcessState = ProcessState
  { knownPeers :: [NodeId]
  , receivedMessages :: [RealMsg]
  } deriving (Show, Eq)

data RealMsg = RealMsg
   { sentAt :: UTCTime -- the τ function
   , payload :: Double
   } deriving (Show, Eq, Generic, Typeable)
instance Binary RealMsg

data StopSending = StopSending deriving (Show, Eq, Generic, Typeable)
instance Binary StopSending

-- |Will generate a random message to send between processes.
randomRealMsg :: RandomGen g => g -> UTCTime -> (RealMsg, g)
randomRealMsg g sentAt = (msg, nextG) where
  (v, nextG) = random g
  msg = RealMsg
    { sentAt = sentAt
    , payload = 1.0 - v -- has to ∈ (0,1] (while `v` ∈ [0,1))
    }

-- |From a list of received messages, computes the final result tuple.
calculateTuple :: [RealMsg] -> (Int, Double)
calculateTuple msgs =
  (len, score)
    where
      len = length msgs
      sorted = sortOn sentAt $ reverse msgs
      indexed = zip [1..] sorted
      products = (\(i,m_i) -> i * (payload m_i)) <$> indexed
      score = sum products

-- |Checks if two lists have exactly the same elements
-- (order-insensitive). It’s used only for merging peer lists, so the
-- overhead of using lists is probably negligible.
--
-- FIXME: use Data.Set
sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs \\ ys) && null (ys \\ xs)

-- |Processes peer list received from a neighbor node.
receiveNewPeers :: ProcessState -> [NodeId] -> Process ProcessState -- FIXME: State?
receiveNewPeers currentState newPeers = do
  let merged = nub $ (knownPeers currentState) ++ newPeers
  say $ "received new peers: " ++ show newPeers
  unless (sameElements merged (knownPeers currentState)) $ do -- if someone new is found,
    say $ "  sending my new peers further (got " ++ show (length merged) ++ ")"
    notifyPeersOfPeers merged                                 -- tell everyone of the fact
  say $ "waiting with knownPeers = " ++ show merged
  return ProcessState { knownPeers = merged
                      , receivedMessages = receivedMessages currentState } -- FIXME: lens

-- |Processes a real message (with a payload) received from a neighboring node.
receiveRealMessage :: ProcessState -> RealMsg -> Process ProcessState
receiveRealMessage currentState msg = do
  --say $ "got " ++ show msg
  return ProcessState { knownPeers = knownPeers currentState
                      , receivedMessages = msg:(receivedMessages currentState) }

-- |Sends all known peers to all known peers (excluding the sending process).
notifyPeersOfPeers :: [NodeId] -> Process ()
notifyPeersOfPeers peers = do
  myself <- getSelfNode
  forM_ (filter (/= myself) peers) $ \p -> nsendRemote p processName peers

-- |Will run 'action' if there’s no 'StopSending' message in current
-- process’ mailbox.
doUnlessStopped :: Process () -> Process ()
doUnlessStopped action = do
  stopSending <- expectTimeout 0 :: Process (Maybe StopSending)
  unless (isJust stopSending) $ action

-- |A subprocess that’s constantly sending 'RealMsg's to known peers,
-- until it receives the 'StopSending' message. To update its list of
-- peers, send… a new list of peers (the old one will be substituted).
childSender :: RandomGen g => g -> [NodeId] -> Process ()
childSender rng peers = do
  now <- liftIO $ getCurrentTime
  let (msg, nextRng) = randomRealMsg rng now
  forM_ peers $ \p -> nsendRemote p processName msg
  -- check mailbox for peer updates (in a non-blocking manner)
  newPeers <- expectTimeout 0 :: Process (Maybe [NodeId])
  doUnlessStopped $ childSender nextRng (fromMaybe peers newPeers)

-- |Keeps finding new peers every few seconds and sending them back to
-- the main process ('sendTo'). Implementation of 'findPeers' is
-- highly dependent on the network. E.g. it could be a UDP multicast.
keepFindingPeers :: (Int -> Process [NodeId]) -> ProcessId -> Process ()
keepFindingPeers findPeers sendTo = do
  found <- findPeers findNewPeersEvery
  unless (null found) $ send sendTo found
  doUnlessStopped $ keepFindingPeers findPeers sendTo

-- |Will send the given 'msg' to 'rcpt' at the given 'time'.
timer :: Serializable a => UTCTime -> a -> ProcessId -> Process ()
timer time msg rcpt = do
  now <- liftIO $ getCurrentTime
  let inThisManyUs = round $ (*1000000) $ diffUTCTime time now
  liftIO $ threadDelay inThisManyUs
  send rcpt msg

-- |Will kill the whole operating system process with this program at
-- the given time ('at').
killer :: UTCTime -> Process ()
killer at = do
  getSelfPid >>= spawnLocal . timer at ()
  _ <- expect :: Process ()
  liftIO $ SE.die "killer: time is up" -- spec says “«program» is killed”

-- |The main/root process of every node.
process :: RandomGen g => g -> UTCTime -> UTCTime -> [NodeId] -> (Int -> Process [NodeId]) -> Process (Int, Double)
process rng stopSendingAt killAt initialPeers findPeers = do

  ------------- spec-0: sending messages -------------

  myself <- getSelfPid
  register processName myself
  spawnLocal $ killer killAt -- potentially, will execute spec-2: killing

  say $ "my initialPeers (of length " ++ show (length initialPeers) ++ ") = " ++ show initialPeers

  let initialState = ProcessState { knownPeers = initialPeers, receivedMessages = [] }
  notifyPeersOfPeers $ knownPeers initialState

  localSender <- spawnLocal $ childSender rng initialPeers
  peerFinder <- spawnLocal $ keepFindingPeers findPeers myself

  -- timers
  forM_ [myself, localSender, peerFinder] $ spawnLocal . timer stopSendingAt StopSending

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

  say $ "pre-grace: I’ve got " ++ show (length $ receivedMessages preGraceState) ++ " messages so far."

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

  say $ "peri-grace: I’ve got " ++ show (length $ receivedMessages finalState) ++ " messages in the final state."

  return $ calculateTuple $ receivedMessages finalState

-- |Runs the 'process' on many nodes, printing the results to stdout.
--
-- Now, we need to somehow provide *different* RNGs to consecutive
-- processes. To not pollute the configuration module (Nodes) with the
-- RNG concept, let’s provide an infinite list of processes to
-- Nodes.runOnNodes. Each process will then have a different RNG, as
-- defined below.
run :: Configuration -> IO ()
run unsafeConf = do
  now <- getCurrentTime
  let stopSendingAt = addSec now $ sendFor conf
  let killAt = addSec stopSendingAt $ gracePeriod conf
  runOnNodes $ fmap (\p -> p stopSendingAt killAt) processesWithRngs
    where
      processesWithRngs = map (\rng -> processWithPrint rng) rngs
      rngs = iterate (snd . split) firstRng
      firstRng = mkStdGen $ seed conf
      processWithPrint a b c d e = liftIO . putStrLn . show =<< process a b c d e -- no sensible way to
                                                                                  -- do this point-free?
      addSec a b = addUTCTime (fromInteger $ toInteger $ b) a
      conf = sanitize unsafeConf
      sanitize (Configuration a b c) = Configuration (abs a) (abs b) (abs c)
