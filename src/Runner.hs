{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Runner
    ( run
    , Configuration(..)
    ) where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Monad
import Data.Foldable
import Data.List
import Data.Time.Clock
import System.Random
import Data.Binary
import Data.Binary.Orphans
import Data.Typeable
import GHC.Generics

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

randomRealMsg :: RandomGen g => g -> UTCTime -> (RealMsg, g)
randomRealMsg g sentAt = (msg, nextG) where
  (v, nextG) = random g
  msg = RealMsg
    { sentAt = sentAt
    , payload = 1.0 - v -- has to ∈ (0,1] (while `v` ∈ [0,1))
    }

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
receiveRealMessage currentState msg =
  return ProcessState { knownPeers = knownPeers currentState
                      , m = msg:(m currentState) }

-- Send all my known peers to all my known peers (but myself).
notifyPeersOfPeers :: [NodeId] -> Process ()
notifyPeersOfPeers peers = do
  myself <- getSelfNode
  forM_ (filter (/= myself) peers) $ \p -> nsendRemote p processName peers

process :: RandomGen g => g -> UTCTime -> UTCTime -> [NodeId] -> Process ()
process rng stopSendingAt stopReceivingAt initialPeers = do
  getSelfPid >>= register processName

  say $ "my initialPeers (of length " ++ show (length initialPeers) ++ ") = " ++ show initialPeers

  let initialState = ProcessState { knownPeers = initialPeers, m = [] }
  notifyPeersOfPeers $ knownPeers initialState

  let loop state = do
        nextState <- receiveWait [ match $ receiveNewPeers state
                                 , match $ receiveRealMessage state]
        loop nextState

  loop initialState

  liftIO $ threadDelay $ 1 * 1000 * 1000

-- Now, we need to somehow provide *different* RNGs to consecutive
-- processes. To not pollute the configuration module (Nodes) with the
-- RNG concept, let’s provide an infinite list of processes to
-- Nodes.runOnNodes. Each process will then have a different RNG, as
-- defined below.

run :: Configuration -> IO ()
run unsafeConf = do
  now <- getCurrentTime
  let stopSendingAt   = addSec now $ sendFor conf
  let stopReceivingAt = addSec stopSendingAt $ gracePeriod conf
  runOnNodes $ fmap (\p -> p stopSendingAt stopReceivingAt) processesWithRngs
    where
      processesWithRngs = map (\rng -> process rng) rngs
      rngs = iterate (snd . split) firstRng
      firstRng = mkStdGen $ seed conf
      addSec a b = addUTCTime (fromInteger $ toInteger $ b) a
      conf = sanitize unsafeConf
      sanitize (Configuration a b c) = Configuration (abs a) (abs b) (abs c)
