module Runner
    ( run
    , Configuration(..)
    ) where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Clock
import System.Random

import Nodes

data Configuration = Configuration
  { sendFor :: Int
  , gracePeriod :: Int
  , seed :: Int
  } deriving (Show, Eq)

process :: RandomGen g => g -> UTCTime -> UTCTime -> [NodeId] -> Process ()
process rng stopSendingAt stopReceivingAt initialPeers = do
  say $ "process started, some random value = " ++ show (fst $ random rng :: Double)
  let sec = 1
  liftIO $ threadDelay $ sec * 1000 * 1000
  say $ "waited " ++ show sec ++ " sec"
  return ()

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
