module Nodes
       ( runOnNodes
       ) where

import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import qualified Control.Distributed.Process.Node as N
import Data.Foldable
import Data.Traversable
import qualified Network.Transport.TCP as NT (createTransport, defaultTCPParameters)

thisManyNodes = 10

transportPort = 10501

initialPeerDiscoveryTimeout = 500 -- µs, this time is “lost” (taken
                                  -- from at least `--send-for SEC`)

-- |Generates n-th loopback address.
--
-- FIXME: should use 'System.Socket.Family.Inet' or something similar
-- (an obvious bug now is generating more than 254 addresses).
nthLoopback :: Int -> String
nthLoopback n = "127.0.0." ++ show n

-- |Starts a 'SimpleLocalnet' 'Backend' with one node on it, and runs
-- a given 'process' supplying it with an initial list of its peers.
--
-- WRT 'logger' process: each node is started in its own Backend, in
-- its own thread, to simulate multiple machines. This presents a
-- problem with calls to `say`—their content would be interleaved when
-- printed to stderr… Because we’re simulating n machines in the same
-- OS process, let’s ‘cheat’ by creating a shared logger DP process.
oneNode :: ProcessId -> (Int, [NodeId] -> Process ()) -> IO ()
oneNode logger (nth, process) = do
  backend <- initializeBackend (nthLoopback nth) (show transportPort) N.initRemoteTable
  node <- newLocalNode backend
  peers <- findPeers backend initialPeerDiscoveryTimeout
  N.runProcess node $ do
    reregister "logger" logger
    process peers

-- |If you wish to run the app on a different set of nodes,
-- reimplement this function. 'processGen' is a list of processes you
-- should be consecutively starting on every node you launch.
runOnNodes :: [[NodeId] -> Process ()] -> IO ()
runOnNodes processGen = do
  loggerPidMV <- newEmptyMVar
  forkIO $ loggerNode loggerPidMV
  loggerPid <- takeMVar loggerPidMV
  threads <- forM args $ forkJoinIO . oneNode loggerPid
  forM_ threads takeMVar -- wait for all nodes except the logger
    where
      args = [1..thisManyNodes] `zip` (cycle processGen) -- cycle in
                                                         -- case we
                                                         -- were to
                                                         -- run out of
                                                         -- Process
                                                         -- definitions

-- |A 'forkIO' that can be waited for.
forkJoinIO :: IO () -> IO (MVar ())
forkJoinIO action = do
  mvar <- newEmptyMVar
  forkFinally action (\_ -> putMVar mvar ())
  return mvar

-- |This node’s '"logger"' process is used throughout the app.
loggerNode :: MVar ProcessId -> IO ()
loggerNode pid = do
  Right t <- NT.createTransport "127.255.255.1" (show transportPort) NT.defaultTCPParameters
  node <- N.newLocalNode t N.initRemoteTable
  N.runProcess node $ do
    Just logger <- whereis "logger"
    liftIO $ putMVar pid logger
    expect :: Process () -- keep the artificial shared logger node
                         -- running, until killed after main thread
                         -- ends (after joining all regular nodes)
