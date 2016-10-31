{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module KeepAlive
       ( pingPeers
       , PeerVisibility(..)
       ) where

import Control.Concurrent
import Control.Distributed.Process
import Control.Monad
import Data.Binary
import Data.Binary.Orphans
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Time.Clock
import Data.Typeable
import GHC.Generics

-- |Internal messages used to communicate changes in visible network
-- topology.
data PeerVisibility = PeerLeft NodeId | PeerIsBack NodeId
                    deriving (Show, Eq, Generic, Typeable)
instance Binary PeerVisibility

data PingerState = PingerState
  { notificationsTo :: ProcessId
  , subpingers :: M.Map NodeId ProcessId
  }

data SubpingerState = SubpingerState
  { lastHeardOf :: UTCTime
  , isZombie :: Bool
  }

-- |Name of the 'peerPinger' process, responsible for deciding which
-- connections are still up. It might not be a good idea to use
-- 'nsendRemote', but, for example, “logger” is using this approach
-- (?).
processName = "pinger"

-- |Pinging frequency.
pingEvery = 3 * 1000 * 1000 -- µs

-- |How long to wait for the Pong until we assume the communication is
-- broken.
markZombieAfter = pingEvery -- µs

-- |A keep-alive message sent back and forth between 'peerPinger'
-- processes of the network nodes to monitor visibility. Its 'NodeId'
-- param is the ID of the *sender* node (to which a 'Pong' reply is to
-- be sent in case of receiving a 'Ping').
data PingPong = Ping NodeId | Pong NodeId
              deriving (Show, Eq, Generic, Typeable)
instance Binary PingPong

sender :: PingPong -> NodeId
sender (Ping n) = n
sender (Pong n) = n

-- |Spawns a local process that will ping and discover network
-- failures between it and corresponding '"pinger"' processes
-- on other nodes.
pingPeers :: ProcessId -> [NodeId] -> Process ProcessId
pingPeers notificationsTo initialPeers = do
  pid <- spawnLocal $ do
    myself <- getSelfPid
    register processName myself
    subs <- forM initialPeers $ \node -> do
      sub <- spawnSubpinger myself node
      return (node, sub)
    pinger $ PingerState
      { notificationsTo = notificationsTo
      , subpingers = M.fromList subs
      }
  return pid

-- |Receives and routes 'PingPong' to appropriate 'subpinger's. Also,
-- receives new nodes and starts new 'subpinger's for them.
pinger :: PingerState -> Process ()
pinger state = do
  myself <- getSelfPid
  nextState <- receiveWait
    [ match $ \(m :: PingPong) -> do
        -- forward 'PingPong's
        case (M.lookup (sender m) (subpingers state)) of
          Just sub -> do
            -- forward
            send sub m
            return state
          Nothing -> do
            -- start a new 'subpinger' for the unknown Node
            sub <- spawnSubpinger myself (sender m)
            send sub m
            return $ state { subpingers = M.insert (sender m) sub (subpingers state) }
    , match $ \(newNodes :: [NodeId]) -> do
        -- receive new nodes and start their subpingers
        let merge oldMap newNode =
              case (M.lookup newNode oldMap) of
                Just sub -> return oldMap -- already known
                Nothing -> do
                  newSub <- spawnSubpinger myself newNode
                  return $ M.insert newNode newSub oldMap
        newSubs <- foldM merge (subpingers state) newNodes
        return state { subpingers = newSubs }
    , match $ \(m :: PeerVisibility) -> do
        -- receive and forward PeerVisibility
        send (notificationsTo state) m
        return state
    ]
  pinger nextState

spawnSubpinger :: ProcessId -> NodeId -> Process (ProcessId)
spawnSubpinger reportTo watchedNode = do
  now <- liftIO $ getCurrentTime
  spawnLocal $ subpinger reportTo watchedNode $ SubpingerState
    { lastHeardOf = now -- seems semantically correct
    , isZombie = False
    }

-- |Watches just one 'NodeId' reporting back to the main '"pinger"'
-- process.
subpinger :: ProcessId -> NodeId -> SubpingerState -> Process()
subpinger reportTo watchedNode initialState = do
  myNode <- getSelfNode

  -- reply to pings
  -- FIXME: perhaps, getting a Ping should also update 'lastHeardOf'?
  localPonger <- spawnLocal $ forever $ do
    ping <- expect :: Process PingPong
    -- say $ "got Ping: " ++ show ping
    nsendRemote watchedNode processName $ Pong myNode

  -- constantly ping the watched node
  localPinger <- spawnLocal $ do
    let loop oldState = do
          nsendRemote watchedNode processName $ Ping myNode
          pong <- expectTimeout markZombieAfter :: Process (Maybe PingPong)
          -- FIXME: we should non-blockingly slurp all remaining Pongs
          -- that might still be in the inbox, in case someone sent
          -- them just-like-that, not in reply to our Ping? If we
          -- don’t handle this slightly malicious behavior, the inbox
          -- has a potential to grow indefinitely.
          nextState <- if isJust pong
            then do
              -- say $ "got Pong: " ++ show pong
              now <- liftIO $ getCurrentTime
              return $ oldState { lastHeardOf = now, isZombie = False }
            else do
              -- say $ "got ping timeout"
              return $ oldState { isZombie = True }
          let wasZombie    = isZombie oldState
          let willBeZombie = isZombie nextState
          when (wasZombie && (not willBeZombie)) $ send reportTo $ PeerIsBack watchedNode
          when ((not wasZombie) && willBeZombie) $ send reportTo $ PeerLeft   watchedNode
          liftIO $ threadDelay pingEvery -- maybe we should use
                                         -- exponential back-off in
                                         -- both success and failure
                                         -- cases to reduce chattiness?
          loop nextState
    loop initialState

  -- forward Pings to localPonger and Pongs to localPinger
  forever $ do
    m <- expect :: Process PingPong
    case m of
      Ping _ -> send localPonger m
      Pong _ -> send localPinger m
