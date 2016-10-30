{-# LANGUAGE ScopedTypeVariables #-}

module RunnerSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Monad
import Data.Time.Calendar
import Data.Time.Clock
import Network.Transport.InMemory
import System.Random

import Runner

tests :: TestTree
tests = testGroup "RunnerSpec"

  [ testCase "notifyPeersOfPeers should not send peers to ‘myself’" $ do
      transport <- createTransport
      nodes <- forM [1..7] $ \_ -> newLocalNode transport initRemoteTable
      let n1:n2:_ = nodes
      receivedNodesOtherMV <- newEmptyMVar
      forkIO $ runProcess n2 $ do
        getSelfPid >>= register processName
        r <- expectTimeout 5000 :: Process (Maybe [NodeId])
        liftIO $ putMVar receivedNodesOtherMV r
      receivedNodesSelf <- runProcess' n1 $ do
        getSelfPid >>= register processName
        notifyPeersOfPeers $ fmap localNodeId nodes
        expectTimeout 5000 :: Process (Maybe [NodeId])
      receivedNodesOther <- readMVar receivedNodesOtherMV
      receivedNodesSelf  @?= Nothing
      receivedNodesOther @?= Just (localNodeId <$> nodes)

  , testProperty "sameElements should work" $
    \(xs :: [Int], ys :: [Int]) -> (not $ null ys) ==> ioProperty $ do
      xss  <- generate $ shuffle xs
      xyss <- generate $ shuffle (xs ++ ys)
      return $
        (sameElements xs xss) &&
        (not $ sameElements xs xyss)

  , testProperty "randomRealMessage should generate correct messages" $
    \(seed :: Int) ->
      let
        rng = mkStdGen seed
        day = fromGregorian 1900 01 01
        daytime = secondsToDiffTime 0
        time = UTCTime day daytime
        (msg, _) = randomRealMsg rng time
      in payload msg <= 1.0 && payload msg > 0.0 && sentAt msg == time

  , testCase "receiveNewPeers should correctly update local ProcessState" $ do
      transport <- createTransport
      let genNodes num = forM [1..num] $ \_ -> newLocalNode transport initRemoteTable
      initialNodes  <- genNodes 4
      smoreNodes    <- genNodes 2
      evenMoreNodes <- genNodes 3
      let node = head initialNodes
      let stateWith nodes = ProcessState
            { knownPeers = localNodeId <$> nodes
            , receivedMessages = [] }
      let initialState = stateWith initialNodes
      let hop state nodes = runProcess' node $ receiveNewPeers state (localNodeId <$> nodes)
      -- TODO: test who in the neighborhood gets sent what
      nextState    <-  hop initialState []
      nextState    @?= initialState
      nextState'   <-  hop initialState smoreNodes
      nextState'   @?= (stateWith $ initialNodes ++ smoreNodes)
      nextState''  <-  hop nextState'   smoreNodes
      nextState''  @?= (stateWith $ initialNodes ++ smoreNodes)
      nextState''' <-  hop nextState''  evenMoreNodes
      nextState''' @?= (stateWith $ initialNodes ++ smoreNodes ++ evenMoreNodes)

  ]

testProcess :: (Eq a, Show a) => TestName -> a -> Process a -> TestTree
testProcess name expected prc = testCase name $ do
  result <- runSingleProcess prc
  result @?= expected

runSingleProcess :: Process a -> IO a
runSingleProcess prc = do
  transport <- createTransport
  node <- newLocalNode transport initRemoteTable
  runProcess' node prc

runProcess' :: LocalNode -> Process a -> IO a
runProcess' node prc = do
  result <- newEmptyMVar
  runProcess node $ do
    r <- prc
    liftIO $ putMVar result r
  readMVar result
