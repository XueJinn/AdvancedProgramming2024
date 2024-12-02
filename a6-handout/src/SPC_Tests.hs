module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef ( newIORef, readIORef, writeIORef, modifyIORef' )
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC (core)"
      [ testCase "add-job" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 1000000 >> writeIORef ref True) 2
          r1 <- jobStatus spc j
          r1 @?= JobPending
          -- Add a worker
          _ <- workerAdd spc "worker1"
          r2 <- jobStatus spc j
          r2 @?= JobRunning
          r3 <- jobWait spc j
          r3 @?= Done
          v <- readIORef ref
          v @?= True
          -- Check if worker can execute other jobs
          j2 <- jobAdd spc $ Job (writeIORef ref False) 1
          r4 <- jobWait spc j2
          r4 @?= Done
          v2 <- readIORef ref
          v2 @?= False,
        testCase "cancel-job" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 3
          _ <- workerAdd spc "worker1"
          jobCancel spc j
          r <- jobWait spc j
          r @?= DoneCancelled
          v <- readIORef ref
          v @?= False
          -- Ensure the worker is still working
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r2 <- jobWait spc j2
          r2 @?= Done
          v2 <- readIORef ref
          v2 @?= True,
        testCase "timeout" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= JobRunning
          r2 <- jobWait spc j
          r2 @?= DoneTimeout
          v <- readIORef ref
          v @?= False
          -- Ensure the worker is still running
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r3 <- jobWait spc j2
          r3 @?= Done
          v2 <- readIORef ref
          v2 @?= True,
        testCase "crash" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          j1 <- jobAdd spc $ Job (error "boom") 1
          r1 <- jobWait spc j1
          r1 @?= DoneCrashed
          -- Ensure new jobs can still work.
          ref <- newIORef False
          j2 <- jobAdd spc $ Job (writeIORef ref True) 1
          r2 <- jobWait spc j2
          r2 @?= Done
          v <- readIORef ref
          v @?= True,
        testCase "worker-gone" $ do
          spc <- startSPC
          ref <- newIORef False
          worker1 <- workerAdd spc "worker1"
          case worker1 of
            Right workerServer -> do
              j <- jobAdd spc $ Job (writeIORef ref True) 1
              r2 <- jobWait spc j
              r2 @?= Done
              v <- readIORef ref
              v @?= True
              workerStop workerServer
              j2 <- jobAdd spc $ Job (writeIORef ref False) 1
              threadDelay 1000000
              r3 <- jobStatus spc j2
              r3 @?= JobPending
            Left _ -> assertFailure "workerAdd failed",
        testCase "worker-gone-2" $ do
          spc <- startSPC
          ref <- newIORef False
          worker1 <- workerAdd spc "worker1"
          case worker1 of
            Right workerServer -> do
              j1 <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 3
              workerStop workerServer
              r1 <- jobWait spc j1
              r1 @?= DoneCancelled
              v <- readIORef ref
              v @?= False
            Left _ -> assertFailure "workerAdd failed",
            -- Add more worker
        testCase "add-workers" $ do
          spc <- startSPC
          _ <- workerAdd spc "worker1"
          _ <- workerAdd spc "worker2"
          _ <- workerAdd spc "worker3"
          -- Add several jobs to see if they are executed by different workers
          ref <- newIORef (0 :: Int)
          j1 <- jobAdd spc $ Job (threadDelay 2000000 >> modifyIORef' ref (+1)) 3
          j2 <- jobAdd spc $ Job (threadDelay 2000000 >> modifyIORef' ref (+1)) 3
          j3 <- jobAdd spc $ Job (threadDelay 2000000 >> modifyIORef' ref (+1)) 3
          threadDelay 1000000
          r1 <- jobStatus spc j1
          r2 <- jobStatus spc j2
          r3 <- jobStatus spc j3
          -- All jobs should be running
          r1 @?= JobRunning
          r2 @?= JobRunning
          r3 @?= JobRunning
          -- Wait for the jobs to finish
          r1' <- jobWait spc j1
          r2' <- jobWait spc j2
          r3' <- jobWait spc j3
          r1' @?= Done
          r2' @?= Done
          r3' @?= Done
          v <- readIORef ref
          v @?= 3
      ]
