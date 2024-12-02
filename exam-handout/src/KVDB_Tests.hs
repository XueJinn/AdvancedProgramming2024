module KVDB_Tests (tests) where

import KVDB
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void)

tests :: TestTree
tests =
  testGroup
    "KVDB"
    [ testCase "Basic put/get" $ do
      db <- startKVDB
      kvPut db "key1" (42 :: Int)
      value <- kvGet db "key1"
      value @?= 42
      
    , testCase "Get blocks until put" $ do
        db <- startKVDB
        resultMVar <- newEmptyMVar
        -- Execute get in a new thread
        void $ forkIO $ do
          value <- kvGet db "key2"
          putMVar resultMVar value
        -- Delay for 0.1 seconds to put the value
        threadDelay 100000
        kvPut db "key2" (100 :: Int)
        value <- takeMVar resultMVar
        value @?= 100

  , testCase "Multiple waiters" $ do
        db <- startKVDB
        mvar1 <- newEmptyMVar
        mvar2 <- newEmptyMVar
        
        void $ forkIO $ do
          value <- kvGet db "key3"
          putMVar mvar1 value
          
        void $ forkIO $ do
          value <- kvGet db "key3"
          putMVar mvar2 value

        threadDelay 100000
        kvPut db "key3" (200 :: Int)

        value1 <- takeMVar mvar1
        value2 <- takeMVar mvar2
        value1 @?= 200
        value2 @?= 200

  , testCase "Put overwrites old value" $ do
      db <- startKVDB
      kvPut db "key4" (1 :: Int)
      kvPut db "key4" 2
      value <- kvGet db "key4"
      value @?= 2

  , testCase "Polymorphic values" $ do
      db <- startKVDB
      kvPut db (1 :: Int) "test"
      str <- kvGet db 1
      str @?= "test"
      db2 <- startKVDB
      kvPut db2 "flag" True
      bool <- kvGet db2 "flag"
      bool @?= True
  ]
