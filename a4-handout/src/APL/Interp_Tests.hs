module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

-- evalIO' :: Exp -> IO (Either Error Val)
-- evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests, testTryCatch, testKeyValueStore, testTransaction]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "State" $
        runEval
          ( do
              putState [(ValInt 0, ValInt 1)]
              modifyState $ map (\(key, _) -> (key, ValInt 5))
              getState
          )
          @?= ([], Right [(ValInt 0, ValInt 5)]),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero")
    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ())
        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]


testTryCatch :: TestTree
testTryCatch = testGroup "TryCatch Tests"
  [ testCase "Pure: Basic TryCatch - Success" $
      runEval (catch (pure "Success") (pure "Fallback")) @?= ([], Right "Success")
    
  , testCase "Pure: Basic TryCatch - Failure" $
      runEval (catch (failure "Error") (pure "Fallback")) @?= ([], Right "Fallback")
    
  , testCase "Pure: Nested TryCatch" $
      runEval (catch (catch (failure "Inner Error") (pure "Inner Fallback")) (pure "Outer Fallback"))
      @?= ([], Right "Inner Fallback")
    
  , testCase "Pure: TryCatch with Print" $
      runEval (catch (evalPrint "Before Error" >> failure "Error") (evalPrint "Fallback")) 
      -- @?= (["Before Error", "Fallback"], Right ())
      @?= (["Fallback"], Right ())
    
  , testCase "Pure: TryCatch with KV Store" $
      runEval (catch (evalKvPut (ValInt 1) (ValInt 100) >> failure "Error") (evalKvGet (ValInt 1))) 
      @?= ([], Left $ "Invalid key: " ++ show (ValInt 1))
    
  , testCase "IO: Basic TryCatch - Success" $ do
      result <- runEvalIO (catch (pure "Success") (pure "Fallback"))
      result @?= Right "Success"
    
  , testCase "IO: Basic TryCatch - Failure" $ do
      result <- runEvalIO (catch (failure "Error") (pure "Fallback"))
      result @?= Right "Fallback"
    
  , testCase "IO: Nested TryCatch" $ do
      result <- runEvalIO (catch (catch (failure "Inner Error") (pure "Inner Fallback")) (pure "Outer Fallback"))
      result @?= Right "Inner Fallback"
    
  , testCase "IO: TryCatch with KV Store" $ do
      result <- runEvalIO (catch (evalKvPut (ValInt 1) (ValInt 100) >> failure "Error") (evalKvGet (ValInt 1)))
      result @?= Right (ValInt 100)
    
  , testCase "IO: TryCatch with User Input" $ do
      (_, result) <- captureIO ["ValInt 200"] $ runEvalIO (catch (evalKvGet (ValInt 2)) (pure (ValInt 999)))
      result @?= Right (ValInt 200)
  ]


testKeyValueStore :: TestTree
testKeyValueStore = testGroup "Key-value Store Tests"
  [ testGroup "Pure Interpreter Tests"
    [ testCase "Put and Get" $
        runEval (do
          evalKvPut (ValInt 1) (ValInt 100)
          evalKvGet (ValInt 1)
        ) @?= ([], Right (ValInt 100))
    
    , testCase "Overwrite Value" $
        runEval (do
          evalKvPut (ValInt 1) (ValInt 100)
          evalKvPut (ValInt 1) (ValInt 200)
          evalKvGet (ValInt 1)
        ) @?= ([], Right (ValInt 200))
    
    , testCase "Get Non-existent Key" $
        runEval (evalKvGet (ValInt 1)) @?= ([], Left "Invalid key: ValInt 1")
    
    , testCase "Put and Get Different Types" $
        runEval (do
          evalKvPut (ValBool True) (ValInt 100)
          evalKvGet (ValBool True)
        ) @?= ([], Right (ValInt 100))
    ]
  
  , testGroup "IO Interpreter Tests"
    [ testCase "Put and Get" $ do
        result <- runEvalIO $ do
          evalKvPut (ValInt 1) (ValInt 100)
          evalKvGet (ValInt 1)
        result @?= Right (ValInt 100)
    
    , testCase "Overwrite Value" $ do
        result <- runEvalIO $ do
          evalKvPut (ValInt 1) (ValInt 100)
          evalKvPut (ValInt 1) (ValInt 200)
          evalKvGet (ValInt 1)
        result @?= Right (ValInt 200)
    
    , testCase "Get Non-existent Key with User Input" $ do
        (output, result) <- captureIO ["ValInt 999"] $ runEvalIO $ evalKvGet (ValInt 1)
        output @?= ["Invalid key: ValInt 1. Enter a replacement: "]
        result @?= Right (ValInt 999)
    
    , testCase "Put and Get Different Types" $ do
        result <- runEvalIO $ do
          evalKvPut (ValBool True) (ValInt 100)
          evalKvGet (ValBool True)
        result @?= Right (ValInt 100)
    
    , testCase "Invalid User Input" $ do
        (output, result) <- captureIO ["InvalidValue"] $ runEvalIO $ evalKvGet (ValInt 1)
        output @?= ["Invalid key: ValInt 1. Enter a replacement: "]
        result @?= Left "Invalid value input: InvalidValue"
    ]
  ]

testTransaction :: TestTree
testTransaction = testGroup "Transaction Tests"
  [ testGroup "Pure Interpreter Tests"
    [ testCase "Successful Transaction" $
        runEval (do
          transaction $ do
            evalKvPut (ValInt 1) (ValInt 100)
            evalKvPut (ValInt 2) (ValInt 200)
          evalKvGet (ValInt 1)
        ) @?= ([], Right (ValInt 100))
    
    , testCase "Failed Transaction" $
        runEval (do
          transaction $ do
            evalKvPut (ValInt 1) (ValInt 100)
            failure "Transaction failed"
          evalKvGet (ValInt 1)
        ) @?= ([], Left "Invalid key: ValInt 1")
    
    , testCase "Nested Transactions - Outer Fails" $
        runEval (do
          transaction $ do
            evalKvPut (ValInt 1) (ValInt 100)
            transaction $ do
              evalKvPut (ValInt 2) (ValInt 200)
            failure "Outer transaction failed"
          evalKvGet (ValInt 1) `catch` pure (ValInt 0)
        ) @?= ([], Right (ValInt 0))
    
    , testCase "Nested Transactions - Inner Fails" $
        runEval (do
          transaction $ do
            evalKvPut (ValInt 1) (ValInt 100)
            transaction $ do
              evalKvPut (ValInt 2) (ValInt 200)
              failure "Inner transaction failed"
            evalKvPut (ValInt 3) (ValInt 300)
          evalKvGet (ValInt 1)
        ) @?= ([], Right (ValInt 100))
    
    , testCase "Transaction with Print" $
        runEval (do
          transaction $ do
            evalPrint "Inside transaction"
            evalKvPut (ValInt 1) (ValInt 100)
          evalPrint "After transaction"
        ) @?= (["Inside transaction", "After transaction"], Right ())
    ]
  
  , testGroup "IO Interpreter Tests"
    [ testCase "Successful Transaction" $ do
        result <- runEvalIO $ do
          transaction $ do
            evalKvPut (ValInt 1) (ValInt 100)
            evalKvPut (ValInt 2) (ValInt 200)
          evalKvGet (ValInt 1)
        result @?= Right (ValInt 100)
    
    , testCase "Failed Transaction" $ do
        result <- runEvalIO $ do
          transaction $ do
            evalKvPut (ValInt 1) (ValInt 100)
            failure "Transaction failed"
          evalKvGet (ValInt 1)
        result @?= Left "Invalid value input: "
    
    , testCase "Nested Transactions - Outer Fails" $ do
        result <- runEvalIO $ do
          transaction $ do
            evalKvPut (ValInt 1) (ValInt 100)
            transaction $ do
              evalKvPut (ValInt 2) (ValInt 200)
            failure "Outer transaction failed"
          evalKvGet (ValInt 1) `catch` pure (ValInt 0)
        result @?= Right (ValInt 0)
    
    , testCase "Nested Transactions - Inner Fails" $ do
        result <- runEvalIO $ do
          transaction $ do
            evalKvPut (ValInt 1) (ValInt 100)
            transaction $ do
              evalKvPut (ValInt 2) (ValInt 200)
              failure "Inner transaction failed"
            evalKvPut (ValInt 3) (ValInt 300)
          evalKvGet (ValInt 1)
        result @?= Right (ValInt 100)
    
    , testCase "Transaction with Print and User Input" $ do
        (output, result) <- captureIO ["ValInt 999"] $ runEvalIO $ do
          transaction $ do
            evalPrint "Inside transaction"
            evalKvPut (ValInt 1) (ValInt 100)
          evalPrint "After transaction"
          evalKvGet (ValInt 2)
        output @?= ["Inside transaction", "After transaction", "Invalid key: ValInt 2. Enter a replacement: "]
        result @?= Right (ValInt 999)
    ]
  ]

-- goodPut = evalKvPut (ValInt 0) (ValInt 1)
-- badPut = evalKvPut (ValInt 0) (ValBool False) >> failure "die"
-- get0  = evalKvGet (ValInt 0)
-- test0 = runEvalIO $ (Free $ TryCatchOp (transaction badPut) (transaction (evalPrint "doing m2") ) ) >> get0
-- test1 = runEval $ (Free $ TryCatchOp (transaction badPut) (transaction (evalPrint "doing m2") ) ) >> get0
-- test2 = runEvalIO $ (Free $ TryCatchOp (badPut)  (evalPrint "doing m2") )  >> get0
-- test3 = runEval $ (Free $ TryCatchOp (badPut)  (evalPrint "doing m2") )  >> get0