module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Error, Val (..), eval, runEval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalTests :: TestTree
evalTests =
  testGroup
    "EValuation"
    [ testCase "Add" $
        eval' (Add (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt 7)),
      --
      testCase "Add (wrong type)" $
        eval' (Add (CstInt 2) (CstBool True))
          @?= ([], Left "Non-integer operand"),
      --
      testCase "Sub" $
        eval' (Sub (CstInt 2) (CstInt 5))
          @?= ([], Right (ValInt (-3))),
      --
      testCase "Div" $
        eval' (Div (CstInt 7) (CstInt 3))
          @?= ([], Right (ValInt 2)),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero"),
      --
      testCase "Pow" $
        eval' (Pow (CstInt 2) (CstInt 3))
          @?= ([], Right (ValInt 8)),
      --
      testCase "Pow0" $
        eval' (Pow (CstInt 2) (CstInt 0))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Pow negative" $
        eval' (Pow (CstInt 2) (CstInt (-1)))
          @?= ([], Left "Negative exponent"),
      --
      testCase "Eql (false)" $
        eval' (Eql (CstInt 2) (CstInt 3))
          @?= ([], Right (ValBool False)),
      --
      testCase "Eql (true)" $
        eval' (Eql (CstInt 2) (CstInt 2))
          @?= ([], Right (ValBool True)),
      --
      testCase "If" $
        eval' (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= ([], Right (ValInt 2)),
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
      testCase "Lambda/Apply" $
        eval'
          (Apply (Lambda "x" (Mul (Var "x") (Var "x"))) (CstInt 4))
          @?= ([], Right (ValInt 16)),
      --
      testCase "TryCatch" $
        eval'
          (TryCatch (Div (CstInt 7) (CstInt 0)) (CstBool True))
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        eval' (Print "foo" $ CstInt 7) 
          @?= (["foo: 7"], Right (ValInt 7)),
      --
      testCase "Print" $
        eval' (Let "x" (Print "foo" $ CstInt 2) (Print "bar" $ CstInt 3))
          @?= (["foo: 2", "bar: 3"], Right (ValInt 3)),
      --
      testCase "Print (error)" $
        eval' (Let "x" (Print "foo" $ CstInt 2) (Var "bar"))
          @?= (["foo: 2"], Left "Unknown variable: bar"),
      --
      testCase "Print (func)" $
        eval' (Print "foo" (Lambda "x" (Var "x")))
          @?= (["foo: #<fun>"], Right (ValFun [] "x" (Var "x"))),
      --
      testCase "Print (bool)" $
        eval' (Print "foo" (CstBool True))
          @?= (["foo: True"], Right (ValBool True)),
      --
      testCase "KvPut" $
        eval' (KvPut (CstInt 1) (CstInt 2))
          @?= ([], Right (ValInt 2)),
      --
      testCase "KvPut & KvGet" $
        eval' (KvGet (CstInt 1))
          @?= ([], Left "Invalid key: ValInt 1"),
      --
      testCase "KvPut & KvGet" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (KvGet (CstInt 0)))
          @?= ([], Right (ValBool True)),
      --
      testCase "KvPut & KvGet (wrong key)" $
        eval' (Let "x" (KvPut (CstInt 1) (CstBool True)) (KvGet (CstInt 0)))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "KvPut & KvGet" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (Let "y" (KvPut (CstInt 0) (CstBool False)) (KvGet (CstInt 0))))
          @?= ([], Right (ValBool False)),
      --
      testCase "KvPut & KvGet (wrong key)" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (Let "y" (KvPut (CstInt 0) (CstBool False)) (KvGet (CstInt 1))))
          @?= ([], Left "Invalid key: ValInt 1"),
      --
      testCase "KvPut & KvGet" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (Let "y" (KvPut (CstInt 1) (CstInt 2)) (KvGet (CstInt 1))))
          @?= ([], Right (ValInt 2)),
      --
      testCase "KvPut & KvGet & Print" $
        eval' (Let "x" (KvPut (CstInt 0) (CstBool True)) (Let "y" (KvPut (CstInt 1) (CstInt 2)) (Print "foo" (KvGet (CstInt 0)))))
          @?= (["foo: True"], Right (ValBool True)),
      --
      testCase "KvPut & KvGet same value" $
        eval' (
          Let "result1" (KvPut (CstInt 1) (CstInt 100))
                   (Let "result2" (KvPut (CstInt 1) (CstInt 200))
                   (Let "get1" (KvGet (CstInt 1))
                   (KvGet (CstInt 1))))
        ) @?= ([], Right (ValInt 200))
        ,
      --
      testCase "TryCatch - No error, with side effect" $
        eval' (TryCatch (Let "x" (Print "Setting x" (CstInt 5)) (Var "x")) (CstInt 0))
          @?= (["Setting x: 5"], Right (ValInt 5)),
      --
      testCase "TryCatch - e1 Error, side effect not visible to e2" $
        eval' (TryCatch (Let "x" (Print "Setting x" (CstInt 5)) (Div (CstInt 1) (CstInt 0))) (CstInt 0))
          @?= ([], Right (ValInt 0)),
      --
      testCase "TryCatch - e1 and e2 Error, e1's side effect not visible to e2" $
        eval' (TryCatch 
                (Let "x" (KvPut (CstInt 1) (CstInt 5)) (Div (CstInt 1) (CstInt 0)))
                (KvGet (CstInt 1)))
          @?= ([], Left "Invalid key: ValInt 1"),
      --
      testCase "TryCatch - Nested, inner catch triggered, side effect not visible to e2" $
        eval' (TryCatch 
                (TryCatch 
                  (Let "x" (Print "Inner" (Div (CstInt 1) (CstInt 0))) (CstInt 1))
                  (Print "Caught inner" (CstInt 2)))
                (CstInt 3))
          @?= (["Caught inner: 2"], Right (ValInt 2)),
      --
      testCase "TryCatch - Nested, outer catch triggered" $
        eval' (TryCatch 
                (TryCatch 
                  (Let "x" (Print "Inner" (CstInt 1)) (Div (CstInt 1) (CstInt 0)))
                  (Print "Caught inner" (Div (CstInt 1) (CstInt 0))))
                (Print "Caught outer" (CstInt 3)))
          @?= (["Caught outer: 3"], Right (ValInt 3)),
      --
      testCase "TryCatch - Side effect in e2" $
        eval' (TryCatch 
                (Div (CstInt 1) (CstInt 0))
                (Print "Caught" (KvPut (CstInt 1) (CstInt 5))))
          @?= (["Caught: 5"], Right (ValInt 5)),
      --
      testCase "TryCatch - Side effect in e2, then using it" $
        eval' (Let "result" 
                (TryCatch 
                  (Div (CstInt 1) (CstInt 0))
                  (Print "Caught" (KvPut (CstInt 1) (CstInt 5))))
                (KvGet (CstInt 1)))
          @?= (["Caught: 5"], Right (ValInt 5))

      
    ]

tests :: TestTree
tests = testGroup "APL" [evalTests]
