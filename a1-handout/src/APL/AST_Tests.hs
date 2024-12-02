module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [
      testCase "Print Add" $
        printExp (Add (CstInt 2) (CstInt 3)) @?= "2 + 3",
      testCase "Print Sub" $
        printExp (Sub (CstInt 5) (CstInt 3)) @?= "5 - 3",
      testCase "Print Mul" $
        printExp (Mul (CstInt 2) (CstInt 3)) @?= "2 * 3",
      testCase "Print Div" $
        printExp (Div (CstInt 6) (CstInt 2)) @?= "6 / 2",
      testCase "Print Pow" $
        printExp (Pow (CstInt 2) (CstInt 3)) @?= "2 ** 3",
      testCase "Print Eql" $
        printExp (Eql (CstInt 2) (CstInt 3)) @?= "2 == 3",
      testCase "Print If" $
        printExp (If (CstBool True) (CstInt 2) (CstInt 3)) @?= "if (true) then 2 else 3",
      testCase "Print Let" $
        printExp (Let "x" (CstInt 2) (Add (Var "x") (CstInt 3))) @?= "let x = 2 in x + 3",
      testCase "Print Lambda" $
        printExp (Lambda "x" (Add (Var "x") (CstInt 1))) @?= "\\x -> x + 1",
      testCase "Print Apply" $
        printExp (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 2)) @?= "(\\x -> x + 1) 2",
      testCase "Print TryCatch" $
        printExp (TryCatch (Div (CstInt 2) (CstInt 0)) (CstInt 1)) @?= "try 2 / 0 catch 1"
    ]
