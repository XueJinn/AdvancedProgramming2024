module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [
      testPos (CstInt 1),
      testNeg (Var "x"),
      testPos (Let "x" (CstInt 1) (Var "x")),
      testNeg (Let "x" (Var "x") (Var "x")),
      testNeg (Let "x" (Var "x") (CstInt 0)),
      testPos (Let "x" (CstInt 0) (Var "x")),
      testPos (Let "x" (CstInt 0) (CstInt 1)),
      testNeg (Add (Var "x") (Var "y")),
      testPos (Add (CstInt 1) (CstInt 2)),
      testPos (Add (CstInt 1) (CstBool True)),
      testNeg (Let "x" (Div (CstInt 1) (CstInt 0)) (Var "x"))
    ]
