module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
-- yComb :: Exp
-- yComb =
--   Lambda "f" $
--     Apply
--       (Lambda "g" (Apply (Var "g") (Var "g")))
--       ( Lambda
--           "g"
--           ( Apply
--               (Var "f")
--               (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
--           )
--       )

-- fact :: Exp
-- fact =
--   Apply yComb $
--     Lambda "rec" $
--       Lambda "n" $
--         If
--           (Eql (Var "n") (CstInt 0))
--           (CstInt 1)
--           (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
          --
          -- TODO - add more

  

      testCase "Apply" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 2)) @?= Right (ValInt 3),

      testCase "Apply (non-function)" $
        eval envEmpty (Apply (CstInt 2) (CstInt 3)) @?= Left "Non-function applied.",

      testCase "Apply (wrong type)" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstBool True)) @?= Left "Non-integer operand",

      testCase "Apply (apply)" $
        eval envEmpty (Apply (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) (CstInt 3)) @?= Right (ValInt 5),

      testCase "Apply (wrong order)" $
        eval envEmpty (Apply (CstInt 2) (Lambda "x" (Add (Var "x") (CstInt 1)))) @?= Left "Non-function applied.",

      testCase "Lambda" $
        eval envEmpty (Lambda "x" (Add (Var "x") (CstInt 1))) @?= Right (ValFun envEmpty "x" (Add (Var "x") (CstInt 1))),

      testCase "Lambda (apply)" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 2)) @?= Right (ValInt 3),

      testCase "Lambda (apply wrong type)" $
        eval envEmpty (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstBool True)) @?= Left "Non-integer operand",

      testCase "Lambda (Let)" $
        eval envEmpty (Let "x" (CstInt 2) (Lambda "y" (Add (Var "x") (Var "y")))) @?= Right (ValFun [("x", ValInt 2)] "y" (Add (Var "x") (Var "y"))),      

      testCase "Lambda (Let apply)" $
        eval envEmpty (Let "x" (CstInt 2) (Apply (Lambda "y" (Add (Var "x") (Var "y"))) (CstInt 3))) @?= Right (ValInt 5),

      testCase "TryCatch" $
        eval envEmpty (TryCatch (Div (CstInt 2) (CstInt 0)) (CstInt 1)) @?= Right (ValInt 1),

      testCase "TryCatch (no exception)" $
        eval envEmpty (TryCatch (Add (CstInt 2) (CstInt 3)) (CstInt 1)) @?= Right (ValInt 5),

      testCase "TryCatch (exception)" $
        eval envEmpty (TryCatch (Div (CstInt 2) (CstInt 0)) (CstInt 1)) @?= Right (ValInt 1),

      testCase "TryCatch (exception)" $
        eval envEmpty (TryCatch (CstInt 0) (CstInt 1)) @?= Right (ValInt 0),

      testCase "TryCatch (exception)" $
        eval envEmpty (TryCatch (Var "missing") (CstInt 1)) @?= Right (ValInt 1)

     -- testCase "infinite recursion" $--
      -- eval envEmpty (Apply (Lambda "x" (Apply (Var "x") (Var "x"))) (Lambda "x" (Apply (Var "x") (Var "x")))) @?= Left "Maximum call stack size exceeded" --


      
        
    ]
