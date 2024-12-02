module APL.InterpSim_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpSim (runEval)
import APL.Monad
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

evalTest :: String -> Exp -> Val -> TestTree
evalTest desc e v = testCase desc $ do
  runEval (eval e) @?= Right v

evalTestFail :: String -> Exp -> TestTree
evalTestFail desc e =
  testCase desc $
    case runEval (eval e) of
      Left _ -> pure ()
      Right v ->
        assertFailure $
          "Expected error but received this value:\n" ++ show v

tests :: TestTree
tests =
  testGroup
    "Simulated concurrent interpreter"
    [
      evalTest
        "State (unknown key)"
        (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstInt 1)))
        (ValTuple [ValInt 1, ValInt 1]),

      -- Tuple tests
      -- Should work after task A.
      evalTest
        "(e1,e2)"
        (Tuple [CstInt 1, CstInt 2])
        (ValTuple [ValInt 1, ValInt 2])
      --
    , evalTest
        "Tuple projection"
        (Project (Tuple [CstInt 1, CstInt 2]) 1)
        (ValInt 2)

    , evalTest
        "Let with tuple"
        (Let "x" 
          (Tuple [CstInt 1, CstInt 2])
          (Project (Var "x") 0))
        (ValInt 1)

    , evalTest
        "Nested tuples"
        (Tuple 
          [ Tuple [CstInt 1, CstInt 2]
          , Tuple [CstInt 3, CstInt 4]
          ])
        (ValTuple 
          [ ValTuple [ValInt 1, ValInt 2]
          , ValTuple [ValInt 3, ValInt 4]
          ])
    
    -- -- Test evaluation order with key-value dependencies
    , evalTest "Tuple evaluation order with key-value dependencies" 
        (Tuple 
          [ KvPut (CstInt 1) (CstInt 10)
          , Let "x"
              (KvGet (CstInt 1))
              (KvPut (CstInt 2) (Var "x"))
          , KvGet (CstInt 2)
          ])
        (ValTuple [ValInt 10, ValInt 10, ValInt 10])
        
    , evalTest "Tuple evaluation order with key-value dependencies and updates"
        (Tuple
          [ KvPut (CstInt 1) (CstInt 100)
          , Let "x"
              (KvGet (CstInt 1))
              (KvPut (CstInt 1) (Add (Var "x") (CstInt 1)))
          , KvGet (CstInt 1)
          ])
        (ValTuple [ValInt 100, ValInt 101, ValInt 101])
      
    -- Loop tests
    -- Should work after Task B.
    ,  evalTest
        "For loop"
        (ForLoop ("x", CstInt 1) ("i", CstInt 10) (Mul (Var "x") (CstInt 2)))
        (ValInt 1024)
    
    , evalTest
        "For loop 2"
        (ForLoop ("x", CstInt 1) ("i", CstInt 5) (Mul (Var "x") (CstInt 2)))
        (ValInt 32)
    
    , evalTest
        "Complex for loop"
        (ForLoop 
          ("x", Tuple [CstInt 1, CstInt 10])
          ("i", CstInt 3)
          (Tuple 
            [ Mul (Project (Var "x") 0) (CstInt 2)
            , Sub (Project (Var "x") 1) (CstInt 1)
            ]))
        (ValTuple [ValInt 8, ValInt 7])

    , evalTest
        "While loop with complex condition"
        (WhileLoop
          ("x", Tuple [CstInt 1, CstInt 10])
          (If 
            (Eql (Project (Var "x") 1) (CstInt 0))
            (CstBool False)
            (CstBool True))
          (Tuple 
            [ Mul (Project (Var "x") 0) (CstInt 2)
            , Sub (Project (Var "x") 1) (CstInt 1)
            ]))
        (ValTuple [ValInt 1024, ValInt 0])

    -- && and || tests
    -- Should work after task C.
    , evalTest
        "e1 && e2"
        (BothOf (CstInt 0) (CstInt 1))
        (ValTuple [ValInt 0, ValInt 1])
      
      -- Should work after task C.
    , evalTest
        "e1 || e2"
        (OneOf (CstInt 0) (CstInt 1))
        (ValInt 0)
      --
      -- Should work after task C.
    , evalTest
        "e1 || e2 (first fails)"
        (OneOf (KvGet (CstInt 0)) (CstInt 1))
        (ValInt 1)

    , evalTest
        "&& with Add"
        (BothOf 
          (Add (CstInt 1) (CstInt 2))
          (Add (CstInt 3) (CstInt 4)))
        (ValTuple [ValInt 3, ValInt 7])

    , evalTestFail
        "&& with first failing"
        (BothOf 
          (Div (CstInt 1) (CstInt 0))
          (CstInt 1))
    
    , evalTest
        "|| with complex expressions"
        (OneOf
          (Add (CstInt 1) (CstInt 2))
          (Add (Add (Add (CstInt 3) (CstInt 4)) (CstInt 5)) (CstInt 6)))
        (ValInt 3)

      -- Mixed
    , evalTest
        "Complex nested expression"
        (Let "x"
          (Tuple [CstInt 1, CstInt 2])
          (OneOf
            (Add (Project (Var "x") 0) (CstInt 1))
            (Mul (Project (Var "x") 1) (CstInt 2))))
        (ValInt 2)

  , evalTest
        "Loop with &&"
        (ForLoop
          ("x", CstInt 1)
          ("i", CstInt 3)
          (BothOf
            (CstInt 1)
            (Var "i")))
        (ValTuple [ValInt 1, ValInt 2]) 

    , evalTest
        "Nested || and &&"
        (OneOf
          (BothOf
            (CstInt 1)
            (CstInt 2))
          (BothOf
            (CstInt 3)
            (CstInt 4)))
        (ValTuple [ValInt 1, ValInt 2])

    , evalTest
        "Concurrent put/get"
        (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 0) (CstBool True)))
        (ValTuple [ValBool True, ValBool True])

    , evalTest
        "Concurrent put/get 2"
        (BothOf (Add (KvGet (CstInt 0)) (CstInt 1)) (KvPut (CstInt 0) (CstInt 2)))
        (ValTuple [ValInt 3, ValInt 2])

    , evalTest
        "Concurrent put/get 3"
        (BothOf (KvPut (KvGet (CstInt 0)) (CstInt 1)) (Let "x" (KvPut (CstInt 0) (CstInt 2)) (KvGet (CstInt 2))))
        (ValTuple [ValInt 1, ValInt 1])

    , evalTest
        "Concurrent loops: one infinite"
        (OneOf 
          (WhileLoop ("x",CstInt 1) (Eql (Var "x") (CstInt 1)) (Var "x")) 
          (ForLoop
            ("x", CstInt 1)
            ("i", CstInt 3)
            (BothOf
              (CstInt 1)
              (Var "i"))))
        (ValTuple [ValInt 1, ValInt 2]) 
    
    , evalTest
        "BothOf with different step counts"
        (BothOf
          (ForLoop
            ("x", CstInt 1)
            ("i", CstInt 3)
            (Add (Var "x") (CstInt 1)))
          (ForLoop
            ("y", CstInt 1)
            ("i", CstInt 2)
            (Let "temp" 
              (Mul (Var "y") (CstInt 2))
              (Add (Var "temp") (CstInt 1)))))  -- i=0, y=3 (2*1+1), i=1, y=7 (2*3+1)
        (ValTuple [ValInt 4, ValInt 7])
    
    , evalTest
        "OneOf selects faster loop"
        (OneOf
          (ForLoop  -- 5 steps
            ("x", CstInt 1)
            ("i", CstInt 5)
            (Add (Var "x") (CstInt 1)))
          (ForLoop  -- 3 steps
            ("y", CstInt 1)
            ("i", CstInt 3)
            (Mul (Var "y") (CstInt 2))))
        (ValInt 8)  -- The second loop finishes first

    , evalTest
        "Nested concurrent operations"
        (BothOf
          (OneOf
            (WhileLoop  -- Infinite loop
              ("x", CstInt 1)
              (CstBool True)
              (Add (Var "x") (CstInt 1)))
            (CstInt 42))  -- Returns immediately
          (ForLoop  -- Finite loop
            ("y", CstInt 1)
            ("i", CstInt 3)
            (Mul (Var "y") (CstInt 2))))
        (ValTuple [ValInt 42, ValInt 8])

    , evalTest
        "Multiple nested concurrent operations"
        (OneOf
          (BothOf
            (WhileLoop  -- Infinite loop 1
              ("x", CstInt 1)
              (CstBool True)
              (Add (Var "x") (CstInt 1)))
            (WhileLoop  -- Infinite loop 2
              ("y", CstInt 2)
              (CstBool True)
              (Mul (Var "y") (CstInt 2))))
          (ForLoop    -- Finite loop
            ("z", CstInt 1)
            ("i", CstInt 4)
            (Let "temp"
              (Add (Var "z") (CstInt 1))
              (Mul (Var "temp") (CstInt 2))))) 
        (ValInt 46)

    , evalTest 
        "Dead lock 2"
        (BothOf
          (BothOf (KvGet (CstInt 0)) (KvPut (CstInt 1) (CstInt 2)))
          (BothOf (KvGet (CstInt 1)) (KvPut (CstInt 0) (CstInt 1))))
        (ValTuple [ValTuple [ValInt 1, ValInt 2], ValTuple [ValInt 2, ValInt 1]])

    , evalTest
        "Function application"
        (Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 2))
        (ValInt 3)
        
    , evalTestFail
        "Project from non-tuple"
        (Project (CstInt 1) 0)

    , evalTestFail
        "Project with invalid index"
        (Project (Tuple [CstInt 1, CstInt 2]) 2)

    , evalTestFail
        "Tuple with only one element"
        (Tuple [CstInt 1])

    , evalTestFail
        "Project with illegal tuple"
        (Project (Tuple [CstInt 1]) 0)
    ]
