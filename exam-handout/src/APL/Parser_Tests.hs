module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

parserTest :: String -> Exp -> TestTree
parserTest s e =
  testCase s $
    case parseAPL "input" s of
      Left err -> assertFailure err
      Right e' -> e' @?= e

parserTestFail :: String -> TestTree
parserTestFail s =
  testCase s $
    case parseAPL "input" s of
      Left _ -> pure ()
      Right e ->
        assertFailure $
          "Expected parse error but received this AST:\n" ++ show e

tests :: TestTree
tests =
  testGroup
    "Parsing"
    [ -- Example tests
      parserTest "x+y" $ Add (Var "x") (Var "y")
      , parserTestFail "x+"
      , parserTest "x * y + z" $ 
        Add (Mul (Var "x") (Var "y")) (Var "z")
    
    , parserTest "x + y * z" $ 
        Add (Var "x") (Mul (Var "y") (Var "z"))

    -- Tuple
    , parserTest "let x = (1,2) in x.0" $
        Let "x" 
          (Tuple [CstInt 1, CstInt 2])
          (Project (Var "x") 0)
    
    , parserTest "(1,2).1" $
        Project (Tuple [CstInt 1, CstInt 2]) 1

    , parserTest "(1, 2+3, (4,5).1, if true then 6 else 7)" $
        Tuple 
          [ CstInt 1
          , Add (CstInt 2) (CstInt 3)
          , Project (Tuple [CstInt 4, CstInt 5]) 1
          , If (CstBool True) (CstInt 6) (CstInt 7)
          ]

    , parserTest "((1,2), (3,4), (5,get 0)).2.1" $
        Project 
          (Project
            (Tuple 
              [ Tuple [CstInt 1, CstInt 2]
              , Tuple [CstInt 3, CstInt 4]
              , Tuple [CstInt 5, KvGet (CstInt 0)]
              ])
            2)
          1
  
    , parserTest "((a,b),(c,d)).0.1" $
        Project
          (Project
            (Tuple [Tuple [Var "a", Var "b"], Tuple [Var "c", Var "d"]])
            0)
          1

    , parserTest "let x = (1,2) in let y = (3,4) in (x.0 + y.0, x.1 * y.1)" $
        Let "x" 
          (Tuple [CstInt 1, CstInt 2])
          (Let "y" 
            (Tuple [CstInt 3, CstInt 4])
            (Tuple 
              [ Add (Project (Var "x") 0) (Project (Var "y") 0)
              , Mul (Project (Var "x") 1) (Project (Var "y") 1)
              ]))

    -- the production "(" Exp ")" is a parenthesized expression
    , parserTest "(1)" $ CstInt 1

    , parserTest "()" $ Tuple []

      -- Loop
    , parserTest "loop x = 1 for i < 5 do x * 2" $
        ForLoop 
          ("x", CstInt 1)
          ("i", CstInt 5)
          (Mul (Var "x") (CstInt 2))

    , parserTest "loop x = 1 while x == 1 do x" $
        WhileLoop
          ("x", CstInt 1)
          (Eql (Var "x") (CstInt 1))
          (Var "x")

    , parserTest "loop x = (1,10) while if x.1 == 0 then false else true do (x.0*2,x.1-1)" $
        WhileLoop
          ("x", Tuple [CstInt 1, CstInt 10])
          (If 
            (Eql (Project (Var "x") 1) (CstInt 0))
            (CstBool False)
            (CstBool True))
          (Tuple 
            [ Mul (Project (Var "x") 0) (CstInt 2)
            , Sub (Project (Var "x") 1) (CstInt 1)
            ])

    , parserTest "loop x = (0,1) while x.0 == 10 do (x.0+1, x.1*2)" $
        WhileLoop
          ("x", Tuple [CstInt 0, CstInt 1])
          (Eql (Project (Var "x") 0) (CstInt 10))
          (Tuple 
            [ Add (Project (Var "x") 0) (CstInt 1)
            , Mul (Project (Var "x") 1) (CstInt 2)
            ])

    , parserTest "loop p = (1,1) for i < let x = 5 in x+1 do (p.0*2, p.1+i)" $
        ForLoop
          ("p", Tuple [CstInt 1, CstInt 1])
          ("i", Let "x" (CstInt 5) (Add (Var "x") (CstInt 1)))
          (Tuple 
            [ Mul (Project (Var "p") 0) (CstInt 2)
            , Add (Project (Var "p") 1) (Var "i")
            ])

    , parserTest "loop x = 1 for i < 3 do loop y = x while y == 10 do y * 2" $
        ForLoop
          ("x", CstInt 1)
          ("i", CstInt 3)
          (WhileLoop
            ("y", Var "x")
            (Eql (Var "y") (CstInt 10))
            (Mul (Var "y") (CstInt 2)))

    -- && and ||
    , parserTest "(1+2) && (3+4)" $
        BothOf 
          (Add (CstInt 1) (CstInt 2))
          (Add (CstInt 3) (CstInt 4))

    , parserTest "(1+2) || (3+4+5+6)" $
        OneOf
          (Add (CstInt 1) (CstInt 2))
          (Add 
            (Add 
              (Add (CstInt 3) (CstInt 4))
              (CstInt 5))
            (CstInt 6))

    , parserTest "get 0 && put 0 true" $
        BothOf
          (KvGet (CstInt 0))
          (KvPut (CstInt 0) (CstBool True))

    , parserTest "get 0 + 1 && put 0 2" $
        BothOf
          (Add (KvGet (CstInt 0)) (CstInt 1))
          (KvPut (CstInt 0) (CstInt 2))
    
    , parserTest "a && b && c" $
        BothOf
          (BothOf (Var "a") (Var "b"))
          (Var "c")

    , parserTest "a || b || c" $
        OneOf
          (OneOf (Var "a") (Var "b"))
          (Var "c")

    , parserTest "put (get 0) 1 && let x = put 0 2 in get 2" $
        BothOf
          (KvPut (KvGet (CstInt 0)) (CstInt 1))
          (Let "x" 
            (KvPut (CstInt 0) (CstInt 2))
            (KvGet (CstInt 2)))
    
    , parserTest "(get 1 && put 1 2) || (get 2 && put 2 3)" $
        OneOf
          (BothOf 
            (KvGet (CstInt 1))
            (KvPut (CstInt 1) (CstInt 2)))
          (BothOf
            (KvGet (CstInt 2))
            (KvPut (CstInt 2) (CstInt 3)))

    , parserTest "let x = 1 && 2 in (x.0 + 3) && (x.1 * 4)" $
        Let "x"
          (BothOf (CstInt 1) (CstInt 2))
          (BothOf
            (Add (Project (Var "x") 0) (CstInt 3))
            (Mul (Project (Var "x") 1) (CstInt 4)))

    , parserTest "((1 && 2) || (3 && 4)).1 * 5" $
        Mul
          (Project
            (OneOf
              (BothOf (CstInt 1) (CstInt 2))
              (BothOf (CstInt 3) (CstInt 4)))
            1)
          (CstInt 5)
    
    -- Mix
    , parserTest "loop x = 1 && 2 while x.0 == 10 do (x.0 + 1) && (x.1 * 2)" $
        WhileLoop
          ("x", BothOf (CstInt 1) (CstInt 2))
          (Eql (Project (Var "x") 0) (CstInt 10))
          (BothOf
            (Add (Project (Var "x") 0) (CstInt 1))
            (Mul (Project (Var "x") 1) (CstInt 2)))

    , parserTest "(loop x = 1 for i < 3 do x * 2) && (loop y = 2 while y == 10 do y + 1)" $
        BothOf
          (ForLoop
            ("x", CstInt 1)
            ("i", CstInt 3)
            (Mul (Var "x") (CstInt 2)))
          (WhileLoop
            ("y", CstInt 2)
            (Eql (Var "y") (CstInt 10))
            (Add (Var "y") (CstInt 1)))

    , parserTest "let p = (1,2) in (get p.0 && put p.1 3) || (get p.1 && put p.0 4)" $
        Let "p"
          (Tuple [CstInt 1, CstInt 2])
          (OneOf
            (BothOf
              (KvGet (Project (Var "p") 0))
              (KvPut (Project (Var "p") 1) (CstInt 3)))
            (BothOf
              (KvGet (Project (Var "p") 1))
              (KvPut (Project (Var "p") 0) (CstInt 4))))

    , parserTest "1 + 2 * 3" $
        Add (CstInt 1) (Mul (CstInt 2) (CstInt 3))

    , parserTest "1 * 2 + 3" $
        Add (Mul (CstInt 1) (CstInt 2)) (CstInt 3)

    , parserTest "1 && 2 || 3" $
        OneOf 
          (BothOf (CstInt 1) (CstInt 2))
          (CstInt 3)

    , parserTest "(1 == 2) && (3 == 4)" $
        BothOf
          (Eql (CstInt 1) (CstInt 2))
          (Eql (CstInt 3) (CstInt 4))

    , parserTest "let x = if true then (1,2) else (3,4) in x.1" $
        Let "x"
          (If (CstBool True)
              (Tuple [CstInt 1, CstInt 2])
              (Tuple [CstInt 3, CstInt 4]))
          (Project (Var "x") 1)
    , parserTest "1 && 2 || 3 && 4" $
        OneOf
          (BothOf (CstInt 1) (CstInt 2))
          (BothOf (CstInt 3) (CstInt 4))

    , parserTest "(1,2,3).1 && (4,5,6).2 || (7,8,9).0" $
        OneOf
          (BothOf
            (Project (Tuple [CstInt 1, CstInt 2, CstInt 3]) 1)
            (Project (Tuple [CstInt 4, CstInt 5, CstInt 6]) 2))
          (Project (Tuple [CstInt 7, CstInt 8, CstInt 9]) 0)

    -- Error
    , parserTestFail "loop"  
    , parserTestFail "get"  
    , parserTestFail "x.2."  
    , parserTestFail "(,)"   
    , parserTestFail "if x"  
    ]
