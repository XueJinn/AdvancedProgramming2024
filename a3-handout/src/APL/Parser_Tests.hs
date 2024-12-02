module APL.Parser_Tests (tests) where

import APL.AST (Exp (..))
import APL.Parser (parseAPL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import GHC.Conc (par)

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
    [ testGroup
        "Constants"
        [ parserTest "123" $ CstInt 123,
          parserTest " 123" $ CstInt 123,
          parserTest "123 " $ CstInt 123,
          parserTestFail "123f",
          parserTest "true" $ CstBool True,
          parserTest "false" $ CstBool False
        ],
      testGroup
        "Basic operators"
        [ parserTest "x+y" $ Add (Var "x") (Var "y"),
          parserTest "x-y" $ Sub (Var "x") (Var "y"),
          parserTest "x*y" $ Mul (Var "x") (Var "y"),
          parserTest "x/y" $ Div (Var "x") (Var "y"),
          parserTest "x**y" $ Pow (Var "x") (Var "y"),
          parserTest "x == y" $ Eql (Var "x") (Var "y")
        ],
      testGroup
        "Operator priority"
        [ parserTest "x+y+z" $ Add (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y-z" $ Sub (Add (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y*z" $ Add (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x*y*z" $ Mul (Mul (Var "x") (Var "y")) (Var "z"),
          parserTest "x/y/z" $ Div (Div (Var "x") (Var "y")) (Var "z"),
          parserTest "x+y**z" $ Add (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x**y**z" $ Pow (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x==y+z" $ Eql (Var "x") (Add (Var "y") (Var "z")),
          parserTest "x==y*z" $ Eql (Var "x") (Mul (Var "y") (Var "z")),
          parserTest "x==y/z" $ Eql (Var "x") (Div (Var "y") (Var "z")),
          parserTest "x==y**z" $ Eql (Var "x") (Pow (Var "y") (Var "z")),
          parserTest "x*y**z" $ Mul (Var "x") (Pow (Var "y") (Var "z")),

          parserTest "a+b*c-d/e**f" $ 
            Sub (Add (Var "a") (Mul (Var "b") (Var "c"))) (Div (Var "d") (Pow (Var "e") (Var "f"))),
          parserTest "(a+b)*(c-d)" $ 
            Mul (Add (Var "a") (Var "b")) (Sub (Var "c") (Var "d")),
          parserTest "a**b**c*d+e/f" $ 
            Add (Mul (Pow (Var "a") (Pow (Var "b") (Var "c"))) (Var "d")) (Div (Var "e") (Var "f")),

          parserTest "a==b+c*d-e/f**g" $ 
            Eql (Var "a") (Sub (Add (Var "b") (Mul (Var "c") (Var "d"))) (Div (Var "e") (Pow (Var "f") (Var "g")))),
          parserTest "a+b==c-d" $ 
            Eql (Add (Var "a") (Var "b")) (Sub (Var "c") (Var "d")),

          parserTest "(a+b)*c" $ Mul (Add (Var "a") (Var "b")) (Var "c"),
          parserTest "a*(b+c)" $ Mul (Var "a") (Add (Var "b") (Var "c")),
          parserTest "(a**b)**c" $ Pow (Pow (Var "a") (Var "b")) (Var "c"),
          parserTest "a**(b**c)" $ Pow (Var "a") (Pow (Var "b") (Var "c")),

          parserTest "a==b==c" $ Eql (Eql (Var "a") (Var "b")) (Var "c"),

          parserTest "f a + g b * h c" $ 
            Add (Apply (Var "f") (Var "a")) (Mul (Apply (Var "g") (Var "b")) (Apply (Var "h") (Var "c"))),
          parserTest "f (a + b) * c" $ 
            Mul (Apply (Var "f") (Add (Var "a") (Var "b"))) (Var "c")
        ],
      testGroup
        "Conditional expressions"
        [ parserTest "if x then y else z" $ If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z"),
          parserTest "if x then (if x then y else z) else z" $
            If (Var "x") (If (Var "x") (Var "y") (Var "z")) (Var "z"),
          parserTest "1 + if x then y else z" $
            Add (CstInt 1) (If (Var "x") (Var "y") (Var "z")),
          parserTest "if x then y else if x then y else z" $
            If (Var "x") (Var "y") $
              If (Var "x") (Var "y") (Var "z")
        ],
      testGroup
        "Lexing edge cases"
        [ parserTest "2 " $ CstInt 2,
          parserTest " 2" $ CstInt 2
        ],
      testGroup
        "Function application"
        [ parserTest "f x" $ Apply (Var "f") (Var "x"),
          parserTest "f x y" $ Apply (Apply (Var "f") (Var "x")) (Var "y"),
          parserTest "f (x y)" $ Apply (Var "f") (Apply (Var "x") (Var "y")),
          parserTest "f x y z" $ Apply (Apply (Apply (Var "f") (Var "x")) (Var "y")) (Var "z"),
          parserTest "f (x y) z" $ Apply (Apply (Var "f") (Apply (Var "x") (Var "y"))) (Var "z"),
          parserTest "f (x (y z))" $ Apply (Var "f") (Apply (Var "x") (Apply (Var "y") (Var "z"))),
          parserTest "f (g x) (h y)" $ Apply (Apply (Var "f") (Apply (Var "g") (Var "x"))) (Apply (Var "h") (Var "y")),
          parserTest "(f x y) (g z)" $ Apply (Apply (Apply (Var "f") (Var "x")) (Var "y")) (Apply (Var "g") (Var "z")),
          parserTest "f (x + y) (z * w)" $ Apply (Apply (Var "f") (Add (Var "x") (Var "y"))) (Mul (Var "z") (Var "w")),
          parserTest "f (if x then y else z) w" $ Apply (Apply (Var "f") (If (Var "x") (Var "y") (Var "z"))) (Var "w"),
          parserTest "f x + g y" $ Add (Apply (Var "f") (Var "x")) (Apply (Var "g") (Var "y")),
          parserTest "f (x + y) * g z" $ Mul (Apply (Var "f") (Add (Var "x") (Var "y"))) (Apply (Var "g") (Var "z")),
          parserTest "(f x) y" $ Apply (Apply (Var "f") (Var "x")) (Var "y"),
          parserTest "f (g (h x))" $ Apply (Var "f") (Apply (Var "g") (Apply (Var "h") (Var "x"))),
          parserTestFail "f x y z)",
          parserTestFail "f (x y",
          parserTestFail "f x y +",
          parserTestFail "x if x then y else z"
        ],
      testGroup
        "Printing, putting, and getting"
        [ parserTest "put x y" $ KvPut (Var "x") (Var "y"),
          parserTest "get x + y" $ Add (KvGet (Var "x")) (Var "y"),
          parserTest "getx" $ Var "getx",
          parserTest "put (x + y) (z * 2)" $ KvPut (Add (Var "x") (Var "y")) (Mul (Var "z") (CstInt 2)),
          parserTest "put (get x) (y + 1)" $ KvPut (KvGet (Var "x")) (Add (Var "y") (CstInt 1)),
          parserTest "put x (if y then 1 else 0)" $ KvPut (Var "x") (If (Var "y") (CstInt 1) (CstInt 0)),
          parserTest "get (x + y) * z" $ Mul (KvGet (Add (Var "x") (Var "y"))) (Var "z"),
          parserTest "get (if x then y else z)" $ KvGet (If (Var "x") (Var "y") (Var "z")),
          parserTest "(get x) (get y)" $ Apply (KvGet (Var "x")) (KvGet (Var "y")),
          parserTest "put (get x) (get (y + 1))" $ KvPut (KvGet (Var "x")) (KvGet (Add (Var "y") (CstInt 1))),
          parserTest "get (put x y)" $ KvGet (KvPut (Var "x") (Var "y")),
          
          parserTest "print \"result\" (get (x + y) * 2)" $ Print "result" (Mul (KvGet (Add (Var "x") (Var "y"))) (CstInt 2)),
          parserTest "print \"foo\" x" $ Print "foo" (Var "x"),
          parserTest "print \"hello\" 42" $ Print "hello" (CstInt 42),
          parserTest "print \"result\" (x + y)" $ Print "result" (Add (Var "x") (Var "y")),
          parserTest "print \"calc\" (f x y + z)" $ Print "calc" (Add (Apply (Apply (Var "f") (Var "x")) (Var "y")) (Var "z")),
          parserTest "print \"nested\" (if x then y else z)" $ Print "nested" (If (Var "x") (Var "y") (Var "z")),
          parserTest "print \"getx\" (get x)" $ Print "getx" (KvGet (Var "x")),
          parserTest "print \"putx\" (put x y)" $ Print "putx" (KvPut (Var "x") (Var "y")),
          parserTestFail "print x",
          parserTestFail "print \"unclosed", 
          parserTestFail "print \"unclosed",
          parserTestFail "print \"extra\" x y", 
          parserTestFail "print (\"dynamic\" + \"string\") x",
          parserTest "print \"complex\" (let x = 5 in x + y)" $ 
            Print "complex" (Let "x" (CstInt 5) (Add (Var "x") (Var "y"))),
          parserTest "print \"lambda\" (\\x -> x + 1)" $ 
            Print "lambda" (Lambda "x" (Add (Var "x") (CstInt 1)))
        ],
      testGroup
      "Lambdas, let-binding and try-catch"
      [
        parserTest "\\x -> x" $ Lambda "x" (Var "x"),
        parserTest "\\x -> x + 1" $ Lambda "x" (Add (Var "x") (CstInt 1)),
        parserTest "\\x -> \\y -> x + y" $ Lambda "x" (Lambda "y" (Add (Var "x") (Var "y"))),
        parserTest "(\\x -> x + 1) 5" $ Apply (Lambda "x" (Add (Var "x") (CstInt 1))) (CstInt 5),

        parserTest "let x = 5 in x" $ Let "x" (CstInt 5) (Var "x"),
        parserTest "let x = 5 in x + y" $ Let "x" (CstInt 5) (Add (Var "x") (Var "y")),
        parserTest "let x = 5 in let y = 6 in x + y" $ Let "x" (CstInt 5) (Let "y" (CstInt 6) (Add (Var "x") (Var "y"))),
        parserTest "let f = \\x -> x + 1 in f 5" $ Let "f" (Lambda "x" (Add (Var "x") (CstInt 1))) (Apply (Var "f") (CstInt 5)),

        parserTest "try x catch y" $ TryCatch (Var "x") (Var "y"),
        parserTest "try (x + 1) catch (y - 1)" $ TryCatch (Add (Var "x") (CstInt 1)) (Sub (Var "y") (CstInt 1)),
        parserTest "try (try x catch y) catch z" $ TryCatch (TryCatch (Var "x") (Var "y")) (Var "z"),

        parserTest "let x = try 5 catch 0 in \\y -> x + y" $ 
          Let "x" (TryCatch (CstInt 5) (CstInt 0)) (Lambda "y" (Add (Var "x") (Var "y"))),
        parserTest "\\x -> let y = x + 1 in try y catch x" $ 
          Lambda "x" (Let "y" (Add (Var "x") (CstInt 1)) (TryCatch (Var "y") (Var "x"))),
        parserTest "try (let x = 5 in x + 1) catch (\\y -> y)" $ 
          TryCatch (Let "x" (CstInt 5) (Add (Var "x") (CstInt 1))) (Lambda "y" (Var "y")),

        parserTest "let f = \\x -> x + 1 in try (f 5) catch 0" $ 
          Let "f" (Lambda "x" (Add (Var "x") (CstInt 1))) (TryCatch (Apply (Var "f") (CstInt 5)) (CstInt 0)),
        parserTest "\\x -> try (if x then 1 else 0) catch 1" $ 
          Lambda "x" (TryCatch (If (Var "x") (CstInt 1) (CstInt 0)) (CstInt 1)),

        parserTestFail "\\x",
        parserTestFail "let x = 5",
        parserTestFail "try x",
        parserTestFail "let let x = 5 in x",
        parserTestFail "\\let -> x",

        parserTest "let x = let y = 5 in y in x" $ 
          Let "x" (Let "y" (CstInt 5) (Var "y")) (Var "x"),
        parserTest "try (try (try x catch y) catch z) catch w" $ 
          TryCatch (TryCatch (TryCatch (Var "x") (Var "y")) (Var "z")) (Var "w"),
        parserTest "\\x -> \\y -> \\z -> x + y + z" $ 
          Lambda "x" (Lambda "y" (Lambda "z" (Add (Add (Var "x") (Var "y")) (Var "z"))))
      ]
    ]
