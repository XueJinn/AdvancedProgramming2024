module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), subExp, printExp)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Parser (parseAPL)
import APL.Eval (runEval, eval)
import APL.Error (Error)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , quickCheck
  , withMaxSuccess
  , property
  , cover
  , checkCoverage
  , sized
  , frequency
  , elements
  )

type VName = String

keywords :: [String]
keywords =
  [ "if",
    "then",
    "else",
    "true",
    "false",
    "let",
    "in",
    "try",
    "catch"
  ]

instance Arbitrary Exp where
  arbitrary = sized (\n -> genExp n [])
  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

genExp :: Int -> [VName] -> Gen Exp
genExp 0 _ = frequency
  [ (3, CstInt . abs <$> arbitrary)
  , (1, CstBool <$> arbitrary)
  ]
genExp size vars =
  frequency
    [ 
      (5, CstInt . abs <$> arbitrary)
    , (2, CstBool <$> arbitrary)
    , (1, Add <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, Sub <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, Mul <$> genExp halfSize vars <*> genExp halfSize vars)
    , (2, Div <$> genExp halfSize vars <*> genExp halfSize vars)
    , (2, Pow <$> genExp halfSize vars <*> genExp halfSize vars)
    , (1, Eql <$> genExp halfSize vars <*> genExp halfSize vars)
    , (10, If <$> genExp thirdSize vars <*> genExp thirdSize vars <*> genExp thirdSize vars)
    , (5, genVar vars)
    , (22, do 
        newVar <- genNewVar
        mayknownVar <- elements $ newVar:vars
        Let mayknownVar <$> genExp halfSize vars <*> genExp halfSize (mayknownVar : vars))
    , (22, do
        newVar <- genNewVar
        mayknownVar <- elements $ newVar:vars
        Lambda mayknownVar <$> genExp (size - 1) (mayknownVar : vars))
    , (10, Apply <$> genExp halfSize vars <*> genExp halfSize vars)
    , (5, TryCatch <$> genExp halfSize vars <*> genExp halfSize vars)
    ]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

    genVar :: [VName] -> Gen Exp
    genVar [] = Var <$> genNewVar
    genVar vs = Var <$> elements vs

    genNewVar :: Gen VName
    genNewVar = frequency
      [ (1, genVarOfLength 2)
      , (2, genVarOfLength 3)
      , (1, genVarOfLength 4)
      ]
      where
        genVarOfLength :: Int -> Gen VName
        genVarOfLength 2 = do
          c1 <- elements (['a'..'z'] ++ ['A'..'Z'])
          c2 <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
          if [c1, c2] `elem` keywords
            then return [c2, c1]
            else return [c1, c2]
        genVarOfLength 3 = do
          c1 <- elements (['a'..'z'] ++ ['A'..'Z'])
          c2 <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
          c3 <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
          if [c1, c2, c3] `elem` keywords
            then return [c3, c2, c1]
            else return [c1, c2, c3]
        genVarOfLength 4 = do
          c1 <- elements (['a'..'z'] ++ ['A'..'Z'])
          c2 <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
          c3 <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
          c4 <- elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
          if [c1, c2, c3, c4] `elem` keywords
            then return [c4, c3, c2, c1]
            else return [c1, c2, c3, c4]
        genVarOfLength _ = error "unsupported variable length"

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e = parseAPL "" (printExp e) == Right e

onlyCheckedErrors :: Exp -> Bool
onlyCheckedErrors e =
  let errors = checkExp e
  in let evalResult = runEval (eval e)
  in case evalResult of 
    Right _ -> True 
    Left err -> lookupErr err errors 
    where
      lookupErr :: Error -> [Error] -> Bool
      lookupErr err errors = case errors of
        [] -> False
        (x:xs) -> if x == err then True else lookupErr err xs

properties :: [(String, Property)]
properties =
  [ ("expCoverage", property expCoverage)
  , ("onlyCheckedErrors", property onlyCheckedErrors)
  , ("parsePrinted", property parsePrinted)
  ]
