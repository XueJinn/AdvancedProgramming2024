module APL.Parser (parseAPL) where

import APL.AST (Exp (..), VName)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    many,
    notFollowedBy,
    parse,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec.Char (space)

type Parser = Parsec Void String

lexeme :: Parser a -> Parser a
lexeme p = p <* space

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
    "catch",
    "print",
    "put",
    "get"
  ]

lVName :: Parser VName
lVName = lexeme $ try $ do
  c <- satisfy isAlpha
  cs <- many $ satisfy isAlphaNum
  let v = c : cs
  if v `elem` keywords
    then fail "Unexpected keyword"
    else pure v

lInteger :: Parser Integer
lInteger =
  lexeme $ read <$> some (satisfy isDigit) <* notFollowedBy (satisfy isAlphaNum)

lString :: String -> Parser ()
lString s = lexeme $ void $ chunk s

lKeyword :: String -> Parser ()
lKeyword s = lexeme $ void $ try $ chunk s <* notFollowedBy (satisfy isAlphaNum)

lBool :: Parser Bool
lBool =
  lexeme . try . choice $
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]



pAtom :: Parser Exp
pAtom =
  choice
    [ CstInt <$> lInteger,
      CstBool <$> lBool,
      Var <$> lVName,
      lString "(" *> pExp <* lString ")"
    ]

pFExp :: Parser Exp
pFExp = pAtom >>= pFExp'
  where
    pFExp' x1 = choice
      [ do
          -- lString " "
          x2 <- pAtom
          pFExp' $ Apply x1 x2,
        pure x1
      ]

pLExp :: Parser Exp
pLExp = choice
  [ If <$> (lKeyword "if" *> pExp)
      <*> (lKeyword "then" *> pExp)
      <*> (lKeyword "else" *> pExp),
    do 
      lString "\\"
      x <- lVName
      lString "->"
      y <- pExp
      pure $ Lambda x y,
    do
      lKeyword "try"
      x <- pExp
      lKeyword "catch"
      y <- pExp
      pure $ TryCatch x y,
    do
      lKeyword "let"
      x <- lVName
      lString "="
      y <- pExp
      lKeyword "in"
      z <- pExp
      pure $ Let x y z,
    pFExp      
  ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= pExp1'
  where
    pExp1' x1 = choice
      [ do
          lString "=="
          x2 <- pExp2
          pExp1' $ Eql x1 x2,
        pure x1
      ]

pExp2 :: Parser Exp
pExp2 = pExp3 >>= pExp2'
  where
    pExp2' x1 = choice
      [ do
          lString "+"
          x2 <- pExp3
          pExp2' $ Add x1 x2,
        do
          lString "-"
          x2 <- pExp3
          pExp2' $ Sub x1 x2,
        pure x1
      ]

pExp3 :: Parser Exp
pExp3 = pExp4 >>= pExp3'
  where
    pExp3' x1 = choice
      [ do
          lString "*"
          x2 <- pExp4
          pExp3' $ Mul x1 x2,
        do
          lString "/"
          x2 <- pExp4
          pExp3' $ Div x1 x2,
        pure x1
      ]

pExp4 :: Parser Exp
pExp4 = pExp5 >>= pExp4'
  where
    pExp4' x1 = choice
      [ do
          lString "**"
          x2 <- pExp4
          pExp4' $ Pow x1 x2,
        pure x1
      ]

pExp5 :: Parser Exp
pExp5 = choice 
  [ pLExp,
    do
      lKeyword "print"
      lString "\""
      x <- lVName
      lString "\""
      y <- pAtom
      pure $ Print x y,
    do
      lKeyword "get"
      x <- pAtom
      pure $ KvGet x,
    do
      lKeyword "put"
      x <- pAtom
      -- lString " "
      y <- pAtom
      pure $ KvPut x y
  ]


-- pLExp :: Parser Exp
-- pLExp =
--   choice
--     [ If
--         <$> (lKeyword "if" *> pExp)
--         <*> (lKeyword "then" *> pExp)
--         <*> (lKeyword "else" *> pExp),
--       pAtom
--     ]

-- pExp1 :: Parser Exp
-- pExp1 = pLExp >>= chain
--   where
--     chain x =
--       choice
--         [ do
--             lString "*"
--             y <- pLExp
--             chain $ Mul x y,
--           do
--             lString "/"
--             y <- pLExp
--             chain $ Div x y,
--           pure x
--         ]

-- pExp0 :: Parser Exp
-- pExp0 = pExp1 >>= chain
--   where
--     chain x =
--       choice
--         [ do
--             lString "+"
--             y <- pExp1
--             chain $ Add x y,
--           do
--             lString "-"
--             y <- pExp1
--             chain $ Sub x y,
--           pure x
--         ]

-- pExp :: Parser Exp
-- pExp = pExp0

pExp :: Parser Exp
pExp = pExp1

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
