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
    "put",
    "get",
    "loop",
    "for",
    "do",
    "while"
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

pBool :: Parser Bool
pBool =
  choice
    [ const True <$> lKeyword "true",
      const False <$> lKeyword "false"
    ]

pAtom :: Parser Exp
pAtom = do
  atom <- choice
    [ CstInt <$> lInteger,
      CstBool <$> pBool,
      Var <$> lVName,
      -- Parse the empty tuple
      do lString "()"; pure $ Tuple [],
      -- Try to parse a tuple
      try $ do
        lString "("
        e <- pTuple
        lString ")"
        pure e,
      try $ lString "(" *> pExp <* lString ")",
      KvPut <$> (lKeyword "put" *> pAtom) <*> pAtom,
      KvGet <$> (lKeyword "get" *> pAtom)
    ]
  -- Parse a tuple project
  pTupleProject atom

pTuple :: Parser Exp
pTuple = do
  -- Parse the first expression
  e <- pExp
  -- Parse the rest of the expressions
  es <- some (lString "," *> pExp)
  pure $ Tuple (e : es)

pTupleProject :: Exp -> Parser Exp
pTupleProject atom = choice 
  [ do
        lString "."
        i <- lInteger
        -- Parse consecutive projects
        pTupleProject (Project atom i),
      pure atom
    ]

pFExp :: Parser Exp
pFExp = chain =<< pAtom
  where
    chain x =
      choice
        [ do
            y <- pAtom
            chain $ Apply x y,
          pure x
        ]

pLExp :: Parser Exp
pLExp =
  choice
    [ If
        <$> (lKeyword "if" *> pExp)
        <*> (lKeyword "then" *> pExp)
        <*> (lKeyword "else" *> pExp),
      Lambda
        <$> (lString "\\" *> lVName)
        <*> (lString "->" *> pExp),
      Let
        <$> (lKeyword "let" *> lVName)
        <*> (lString "=" *> pExp)
        <*> (lKeyword "in" *> pExp),
      try $ do
        lKeyword "loop"
        pvar <- lVName
        lString "="
        initp <- pExp
        lKeyword "for"
        ivar <- lVName
        lString "<"
        bound <- pExp
        lKeyword "do"
        body <- pExp
        pure $ ForLoop (pvar, initp) (ivar, bound) body,
      try $ do
        lKeyword "loop"
        pvar <- lVName
        lString "="
        initp <- pExp
        lKeyword "while"
        cond <- pExp
        lKeyword "do"
        body <- pExp
        pure $ WhileLoop (pvar, initp) cond body,
      pFExp
    ]

pExp3 :: Parser Exp
pExp3 = pLExp >>= chain
  where
    chain x =
      choice
        [ do
            lString "*"
            y <- pLExp
            chain $ Mul x y,
          do
            lString "/"
            y <- pLExp
            chain $ Div x y,
          pure x
        ]

pExp2 :: Parser Exp
pExp2 = pExp3 >>= chain
  where
    chain x =
      choice
        [ do
            lString "+"
            y <- pExp3
            chain $ Add x y,
          do
            lString "-"
            y <- pExp3
            chain $ Sub x y,
          pure x
        ]

pExp1 :: Parser Exp
pExp1 = pExp2 >>= chain
  where
    chain x =
      choice
        [ do
            lString "=="
            y <- pExp2
            chain $ Eql x y,
          pure x
        ]

pBothOf :: Parser Exp
pBothOf = pExp1 >>= pBothOf'
  where
    pBothOf' e1 =
      choice
        [
          do
            lString "&&"
            e2 <- pExp1
            pBothOf' $ BothOf e1 e2,
          pure e1
        ]

pOneOf :: Parser Exp
pOneOf = pBothOf >>= pOneOf'
  where
    pOneOf' e1 = 
      choice 
        [
          do
            lString "||"
            e2 <- pBothOf
            pOneOf' $ OneOf e1 e2,
          pure e1
        ]

pExp :: Parser Exp
pExp = pOneOf

parseAPL :: FilePath -> String -> Either String Exp
parseAPL fname s = case parse (space *> pExp <* eof) fname s of
  Left err -> Left $ errorBundlePretty err
  Right x -> Right x
