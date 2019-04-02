{-# LANGUAGE OverloadedStrings #-}

module Language.Trout.Parser (
  identifier,
  intExpr,
  frameExpr,
  streamExpr,
  expr,
  condition,
  statement,
  programParser,
  fileParser
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Control.Monad
import Data.Void
import Data.Text

import Language.Trout.Grammar

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

identifier :: Parser Identifier
identifier = lexeme $ choice
  [ variable
  , inputIndex
  , returnIndex ]
  where
    variable :: Parser Identifier
    variable = Variable <$> ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
    inputIndex :: Parser Identifier
    inputIndex = InputIndex <$> between (symbol "[") (symbol "]") (intExpr <?> "input stream index")
    returnIndex :: Parser Identifier
    returnIndex = ReturnIndex <$> between (symbol "<") (symbol ">") (intExpr <?> "output stream index")

pref :: Text -> (a -> a) -> Operator Parser a
pref c f = Prefix (f <$ symbol c)

inf :: Text -> (a -> a -> a) -> Operator Parser a
inf c f = InfixL (f <$ symbol c)

intExpr :: Parser IntExpr
intExpr = makeExprParser exprTerm opTable
  where
    exprTerm :: Parser IntExpr
    exprTerm = choice
      [ between (symbol "(") (symbol ")") intExpr
      , IntNum <$> lexeme L.decimal
      , IntIdentifier <$> identifier ]
    opTable = [
        [ pref "+" IntPositive
        , pref "-" IntNegative ],
        [ inf "/" IntDivide ],
        [ inf "*" IntMultiply ],
        [ inf "+" IntAdd ],
        [ inf "-" IntSubtract]
      ]

frameExpr :: Parser FrameExpr
frameExpr = makeExprParser exprTerm opTable
  where
    exprTerm :: Parser FrameExpr
    exprTerm = choice
      [ (\i -> Frame [i]) <$> try intExpr
      , FrameIdentifier <$> identifier ]
    opTable = [
        [ inf "," AppendFrame]
      ]

streamExpr :: Parser StreamExpr
streamExpr = choice
  [ try iteratorStream
  , flatStream ]
  where
    flatStream :: Parser StreamExpr
    flatStream = makeExprParser exprTerm opTable
    exprTerm :: Parser StreamExpr
    exprTerm = choice
      [ (\f -> Stream [f]) <$> try frameExpr
      , StreamIdentifier <$> identifier ]
    opTable = [[inf "&" AppendStream]]
    iteratorStream :: Parser StreamExpr
    iteratorStream =
      Iterator <$>
      flatStream <*>
      between (symbol "{") (symbol "}") programParser

expr :: Parser Expr
expr = choice
  [ try $ liftM simplifyExpr nonIdExpr
  , VExpr <$> identifier ]
  where
    nonIdExpr :: Parser Expr
    nonIdExpr = do
      e <- complexExpr
      if isId e then fail "" else return e
    complexExpr :: Parser Expr
    complexExpr = choice
      [ SExpr <$> try streamExpr
      , FExpr <$> try frameExpr
      , IExpr <$> try intExpr ]
    isId :: Expr -> Bool
    isId (SExpr (StreamIdentifier _)) = True
    isId (FExpr (FrameIdentifier _)) = True
    isId (IExpr (IntIdentifier _)) = True
    isId _ = False
    simplifyExpr :: Expr -> Expr
    simplifyExpr (SExpr (Stream [f])) = simplifyExpr (FExpr f)
    simplifyExpr (FExpr (Frame [i])) = simplifyExpr (IExpr i)
    simplifyExpr e = e

condition :: Parser Condition
condition = lexeme $ try equals <|> notEquals
  where
    equals = do
      left <- expr
      _ <- symbol "=="
      right <- expr
      return $ Equals left right
    notEquals = do
      left <- expr
      _ <- symbol "!="
      right <- expr
      return $ NotEquals left right

statement :: Parser Statement
statement = choice
  [ try breaks
  , try nullAssignment
  , try assignment
  , try conditionalIf
  , prints ]
  where
    breaks = symbol "break" >> return Break
    nullAssignment =
      symbol "_" >>
      symbol "=" >>
      expr >>= return . NullAssignment
    assignment = do
      i <- identifier
      _ <- symbol "="
      e <- expr
      return $ Assignment i e
    conditionalIf = do
      _ <- symbol "if"
      c <- between (symbol "(") (symbol ")") condition
      s <- statement
      return $ ConditionalIf c s
    prints = expr >>= return . Print

programParser :: Parser Program
programParser = choice
  [ try statements
  , emptyProgram ]
  where
    blankLines = many (try $ spaceConsumer >> eol) >> spaceConsumer
    emptyProgram = blankLines >> return []
    statements = do
      _ <- blankLines
      s <- statement
      ss <- programParser
      return (s:ss)

fileParser :: Parser Program
fileParser = do
  p <- programParser
  _ <- eof
  return p
