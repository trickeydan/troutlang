{-# LANGUAGE OverloadedStrings #-}

module Language.Trout.Parser (
  identifier,
  intExpr,
  frameExpr,
  stdInputFrameExpr,
  streamExpr,
  boolExpr,
  expr,
  statement,
  programParser,
  fileParser
) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
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

stdInputFrameExpr :: Parser FrameExpr
stdInputFrameExpr = Frame <$> intList
  where
    intList :: Parser [IntExpr]
    intList = (:) <$>
      fmap IntNum ( L.signed spaceConsumer $ lexeme L.decimal) <*>
      choice
        [ try intList
        , return [] ]

streamExpr :: Parser StreamExpr
streamExpr = choice
  [ try iteratorStream
  , flatStream ]
  where
    flatStream :: Parser StreamExpr
    flatStream = makeExprParser exprTerm opTable
    exprTerm :: Parser StreamExpr
    exprTerm = choice
      [ try (symbol "#") >> return InfiniteStream
      , try (symbol "IN") >> return InputStream
      , (\f -> Stream [f]) <$> try frameExpr
      , StreamIdentifier <$> identifier ]
    opTable = [[inf "&" AppendStream]]
    iteratorStream :: Parser StreamExpr
    iteratorStream =
      Iterator <$>
      flatStream <*>
      between (symbol "{") (symbol "}") programParser

boolExpr :: Parser BoolExpr
boolExpr = makeExprParser exprTerm opTable
  where
    exprTerm = choice
      [ try $ between (symbol "(") (symbol ")") boolExpr
      , try loneBool
      , comparison ]
    opTable = [
        [ pref "!" Not ],
        [ inf "|" Or
        , inf "." And ]
      ]
    comparison :: Parser BoolExpr
    comparison = try equals <|> try notEquals <|> try gt <|> try lt
      where
        equals = do
          left <- expr
          _ <- symbol "=="
          Equals left <$> expr
        notEquals = do
          left <- expr
          _ <- symbol "!="
          NotEquals left <$> expr
        gt = do
          left <- expr
          _ <- symbol ">"
          GreaterThan left <$> expr
        lt = do
          left <- expr
          _ <- symbol "<"
          LessThan left <$> expr
    loneBool :: Parser BoolExpr
    loneBool = choice
      [ symbol "True" >> return (Boolean True)
      , symbol "False" >> return (Boolean False) ]

expr :: Parser Expr
expr = choice
  [ try $ simplifyExpr <$> nonIdExpr
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
    isId (SExpr (Stream [a])) = isId (FExpr a)
    isId (FExpr (FrameIdentifier _)) = True
    isId (FExpr (Frame [a])) = isId (IExpr a)
    isId (IExpr (IntIdentifier _)) = True
    isId _ = False
    simplifyExpr :: Expr -> Expr
    simplifyExpr (SExpr (Stream [f])) = simplifyExpr (FExpr f)
    simplifyExpr (FExpr (Frame [i])) = simplifyExpr (IExpr i)
    simplifyExpr e = e

statement :: Parser Statement
statement = choice
  [ try breaks
  , try nullAssignment
  , try assignment
  , try conditionalIf
  , prints ]
  where
    breaks = symbol "break" >> return Break
    nullAssignment = fmap NullAssignment $
      symbol "_" >>
      symbol "=" >>
      expr
    assignment = do
      i <- identifier
      _ <- symbol "="
      Assignment i <$> expr
    conditionalIf = do
      _ <- symbol "if"
      c <- between (symbol "(") (symbol ")") boolExpr
      ConditionalIf c <$> statement
    prints = Print <$> expr

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
