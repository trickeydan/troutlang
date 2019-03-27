{-# LANGUAGE OverloadedStrings #-}

module Language.Trout.Parser (
  identifier,
  intExpr
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

intExpr :: Parser IntExpr
intExpr = makeExprParser exprTerm opTable
  where
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
    pref c f = Prefix (f <$ symbol c)
    inf c f = InfixL (f <$ symbol c)
