{-# LANGUAGE OverloadedStrings #-}

module Language.Trout.Parser (

) where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text

type Parser = Parsec Void Text

spaceConsumer :: Parser ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer
