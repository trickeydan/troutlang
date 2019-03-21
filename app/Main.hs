module Main where

import Lib

import Data.Void
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Error

type Parser = Parsec Void String

main = do
  input <- fmap head getArgs
  parseTest singleLetterP input

singleLetterP :: Parser Char
singleLetterP = char 'h'
