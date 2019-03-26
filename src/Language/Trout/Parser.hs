{-# LANGUAGE OverloadedStrings #-}

module Language.Trout.Parser (

) where

import Text.Megaparsec
import Data.Void
import Data.Text

type Parser = Parsec Void Text
