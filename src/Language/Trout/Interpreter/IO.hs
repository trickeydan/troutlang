module Language.Trout.Interpreter.IO (
  StreamBuffer,
  InBuffer(InBuffer),
  OutBuffer(OutBuffer)
) where

import Language.Trout.Grammar
import Prelude hiding (getLine, putStr, putStrLn)
import Data.Text(Text)
import Data.Text.IO
import System.IO(isEOF)
import System.Exit(exitSuccess)

type StreamBuffer = (InBuffer, OutBuffer)
newtype InBuffer = InBuffer [Text]
newtype OutBuffer = OutBuffer [Text]

printToBuffer :: StreamBuffer -> Text -> IO StreamBuffer
printToBuffer (InBuffer i, OutBuffer o) t = do
  fullBuffer <- fillBuffer (InBuffer i, OutBuffer (t:o))
  putStrLn t
  return fullBuffer

fillBuffer :: StreamBuffer -> IO StreamBuffer
fillBuffer b@(InBuffer i, OutBuffer o) =
  if
    length i < length o
  then do
    l <- fetchLine
    fillBuffer (InBuffer (l:i), OutBuffer o)
  else
    return b

fetchLine :: IO Text
fetchLine = do
  eof <- isEOF
  if eof then exitSuccess else getLine