module Language.Trout.Interpreter.IO (
  StreamBuffer,
  InBuffer(InBuffer),
  OutBuffer(OutBuffer),
  printToBuffer,
  extendBuffer,
  extractLatestInput
) where

import Prelude hiding (getLine, putStr, putStrLn)
import Data.Text(Text)
import Data.Text.IO
import System.IO(isEOF)
import System.Exit(exitSuccess)

type StreamBuffer = (InBuffer, OutBuffer)
newtype InBuffer = InBuffer [Maybe Text]
newtype OutBuffer = OutBuffer [Text]

printToBuffer :: StreamBuffer -> Text -> IO StreamBuffer
printToBuffer (InBuffer i, OutBuffer o) t = do
  fullBuffer <- fillBuffer (InBuffer i, OutBuffer (t:o))
  putStrLn t
  return fullBuffer

extendBuffer :: StreamBuffer -> IO StreamBuffer
extendBuffer (InBuffer i, OutBuffer o) = fetchLine >>=
  (\l -> return (InBuffer (Just l : i), OutBuffer o))

extractLatestInput :: StreamBuffer -> IO (StreamBuffer, Text)
extractLatestInput b@(InBuffer [], _) =
  extendBuffer b >>= extractLatestInput
extractLatestInput b@(InBuffer i, o ) =
  if
    needExtending i
  then
    extendBuffer b >>= extractLatestInput
  else do
    let (i', value) = extractLast i
    return ((InBuffer i', o), value)
  where
    needExtending [] = True
    needExtending [Just _] = False
    needExtending [Nothing] = True
    needExtending (_:xs) = needExtending xs

    extractLast [Just txt] = ([Nothing], txt)
    extractLast (x:xs) = (x : fst r, snd r)
      where r = extractLast xs
    extractLast [] = error "Internal error."

fillBuffer :: StreamBuffer -> IO StreamBuffer
fillBuffer b@(InBuffer i, OutBuffer o) =
  if
    length i < length o
  then do
    l <- fetchLine
    fillBuffer (InBuffer (Just l : i), OutBuffer o)
  else
    return b

fetchLine :: IO Text
fetchLine = do
  eof <- isEOF
  if eof then exitSuccess else getLine