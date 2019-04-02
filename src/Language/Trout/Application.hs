module Language.Trout.Application (
    runApp
) where

import Language.Trout.Parser(fileParser)
import Language.Trout.Error(syntaxError)
import Language.Trout.Interpreter(executeProgram)
import Language.Trout.Interpreter.Store
import Text.Megaparsec(runParser)
import System.Environment(getArgs)
import Control.Monad
import Control.Monad.State
import Data.Text.IO(readFile)
import Prelude hiding(readFile)

runApp :: IO ()
runApp = do
  args <- getArgs
  let file = head args
  source <- readFile file
  let parsed = runParser fileParser file source
  program <- extractProgram parsed
  void $ runStateT (executeProgram program) (TroutStore [])
  where
    extractProgram (Left bundle) = syntaxError bundle >> return []
    extractProgram (Right program) = return program

