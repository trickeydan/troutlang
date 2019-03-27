module Main where

import Language.Trout.Interpreter
import Language.Trout.Interpreter.Store
import Language.Trout.Grammar
import Control.Monad.State

program :: Program
program = 
    PrintExpr (IntNum 10): 
    PrintExpr (IntAdd (IntNum 10) (IntNum 12)):
    PrintExpr (IntAdd (IntNum 10) (IntAdd (IntNum 10) (IntNum 12))):[]


main :: IO ()
main = runStateT (runProgram program) (TroutStore []) >> return ()
