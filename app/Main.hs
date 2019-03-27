module Main where

import Language.Trout.Interpreter
import Language.Trout.Interpreter.Store
import Language.Trout.Grammar
import Control.Monad.State
import Control.Monad (void)

program :: Program
program = 
    [
        PrintExpr (IntNum 10),
        PrintExpr (IntAdd (IntNum 10) (IntNum 12)),
        PrintExpr (IntAdd (IntNum 10) (IntAdd (IntNum 10) (IntNum 12))),
        Assignment (Variable "bee") (IntNum 12),
        PrintIdentifier (Variable "bee")
    ]


main :: IO ()
main = void $ runStateT (runProgram program) (TroutStore [])
