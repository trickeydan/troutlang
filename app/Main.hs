module Main where

import Language.Trout.Interpreter
import Language.Trout.Interpreter.Store
import Language.Trout.Grammar
import Control.Monad.State
import Control.Monad (void)

program :: Program
program = 
    [
        Print (IExpr (IntNum 10)),
        Print (IExpr (IntAdd (IntNum 10) (IntNum 10))),
        Print (IExpr (IntMultiply (IntNum 10) (IntNum 10))),
        Assignment (Variable "test") (IExpr (IntNum 25)),
        Print (IExpr (IntIdentifier (Variable "test"))),
        Print (VExpr (Variable "test"))
    ]


main :: IO ()
main = void $ runStateT (executeProgram program) (TroutStore [])
