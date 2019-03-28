module Main where

import Language.Trout.Interpreter
import Language.Trout.Interpreter.Store
import Language.Trout.Grammar
import Control.Monad.State
import Control.Monad (void)

program :: Program
program = 
    [
    ]


main :: IO ()
main = void $ runStateT (runProgram program) (TroutStore [])
