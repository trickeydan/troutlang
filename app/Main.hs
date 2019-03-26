module Main where

import Language.Trout.Interpreter
import Language.Trout.Grammar

program = 
    PrintExpr (IntNum 10): 
    PrintExpr (IntAdd (IntNum 10) (IntNum 12)):
    PrintExpr (IntAdd (IntNum 10) (IntAdd (IntNum 10) (IntNum 12))):[]


main :: IO ()
main = evalProgram program
