module Language.Trout.Interpreter where

import Language.Trout.Interpreter.State
import Language.Trout.Grammar

evalProgram :: Program -> IO ()
evalProgram [] = putStrLn "Execution complete."
evalProgram (x:xs) = do 
    evalStatement x
    evalProgram xs

evalStatement :: Statement -> IO ()
evalStatement (PrintExpr intExpr) = evalPrintExpr intExpr
evalStatement _ = putStrLn "Unimplemented Statement."

evalPrintExpr :: IntExpr -> IO ()
evalPrintExpr expr = putStrLn $ show $ reduceIntExpr expr

reduceIntExpr :: IntExpr -> Int
reduceIntExpr (IntNum n) = n
reduceIntExpr (IntAdd expr1 expr2) = reduceIntExpr expr1 + reduceIntExpr expr2
reduceIntExpr (IntSubtract expr1 expr2) = reduceIntExpr expr1 - reduceIntExpr expr2
reduceIntExpr (IntDivide expr1 expr2) = reduceIntExpr expr1 `div` reduceIntExpr expr2
reduceIntExpr (IntMultiply expr1 expr2) = reduceIntExpr expr1 * reduceIntExpr expr2

