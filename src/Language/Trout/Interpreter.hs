module Language.Trout.Interpreter where

import Language.Trout.Interpreter.Reduction
import Language.Trout.Interpreter.State
import Language.Trout.Grammar

evalProgram :: Program -> IO ()
evalProgram [] = putStrLn "Execution complete."
evalProgram (x:xs) = do 
    evalStatement x
    evalProgram xs

evalStatement :: Statement -> IO ()
evalStatement (PrintExpr intExpr) = evalPrintExpr intExpr
evalStatement (PrintIdentifier ident) = evalPrintIdentifier ident
evalStatement _ = putStrLn "Unimplemented Statement."

-- Print Statements

evalPrintExpr :: IntExpr -> IO ()
evalPrintExpr expr = putStrLn $ show $ reduceIntExpr expr

evalPrintIdentifier :: Identifier -> IO ()
evalPrintIdentifier expr = putStrLn $ show $ reduceIdentifier ident
