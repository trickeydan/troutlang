module Language.Trout.Interpreter where

import Control.Monad.State

import Language.Trout.Interpreter.Reduction
import Language.Trout.Interpreter.State
import Language.Trout.Grammar

runProgram :: Program -> TroutState ()
runProgram program = do
    evalProgram program
    return ()

evalProgram :: Program -> TroutState ()
evalProgram [] = liftIO $ putStrLn "Execution complete."
evalProgram (x:xs) = do 
    evalStatement x
    evalProgram xs

evalStatement :: Statement -> TroutState ()
evalStatement (PrintExpr intExpr) = evalPrintExpr intExpr
evalStatement (PrintIdentifier ident) = evalPrintIdentifier ident
evalStatement _ = liftIO $ putStrLn "Unimplemented Statement."

-- -- Print Statements

evalPrintExpr :: IntExpr -> TroutState ()
evalPrintExpr expr = liftIO $ print $ reduceIntExpr expr

evalPrintIdentifier :: Identifier -> TroutState ()
evalPrintIdentifier ident = liftIO $ print $ reduceIdentifier ident
