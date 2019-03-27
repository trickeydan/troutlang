module Language.Trout.Interpreter where

import Language.Trout.Interpreter.Reduction
import Language.Trout.Interpreter.State
import Language.Trout.Grammar

runProgram :: Program -> TroutState ()
runProgram program = do
    evalProgram program
    return ()

evalProgram :: Program -> TroutState ()
evalProgram [] = troutPrint "Execution complete."
evalProgram (x:xs) = do 
    evalStatement x
    evalProgram xs

evalStatement :: Statement -> TroutState ()
evalStatement (Iterator _ _) = troutPrint "Iteration is currently unimplemented."
evalStatement (Assignment _ _) = troutPrint "Assignment is currently unimplemented."
evalStatement (ConditionalIf _ _ ) = troutPrint "If is currently unimplemented."
evalStatement (PrintExpr intExpr) = evalPrintExpr intExpr
evalStatement (PrintIdentifier ident) = evalPrintIdentifier ident

-- Print Statements

evalPrintExpr :: IntExpr -> TroutState ()
evalPrintExpr expr = troutPrint $ reduceIntExpr expr

evalPrintIdentifier :: Identifier -> TroutState ()
evalPrintIdentifier ident = troutPrint $ reduceIdentifier ident
