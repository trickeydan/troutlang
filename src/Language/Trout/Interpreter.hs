module Language.Trout.Interpreter 
(
    runProgram
)
where

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
evalStatement (Assignment ident expr) = evalAssignment ident expr
evalStatement (ConditionalIf _ _ ) = troutPrint "If is currently unimplemented."
evalStatement (PrintExpr intExpr) = evalPrintExpr intExpr
evalStatement (PrintIdentifier ident) = evalPrintIdentifier ident

-- Assignment

evalAssignment :: Identifier -> IntExpr -> TroutState ()
evalAssignment (Variable name) expr = evalAssignmentToVariable name expr
evalAssignment (InputIndex index) expr = troutPrint "Indices are currently unimplemented."
evalAssignment (ReturnIndex index) expr =  troutPrint "Indices are currently unimplemented."

evalAssignmentToVariable :: String -> IntExpr -> TroutState ()
evalAssignmentToVariable name expr = do
    let value = reduceIntExpr expr
    troutSetVar name value
    troutDumpState

-- Print Statements

evalPrintExpr :: IntExpr -> TroutState ()
evalPrintExpr expr = troutPrint $ reduceIntExpr expr

evalPrintIdentifier :: Identifier -> TroutState ()
evalPrintIdentifier ident = troutPrint $ reduceIdentifier ident
