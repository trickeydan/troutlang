module Language.Trout.Interpreter 
(
    executeProgram
)
where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Type.Int
import Language.Trout.Interpreter.Type.Stream
import Language.Trout.Grammar

executeProgram :: Program -> TroutState ()
executeProgram program = do
    evalProgram program
    return ()

evalProgram :: Program -> TroutState ()
evalProgram [] = troutPrint "Execution complete."
evalProgram (x:xs) = do 
    evalStatement x
    evalProgram xs

evalStatement :: Statement -> TroutState ()
evalStatement (Assignment _ _) = troutPrint "Assignment is currently unimplemented."
evalStatement (NullAssignment _) = troutPrint "Null Assignment is currently unimplemented."
evalStatement (ConditionalIf _ _ ) = troutPrint "ConditionalIf is currently unimplemented."
evalStatement (Print expr) = evalPrintStatement expr
evalStatement (Break) = troutPrint "Break is currently unimplemented."

-- Print Statement

evalPrintStatement:: Expr -> TroutState ()
evalPrintStatement (SExpr expr)= do
    st <- evalStreamExpr expr
    troutPrint st
evalPrintStatement (FExpr expr)= troutPrint "Frame printing is currently unimplemented."
evalPrintStatement (IExpr expr) = do
    val <- evalIntExpr expr
    troutPrint val
evalPrintStatement (VExpr expr)= troutPrint "Variable printing is currently unimplemented."