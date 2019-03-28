module Language.Trout.Interpreter 
(
    executeProgram
)
where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
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
evalStatement (Assignment ident expr) = evalAssignment ident expr
evalStatement (NullAssignment _) = troutPrint "Null Assignment is currently unimplemented."
evalStatement (ConditionalIf _ _ ) = troutPrint "ConditionalIf is currently unimplemented."
evalStatement (Print expr) = evalPrintStatement expr
evalStatement (Break) = troutPrint "Break is currently unimplemented."

evalExpr :: Expr -> TroutState VarValue
evalExpr (SExpr expr) = error "SExpr evaluation is unimplemented"
evalExpr (VExpr expr) = error "VExpr evaluation is unimplemented"
evalExpr (FExpr expr) = error "FExpr evaluation is unimplemented"
evalExpr (IExpr expr) = do
    eval <- evalIntExpr expr
    return $ IntVal eval

-- Assignment Statement

evalAssignment :: Identifier -> Expr -> TroutState ()
evalAssignment (Variable name) expr = do
    val <- evalExpr expr
    troutSetVar name val
evalAssignment _ _ = error "Only assignment to variables is allowed"


-- Print Statement

evalPrintStatement:: Expr -> TroutState ()
evalPrintStatement (IExpr expr) = do
    val <- evalExpr (IExpr expr)
    troutPrint $ troutGetIntFromVarValue val
evalPrintStatement _ = error "Print in is not implemented for that."