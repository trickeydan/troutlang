module Language.Trout.Interpreter 
(
    executeProgram
)
where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.Type.Int
import Language.Trout.Grammar
import Language.Trout.Error

executeProgram :: Program -> TroutState ()
executeProgram program = do
    evalProgram program
    return ()

evalProgram :: Program -> TroutState ()
evalProgram [] = return ()
evalProgram (x:xs) = do 
    evalStatement x
    evalProgram xs

evalStatement :: Statement -> TroutState ()
evalStatement (Assignment ident expr) = evalAssignment ident expr
evalStatement (NullAssignment expr) = evalNullAssignment expr
evalStatement (ConditionalIf _ _ ) = notImplemented "ConditionalIf is currently unimplemented."
evalStatement (Print expr) = evalPrintStatement expr
evalStatement (Break) = notImplemented "Break is currently unimplemented."

evalExpr :: Expr -> TroutState VarValue
evalExpr (SExpr _) = do
    notImplemented "SExpr evaluation is unimplemented"
    return $ IntVal 0
evalExpr (VExpr ident) = evalIdentifier ident
evalExpr (FExpr _) = do
    notImplemented "FExpr evaluation is unimplemented"
    return $ IntVal 0
evalExpr (IExpr expr) = do
    eval <- evalIntExpr expr
    return $ IntVal eval

evalIdentifier :: Identifier -> TroutState VarValue
evalIdentifier (Variable name) = troutGetVarAny name
evalIdentifier (InputIndex _) = do
    notImplemented "Evaluation of input indices"
    return $ IntVal 0
evalIdentifier (ReturnIndex _ ) = do
    notImplemented "Evaluation of output indices"
    return $ IntVal 0

-- Assignment Statement

evalAssignment :: Identifier -> Expr -> TroutState ()
evalAssignment (Variable name) expr = do
    val <- evalExpr expr
    troutSetVar name val
evalAssignment (InputIndex _) _ = notImplemented "Assignment to input indices"
evalAssignment _ _ = typeError "Only assignment to variables or input indices is allowed."

-- NullAssignment Statement

evalNullAssignment :: Expr -> TroutState ()
evalNullAssignment expr = evalExpr expr >> return ()

-- Print Statement

evalPrintStatement:: Expr -> TroutState ()
evalPrintStatement expr = do
    val <-  evalExpr expr
    troutPrint val