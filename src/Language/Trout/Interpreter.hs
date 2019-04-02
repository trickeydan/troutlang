module Language.Trout.Interpreter 
(
    executeProgram
)
where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.Type.Int
import Language.Trout.Interpreter.Type.Frame
import Language.Trout.Interpreter.Type.Stream
import Language.Trout.Grammar
import Language.Trout.Error

import Control.Monad(void)

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
evalStatement Break = notImplemented "Break is currently unimplemented."

evalExpr :: Expr -> TroutState VarValue
evalExpr (SExpr expr) = do
    intss <- evalStreamExpr expr
    return $ StreamVal intss
evalExpr (FExpr expr) = do
    intExprs <- evalFrameExpr expr
    getFrameVarValue intExprs
evalExpr (IExpr expr) = do
    eval <- evalIntExpr expr
    return $ IntVal eval
evalExpr (VExpr ident) = evalIdentifier ident

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
evalNullAssignment expr = void $ evalExpr expr

-- Print Statement

evalPrintStatement:: Expr -> TroutState ()
evalPrintStatement expr = do
    val <-  evalExpr expr
    troutPrint val