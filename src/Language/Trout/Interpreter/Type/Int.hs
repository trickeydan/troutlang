module Language.Trout.Interpreter.Type.Int where

import Language.Trout.Grammar
import Language.Trout.Error
import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store

evalIntExpr :: IntExpr -> TroutState Int
evalIntExpr (IntNum n) = return n
evalIntExpr (IntIdentifier ident) = evalIntIdentifier ident
evalIntExpr (IntAdd expr1 expr2) = do
    val1 <- evalIntExpr expr1
    val2 <- evalIntExpr expr2
    return (val1 + val2)
evalIntExpr (IntPositive expr) = evalIntExpr expr
evalIntExpr (IntNegative expr) = do
    val <- evalIntExpr expr
    return (- val)
evalIntExpr (IntSubtract expr1 expr2) = do
    val1 <- evalIntExpr expr1
    val2 <- evalIntExpr expr2
    return (val1 - val2)
evalIntExpr (IntDivide expr1 expr2) = do
    val1 <- evalIntExpr expr1
    val2 <- evalIntExpr expr2
    return (val1 `div` val2)
evalIntExpr (IntMultiply expr1 expr2) = do
    val1 <- evalIntExpr expr1
    val2 <- evalIntExpr expr2
    return (val1 * val2)

evalIntIdentifier :: Identifier -> TroutState Int
evalIntIdentifier (Variable name) = do
    val <- troutGetVar name IntType
    return $ troutGetIntFromVarValue val
evalIntIdentifier (InputIndex _) = do
    notImplemented "BLOCKED: Input Indices not implemented in integer expressions"
    return 0
evalIntIdentifier (ReturnIndex _) = do
    notImplemented "BLOCKED: Return Indices not implemented in integer expressions"
    return 0