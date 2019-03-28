module Language.Trout.Interpreter.Type.Int where

import Language.Trout.Grammar
import Language.Trout.Interpreter.State

evalIntExpr :: IntExpr -> TroutState Int
evalIntExpr (IntNum n) = return n
evalIntExpr (IntIdentifier ident) = error "Bees"
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