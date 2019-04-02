module Language.Trout.Interpreter.Type.Frame where

import Language.Trout.Grammar
import Language.Trout.Error
import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.Type.Int

evalFrameExpr :: FrameExpr -> TroutState [IntExpr]
evalFrameExpr (Frame xs) = return xs
evalFrameExpr (FrameIdentifier ident) = do
    val <- evalFrameIdentifier ident
    return $ map IntNum val
evalFrameExpr (AppendFrame expr1 expr2) = do
    val1 <- evalFrameExpr expr1
    val2 <- evalFrameExpr expr2
    return $ val1 ++ val2

evalFrameIdentifier :: Identifier -> TroutState [Int]
evalFrameIdentifier (Variable name) = do
    val <- troutGetVar name FrameType
    return $ troutGetFrameFromVarValue val
evalFrameIdentifier _ = do
    typeError "Only Integers can be stored in Indices"
    return []

getFrameVarValue :: [IntExpr] -> TroutState VarValue
getFrameVarValue exprs = do
    ints <- reduceIntExprList exprs
    return $ FrameVal ints

reduceIntExprList :: [IntExpr] -> TroutState [Int]
reduceIntExprList [] = return []
reduceIntExprList (x:xs) = do
    val <- evalIntExpr x
    vals <- reduceIntExprList xs
    return (val:vals)