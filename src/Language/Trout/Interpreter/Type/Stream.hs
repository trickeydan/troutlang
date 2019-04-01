module Language.Trout.Interpreter.Type.Stream where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
import Language.Trout.Grammar
import Language.Trout.Error

evalStreamExpr :: StreamExpr -> TroutState [FrameExpr]
evalStreamExpr (Stream fexprs) = return fexprs
evalStreamExpr (AppendStream expr1 expr2) = do
    st1 <- evalStreamExpr expr1
    st2 <- evalStreamExpr expr2
    return $ st1 ++ st2
evalStreamExpr (StreamIdentifier _) =  do
    notImplemented "StreamIdentifiers are not implemented."
    return []
evalStreamExpr (Iterator _ _) = do
    notImplemented "Iterators are not implemented"
    return []

evalStreamIdentifier :: Identifier -> TroutState StreamExpr
evalStreamIdentifier (InputIndex _) = do
    typeError "An input index is only of type int and is not compatible with type stream."
    return $ Stream []
evalStreamIdentifier (ReturnIndex _) = do
    typeError "An return index is only of type int and is not compatible with type stream."
    return $ Stream []
evalStreamIdentifier (Variable name) = do
    val <- troutGetVar name StreamType
    let str = checkVarStream val
    return str
    where
        checkVarStream :: VarValue -> StreamExpr
        checkVarStream (StreamVal _) = Stream [] -- Fix me
        checkVarStream _ = error "TypeError: Expected Stream"