module Language.Trout.Interpreter.Type.Stream (evalStreamExpr) where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
import Language.Trout.Grammar

evalStreamExpr :: StreamExpr -> TroutState [FrameExpr]
evalStreamExpr (Stream fexprs) = return fexprs
evalStreamExpr (AppendStream expr1 expr2) = do
    st1 <- evalStreamExpr expr1
    st2 <- evalStreamExpr expr2
    return $ st1 ++ st2
evalStreamExpr (StreamIdentifier _) =  notImplemented "StreamIdentifiers are not implemented."
evalStreamExpr (Iterator expr statements) =  notImplemented "Iterators are not implemented"

evalStreamIdentifier :: Identifier -> TroutState StreamExpr
evalStreamIdentifier (InputIndex _) = error "An input index is only of type int and is not compatible with type stream."
evalStreamIdentifier (ReturnIndex _) = error "An return index is only of type int and is not compatible with type stream."
evalStreamIdentifier (Variable name) = do
    val <- troutGetVar name StreamType
    let str = checkVarStream val
    return str
    where
        checkVarStream :: VarValue -> StreamExpr
        checkVarStream (StreamVal _) = Stream [] -- Fix me
        checkVarStream _ = error "TypeError: Expected Stream"