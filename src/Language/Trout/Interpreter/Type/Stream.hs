module Language.Trout.Interpreter.Type.Stream where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.Type.Frame
import Language.Trout.Grammar
import Language.Trout.Error

evalStreamExpr :: StreamExpr -> TroutState [[Int]]
evalStreamExpr (Stream fexprs) = ooF fexprs
    where
        ooF :: [FrameExpr] -> TroutState [[Int]]
        ooF [] = return []
        ooF (x:xs) = do
            ints <- getIntListFromFExpr x
            next <- ooF xs
            return $ ints:next
                where
                    getIntListFromFExpr :: FrameExpr -> TroutState [Int]
                    getIntListFromFExpr fexpr = do
                        intexprs <- evalFrameExpr fexpr
                        reduceIntExprList intexprs

evalStreamExpr (AppendStream expr1 expr2) = do
    st1 <- evalStreamExpr expr1
    st2 <- evalStreamExpr expr2
    return $ st1 ++ st2
evalStreamExpr (StreamIdentifier ident) =  do
    expr <- evalStreamIdentifier ident
    evalStreamExpr expr
evalStreamExpr (Iterator _ _) = do
    notImplemented "BLOCKED: Iterators are not implemented"
    return []
evalStreamExpr InputStream = do
    notImplemented "TODO: input stream"
    return []

evalStreamIdentifier :: Identifier -> TroutState StreamExpr
evalStreamIdentifier (InputIndex _) = do
    typeError "An input index is only of type int and is not compatible with type stream."
    return $ Stream []
evalStreamIdentifier (ReturnIndex _) = do
    typeError "A return index is only of type int and is not compatible with type stream."
    return $ Stream []
evalStreamIdentifier (Variable name) = do
    val <- troutGetVar name StreamType
    let str = checkVarStream val
    return str
    where
        checkVarStream :: VarValue -> StreamExpr
        checkVarStream (StreamVal _) = Stream [] -- Fix me
        checkVarStream _ = error "TypeError: Expected Stream"