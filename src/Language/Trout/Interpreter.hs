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
evalStatement (ConditionalIf _ _ ) = notImplemented "BLOCKED: ConditionalIf is currently unimplemented."
evalStatement (Print expr) = evalPrintStatement expr
evalStatement Break = notImplemented "BLOCKED: Break is currently unimplemented."

evalExpr :: Expr -> TroutState VarValue
evalExpr (SExpr expr) = do
    intss <- evalStreamExpr expr
    let out = StreamVal intss
    troutPrint out
    return out
evalExpr (FExpr expr) = do
    p <- getPrintContext
    setPrintContext (PrintContext False)
    intExprs <- evalFrameExpr expr
    setPrintContext p
    vv <- getFrameVarValue intExprs
    troutPrint vv
    return vv
evalExpr (IExpr expr) = do
    p <- getPrintContext
    setPrintContext (PrintContext False)
    eval <- evalIntExpr expr
    setPrintContext p
    let out = IntVal eval
    troutPrint out
    return out
evalExpr (VExpr ident) = do
    p <- getPrintContext
    setPrintContext (PrintContext False)
    out <- evalIdentifier ident
    setPrintContext p
    troutPrint out
    return out

evalIdentifier :: Identifier -> TroutState VarValue
evalIdentifier (Variable name) = troutGetVarAny name
evalIdentifier (InputIndex iiexpr) = do
    i <- evalIntExpr iiexpr
    StreamContext (fr, _) <- getStreamContext
    return $ IntVal $ fromII i fr
    where
        fromII index (IterationFrame is) = is !! index
        fromII _ _ = error "Input index not found in input stream."
evalIdentifier (ReturnIndex riexpr ) = do
    i <- evalIntExpr riexpr
    v <- troutGetIndex i
    return $ IntVal v

-- Assignment Statement

evalAssignment :: Identifier -> Expr -> TroutState ()
evalAssignment (Variable name) expr = do
    val <- evalExpr expr
    troutSetVar name val
evalAssignment (ReturnIndex riexpr) e = do
    i <- evalIntExpr riexpr
    v <- evalExpr e
    writeIndex i v
    where
        writeIndex i (IntVal r) = troutSetIndex i r
        writeIndex _ _ = typeError "Only integers may be stored in return indices."
evalAssignment _ _ = typeError "Only assignment to variables or return indices is allowed."

-- NullAssignment Statement

evalNullAssignment :: Expr -> TroutState ()
evalNullAssignment expr = void $ evalExpr expr

-- Print Statement

evalPrintStatement:: Expr -> TroutState ()
evalPrintStatement expr = do
    setPrintContext $ PrintContext True
    _ <- evalExpr expr
    setPrintContext $ PrintContext False
