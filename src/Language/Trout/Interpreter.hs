module Language.Trout.Interpreter 
(
    executeProgram
)
where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.Type.Int
import Language.Trout.Interpreter.Type.Frame
import Language.Trout.Grammar
import Language.Trout.Error
import System.Exit(exitSuccess)
import Control.Monad.State
import Control.Monad(when, void)
import Data.HashMap.Strict(empty)

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
evalStatement (ConditionalIf bexpr stmt ) = evalConditional bexpr stmt -- Note: also a special case in iterators.
evalStatement (Print expr) = evalPrintStatement expr
evalStatement Break = liftIO exitSuccess -- Note: this needs to be handled specialy inside iterators.

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

-- ConditionalIf Statement

evalConditional :: BoolExpr -> Statement -> TroutState ()
evalConditional bexpr stmt = do
    conditionMet <- evalBoolExpr bexpr
    when conditionMet $ evalStatement stmt

evalBoolExpr :: BoolExpr -> TroutState Bool
evalBoolExpr (Boolean b) = return b
evalBoolExpr (Equals a b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    return $ a' == b'
evalBoolExpr (NotEquals a b) = do
    a' <- evalExpr a
    b' <- evalExpr b
    return $ a' /= b'
evalBoolExpr (LessThan a b) = do
    (IntVal a') <- evalExpr a
    (IntVal b') <- evalExpr b
    return $ a' < b'
evalBoolExpr (GreaterThan a b) = do
    (IntVal a') <- evalExpr a
    (IntVal b') <- evalExpr b
    return $ a' > b'
evalBoolExpr (Or a b) = do
    a' <- evalBoolExpr a
    b' <- evalBoolExpr b
    return $ a' || b'
evalBoolExpr (And a b) = do
    a' <- evalBoolExpr a
    b' <- evalBoolExpr b
    return $ a' && b'
evalBoolExpr (Not a) = do
    a' <- evalBoolExpr a
    return $ not a'

-- Stream Handling

evalStreamExpr :: StreamExpr -> TroutState [[Int]]
evalStreamExpr (Stream []) = return []
evalStreamExpr (Stream (f:fs)) = do
    p <- getPrintContext
    setPrintContext (PrintContext False)
    (FrameVal f') <- evalExpr (FExpr f)
    setPrintContext p
    troutPrint (FrameVal f')
    restOfTheOwl <- evalStreamExpr (Stream fs)
    return $ f' : restOfTheOwl
evalStreamExpr InputStream = do
    f <- troutRead
    troutPrint (FrameVal f)
    remainingIn <- evalStreamExpr InputStream
    return (f:remainingIn)
evalStreamExpr (AppendStream s1 s2) = do
    s1' <- evalStreamExpr s1
    s2' <- evalStreamExpr s2
    return (s1' ++ s2')
evalStreamExpr (StreamIdentifier i) = do
    (StreamVal r) <- evalIdentifier i
    return r
evalStreamExpr (Iterator s ss) = do
    s' <- evalIterator s ss
    troutPrint (StreamVal s')
    return s'

evalIterator :: StreamExpr -> [Statement] -> TroutState [[Int]]
evalIterator InputStream ss = do
    sc <- getStreamContext
    inFrame <- troutRead
    setStreamContext $ StreamContext $
        (IterationFrame inFrame, empty)
    outFrame <- iterationStep ss
    term <- iterationTerminated
    if term
        then do
            setStreamContext sc
            return [outFrame]
        else do
            remaining <- evalIterator InputStream ss
            setStreamContext sc
            return $ outFrame : remaining
evalIterator e ss = do
    sc <- getStreamContext
    pc <- getPrintContext
    setPrintContext (PrintContext False)
    inStream <- evalStreamExpr e
    setPrintContext pc
    out <- iterateOver inStream ss
    setStreamContext sc
    return out
    where
        iterateOver :: [[Int]] -> [Statement] -> TroutState [[Int]]
        iterateOver [] _ = return []
        iterateOver (f:fs) stmts = do
            setStreamContext $ StreamContext $
                (IterationFrame f, empty)
            step <- iterationStep ss
            term <- iterationTerminated
            if term
                then return [step]
                else do
                    remainingSteps <- iterateOver fs stmts
                    return $ step : remainingSteps

iterationTerminated :: TroutState Bool
iterationTerminated = do
    (StreamContext (i, _)) <- getStreamContext
    if i == BlankStream
        then return True
        else return False

iterationStep :: [Statement] -> TroutState [Int]
iterationStep [] = troutGetOutputFrame
iterationStep (Break : _) = do
    setStreamContext $ StreamContext (BlankStream, empty)
    troutGetOutputFrame
iterationStep (ConditionalIf bexpr Break : ss) = do
    bresult <- evalBoolExpr bexpr
    if bresult
        then iterationStep (Break : ss)
        else iterationStep ss
iterationStep (s:ss) = do
    evalStatement s
    iterationStep ss
