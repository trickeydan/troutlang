module Language.Trout.Interpreter 
(
    executeProgram,
    wrapInt,
    wrapFrame,
    wrapStream
)
where

import Language.Trout.Interpreter.State
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.Type.Int
import Language.Trout.Grammar
import Language.Trout.Error
import System.Exit(exitSuccess)
import Control.Monad.State
import Control.Monad(when, void)
import Data.HashMap.Strict(empty)
import Data.Maybe

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
evalExpr e = let v = castVExpr e in doEval v e
    where
        doEval (Just a) _ = evalConcreteExpr a
        doEval Nothing a = evalConcreteExpr a

evalConcreteExpr :: Expr -> TroutState VarValue
evalConcreteExpr (SExpr expr) = do
    intss <- evalStreamExpr expr
    let out = StreamVal intss
    return out
evalConcreteExpr (FExpr expr) = do
    p <- getPrintContext
    setPrintContext (PrintContext False)
    intExprs <- evalFrameExpr expr
    setPrintContext p
    vv <- getFrameVarValue intExprs
    troutPrint vv
    return vv
evalConcreteExpr (IExpr expr) = do
    p <- getPrintContext
    setPrintContext (PrintContext False)
    eval <- evalIntExpr expr
    setPrintContext p
    let out = IntVal eval
    troutPrint out
    return out
evalConcreteExpr (VExpr ident) = do
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
    pc <- getPrintContext
    setPrintContext (PrintContext False)
    val <- evalExpr expr
    troutSetVar name val
    setPrintContext pc
evalAssignment (ReturnIndex riexpr) e = do
    pc <- getPrintContext
    setPrintContext (PrintContext False)
    i <- evalIntExpr riexpr
    v <- evalExpr e
    writeIndex i v
    setPrintContext pc
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

-- Frame Handling

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
    if isNothing $ castVExpr $ FExpr $ Frame exprs
        then do
            ints <- reduceIntExprList exprs
            return $ FrameVal ints
        else do
            let (Just (VExpr i)) = castVExpr $ FExpr $ Frame exprs
            wrapFrame <$> evalIdentifier i

reduceIntExprList :: [IntExpr] -> TroutState [Int]
reduceIntExprList [] = return []
reduceIntExprList (x:xs) = do
    val <- evalIntExpr x
    vals <- reduceIntExprList xs
    return (val:vals)

-- Stream Handling

evalStreamExpr :: StreamExpr -> TroutState [[Int]]
evalStreamExpr (Stream []) = return []
evalStreamExpr (Stream (f:fs)) = do
    p <- getPrintContext
    setPrintContext (PrintContext False)
    ef <- evalExpr (FExpr f)
    if isNothing $ castVExpr (FExpr f)
    then do
        let (FrameVal f') = wrapFrame ef
        setPrintContext p
        troutPrint (FrameVal f')
        restOfTheOwl <- evalStreamExpr (Stream fs)
        return $ f' : restOfTheOwl
    else do
        let (StreamVal s) = wrapStream ef
        evalStreamExpr $ (Stream . map (Frame . map IntNum)) s

evalStreamExpr InputStream = do
    f <- troutRead
    troutPrint (FrameVal f)
    remainingIn <- evalStreamExpr InputStream
    return (f:remainingIn)
evalStreamExpr InfiniteStream = do
    troutPrint (FrameVal [1])
    remaining <- evalStreamExpr InfiniteStream
    return ([1]:remaining)
evalStreamExpr (AppendStream s1 s2) = do
    s1' <- evalStreamExpr s1
    s2' <- evalStreamExpr s2
    return (s1' ++ s2')
evalStreamExpr (StreamIdentifier i) = do
    ei <- evalIdentifier i
    let (StreamVal r) = wrapStream ei
    return r
evalStreamExpr (Iterator s ss) = do
    s' <- evalIterator s ss
    troutPrint (StreamVal s')
    return s'

evalIterator :: StreamExpr -> [Statement] -> TroutState [[Int]]
evalIterator InputStream ss = evalUnbounded troutRead ss
evalIterator InfiniteStream ss = evalUnbounded (return [1]) ss
evalIterator e ss = do
    sc <- getStreamContext
    pc <- getPrintContext
    setPrintContext (PrintContext False)
    inStream <- evalStreamExpr e
    out <- iterateOver inStream ss
    setStreamContext sc
    setPrintContext pc
    return out
    where
        iterateOver :: [[Int]] -> [Statement] -> TroutState [[Int]]
        iterateOver [] _ = return []
        iterateOver (f:fs) stmts = do
            setStreamContext $ StreamContext
                (IterationFrame f, empty)
            step <- iterationStep ss
            term <- iterationTerminated
            if term
                then return [step]
                else do
                    remainingSteps <- iterateOver fs stmts
                    return $ step : remainingSteps

evalUnbounded :: TroutState [Int] -> [Statement] -> TroutState [[Int]]
evalUnbounded source ss = do
    pc <- getPrintContext
    sc <- getStreamContext
    setPrintContext (PrintContext False)
    inFrame <- source
    setStreamContext $ StreamContext
        (IterationFrame inFrame, empty)
    outFrame <- iterationStep ss
    setPrintContext pc
    when (outFrame /= []) $ troutPrint (FrameVal outFrame)
    term <- iterationTerminated
    if term
        then do
            setStreamContext sc
            setPrintContext (PrintContext False)
            return [outFrame]
        else do
            remaining <- evalIterator InputStream ss
            setStreamContext sc
            setPrintContext (PrintContext False)
            return $ outFrame : remaining

iterationTerminated :: TroutState Bool
iterationTerminated = do
    (StreamContext (i, _)) <- getStreamContext
    if i == BrokenStream
        then return True
        else return False

iterationStep :: [Statement] -> TroutState [Int]
iterationStep [] = troutGetOutputFrame
iterationStep (Break : _) = do
    breakStream
    troutGetOutputFrame
iterationStep (ConditionalIf bexpr Break : ss) = do
    pc <- getPrintContext
    setPrintContext (PrintContext False)
    bresult <- evalBoolExpr bexpr
    setPrintContext pc
    if bresult
        then iterationStep (Break : ss)
        else iterationStep ss
iterationStep (s:ss) = do
    evalStatement s
    iterationStep ss

-- Utility functions

wrapStream :: VarValue -> VarValue
wrapStream (IntVal v) = StreamVal [[v]]
wrapStream (FrameVal v) = StreamVal [v]
wrapStream v = v

wrapFrame :: VarValue -> VarValue
wrapFrame (IntVal v) = FrameVal [v]
wrapFrame (FrameVal v) = FrameVal v
wrapFrame (StreamVal [v]) = FrameVal v
wrapFrame _ = error "Cannot cast a non-trivial Stream as a Frame."

wrapInt :: VarValue -> VarValue
wrapInt (IntVal v) = IntVal v
wrapInt (FrameVal [v]) = IntVal v
wrapInt (StreamVal [[v]]) = IntVal v
wrapInt _ = error "Cannot cast a non-trivial Stream or Frame as an Int."

castVExpr :: Expr -> Maybe Expr
castVExpr (SExpr (StreamIdentifier i)) = Just $ VExpr i
castVExpr (SExpr (Stream [f])) = castVExpr $ FExpr f
castVExpr (FExpr (FrameIdentifier i)) = Just $ VExpr i
castVExpr (FExpr (Frame [i])) = castVExpr $ IExpr i
castVExpr (IExpr (IntIdentifier i)) = Just $ VExpr i
castVExpr _ = Nothing
