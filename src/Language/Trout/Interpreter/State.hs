module Language.Trout.Interpreter.State where

import Control.Monad.State
import Control.Monad(when)
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.IO
import Language.Trout.Grammar
import Language.Trout.Parser(stdInputFrameExpr)
import Text.Megaparsec(runParser)
import Data.Text hiding (empty, map, takeWhile)
import Data.HashMap.Strict hiding (map)
import Prelude hiding (lookup)

-- In future, refactor to use record syntax.
type TroutState a = StateT (StreamBuffer, StreamContext, PrintContext, TroutStore) IO a

newtype StreamContext = StreamContext (IterableStream, HashMap Int Int)
data IterableStream =
    BlankStream
    | IterationFrame [Int]
    deriving(Eq, Show)
newtype PrintContext = PrintContext Bool

isBlank :: IterableStream -> Bool
isBlank BlankStream = True
isBlank _ = False

blank :: (StreamBuffer, StreamContext, PrintContext, TroutStore)
blank = (
        (InBuffer [], OutBuffer []),
        StreamContext (BlankStream, empty),
        PrintContext False,
        TroutStore []
    )

troutPrint :: VarValue -> TroutState ()
troutPrint (StreamVal []) = return ()
troutPrint (StreamVal (x:xs)) = do
    troutPrint (FrameVal x)
    troutPrint (StreamVal xs)
troutPrint t = do
    (buffer, sc, PrintContext pc, store) <- get
    when pc $ do
        buffer' <- liftIO . printToBuffer buffer . pack . show $ t
        put (buffer', sc, PrintContext pc, store)

troutRead :: TroutState [Int]
troutRead = do
    (buffer, sc, pc, store) <- get
    (buffer', txt) <- liftIO $ extractLatestInput buffer
    let f = extract $ runParser stdInputFrameExpr "stdin" txt
    put (buffer', sc, pc, store)
    return $ unwrap f
    where
        extract (Left _) = error "Error parsing standard input"
        extract (Right a) = a
        unwrap (Frame []) = []
        unwrap (Frame ((IntNum i):xs)) = i : unwrap (Frame xs)
        unwrap _ = error "Non-integer in standard input."

troutSetIndex :: Int -> Int -> TroutState ()
troutSetIndex i e = do
    (b, StreamContext (str, hm), pc, s) <- get
    if
        isBlank str
    then
        error "Cannot set stream index outside iterator."
    else
        put (b, StreamContext (str, insert i e hm), pc, s)

troutGetIndex :: Int -> TroutState Int
troutGetIndex i = do
    (_, StreamContext (str, hm), _, _) <- get
    let v = lookup i hm
    if
        isBlank str
    then
        error "Cannot read stream index outside iterator."
    else
        output v
    where
        output Nothing = error "Empty index accessed."
        output (Just x) = return x

troutGetOutputFrame :: TroutState [Int]
troutGetOutputFrame = do
    (_, StreamContext (str, hm), _, _) <- get
    if
        isBlank str
    then
        error "Cannot output iterator frame outside iterator."
    else
        return $ chopMaybes $ map (`lookup` hm) [0..]
    where
        chopMaybes :: [Maybe Int] -> [Int]
        chopMaybes [] = []
        chopMaybes (Just x : xs) = x : chopMaybes xs
        chopMaybes (Nothing : _) = []

setPrintContext :: PrintContext -> TroutState ()
setPrintContext context = do
    (b, sc, _, s) <- get
    put (b, sc, context, s)

getPrintContext :: TroutState PrintContext
getPrintContext = do
    (_, _, pc, _) <- get
    return pc
    
setStreamContext :: StreamContext -> TroutState ()
setStreamContext context = do
    (b, _, pc, s) <- get
    put (b, context, pc, s)

getStreamContext :: TroutState StreamContext
getStreamContext = do
    (_, sc, _ , _) <- get
    return sc

-- Data Things

troutDumpState :: TroutState ()
troutDumpState = do
    (_, _, _, tstate) <- get
    (liftIO . print) tstate

troutSetVar :: String -> VarValue -> TroutState ()
troutSetVar name value = do
    (a, c, p, beforeStore) <- get
    put (a, c, p, setVar beforeStore name value)

troutGetVar :: String -> VarType -> TroutState VarValue
troutGetVar name vartype = do
    (_, _, _, store) <- get
    let val = getVar store name vartype
    return val

troutGetVarAny :: String -> TroutState VarValue
troutGetVarAny name = do
    (_, _, _, store) <- get
    let val = getVarAny store name
    return val

-- Deprecate me
troutGetIntFromVarValue:: VarValue -> Int
troutGetIntFromVarValue (IntVal val) = val
troutGetIntFromVarValue _ = error "Expected Type Int"

troutGetFrameFromVarValue:: VarValue -> [Int]
troutGetFrameFromVarValue (FrameVal val) = val
troutGetFrameFromVarValue _ = error "Expected Type Frame"