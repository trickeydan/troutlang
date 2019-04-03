module Language.Trout.Interpreter.State where

import Control.Monad.State
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.IO
import Language.Trout.Grammar
import Data.Text

type TroutState a = StateT (StreamBuffer, StreamContext, PrintContext, TroutStore) IO a

newtype StreamContext = StreamContext (Maybe StreamExpr)
newtype PrintContext = PrintContext Bool

blank :: (StreamBuffer, StreamContext, PrintContext, TroutStore)
blank = ((InBuffer [], OutBuffer []), StreamContext Nothing, PrintContext False, TroutStore [])

troutPrint :: Show a => a -> TroutState ()
troutPrint t = do
    (buffer, context, pc, store) <- get
    buffer' <- liftIO . printToBuffer buffer . pack . show $ t
    put (buffer', context, pc, store)

-- Data Things

troutDumpState :: TroutState ()
troutDumpState = do
    (_, _, _, tstate) <- get
    troutPrint tstate

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