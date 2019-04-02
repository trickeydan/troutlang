module Language.Trout.Interpreter.State where

import Control.Monad.State
import Language.Trout.Interpreter.Store
import Language.Trout.Interpreter.IO
import Data.Text

type TroutState a = StateT (StreamBuffer, TroutStore) IO a

troutPrint :: Show a => a -> TroutState ()
troutPrint t = do
    (buffer, store) <- get
    buffer' <- liftIO . (printToBuffer buffer) . pack . show $ t
    put (buffer', store)

-- Data Things

troutDumpState :: TroutState ()
troutDumpState = do
    (_, tstate) <- get
    troutPrint tstate

troutSetVar :: String -> VarValue -> TroutState ()
troutSetVar name value = do
    (a, beforeStore) <- get
    put (a, setVar beforeStore name value)

troutGetVar :: String -> VarType -> TroutState VarValue
troutGetVar name vartype = do
    (_, store) <- get
    let val = getVar store name vartype
    return val

troutGetVarAny :: String -> TroutState VarValue
troutGetVarAny name = do
    (_, store) <- get
    let val = getVarAny store name
    return val

-- Deprecate me
troutGetIntFromVarValue:: VarValue -> Int
troutGetIntFromVarValue (IntVal val) = val
troutGetIntFromVarValue _ = error "Expected Type Int"

troutGetFrameFromVarValue:: VarValue -> [Int]
troutGetFrameFromVarValue (FrameVal val) = val
troutGetFrameFromVarValue _ = error "Expected Type Frame"