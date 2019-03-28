module Language.Trout.Interpreter.State where

import Control.Monad.State
import Language.Trout.Interpreter.Store

type TroutState a = StateT TroutStore IO a

troutPrint :: Show a => a -> TroutState ()
troutPrint = liftIO . print

-- Data Things

troutDumpState :: TroutState ()
troutDumpState = do
    tstate <- get
    troutPrint tstate

-- troutSetVar :: String -> Int -> TroutState ()
-- troutSetVar name value = do
--     before <- get
--     put $ setVarInStore before name value

-- troutGetVar :: String -> TroutState Int
-- troutGetVar name = do
--     tstate <- get
--     let val = getVarFromStore tstate name
--     return val