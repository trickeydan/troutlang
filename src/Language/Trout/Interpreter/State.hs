module Language.Trout.Interpreter.State where

import Control.Monad.State
import Language.Trout.Interpreter.Store

type TroutState a = StateT TroutStore IO a

troutPrint :: Show a => a -> TroutState ()
troutPrint = liftIO . print