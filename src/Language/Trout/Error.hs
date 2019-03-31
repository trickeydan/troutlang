module Language.Trout.Error where

import Control.Monad.State
import System.Exit
import System.Console.ANSI
import Language.Trout.Interpreter.State

notImplemented :: String -> TroutState ()
notImplemented message = do
    liftIO $ setSGR[SetColor Foreground Vivid Red]
    troutPrint $ id $ "Not Implemented: " ++ message
    liftIO $ setSGR[Reset]
    liftIO $ exitFailure
