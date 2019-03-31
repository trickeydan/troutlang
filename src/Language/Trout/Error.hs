module Language.Trout.Error where

import Control.Monad.State
import System.Exit
import System.Console.ANSI
import Language.Trout.Interpreter.State

-- TODO: Make these functions nicer

notImplemented :: String -> TroutState ()
notImplemented message = do
    liftIO $ setSGR[SetColor Foreground Vivid Red]
    troutPrint $ "Not Implemented: " ++ message
    liftIO $ setSGR[Reset]
    troutPrint "Execution terminated."
    liftIO $ exitFailure

typeError :: String -> TroutState ()
typeError message = do
    liftIO $ setSGR[SetColor Foreground Vivid Red]
    troutPrint $ "Type Error: " ++ message
    liftIO $ setSGR[Reset]
    troutPrint "Execution terminated."
    liftIO $ exitFailure
