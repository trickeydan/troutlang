module Language.Trout.Error where

import Control.Monad.State
import System.Exit
import System.Console.ANSI
import Language.Trout.Interpreter.State

notImplemented :: String -> TroutState ()
notImplemented m = troutError $ "Not Implemented: " ++ m

typeError :: String -> TroutState ()
typeError m = troutError $ "TypeError: " ++ m

troutError:: String -> TroutState ()
troutError message = do
    liftIO $ setSGR[SetColor Foreground Vivid Red]
    troutPrint $ message
    liftIO $ setSGR[Reset]
    troutPrint "Execution terminated."
    liftIO $ exitFailure