module Language.Trout.Error where

import Control.Monad.State
import System.Exit
import System.Console.ANSI
import Language.Trout.Interpreter.State
import Text.Megaparsec(errorBundlePretty, Stream, ShowErrorComponent, ParseErrorBundle)

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

syntaxError :: (Stream s, ShowErrorComponent e) => ParseErrorBundle s e -> IO ()
syntaxError bundle = do
    setSGR[SetColor Foreground Vivid Red]
    putStrLn "Error during parsing:"
    putStrLn $ errorBundlePretty bundle
    setSGR[Reset]
    exitFailure