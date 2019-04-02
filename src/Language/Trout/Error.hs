module Language.Trout.Error where

import Control.Monad.State
import System.Exit
import System.Console.ANSI
import Language.Trout.Interpreter.State
import Text.Megaparsec(errorBundlePretty, Stream, ShowErrorComponent, ParseErrorBundle)

notImplemented :: String -> TroutState ()
notImplemented m = liftIO $ troutError $ "Not Implemented: " ++ m

typeError :: String -> TroutState ()
typeError m = liftIO $ troutError $ "TypeError: " ++ m

troutError:: String -> IO ()
troutError message = do
    setSGR[SetColor Foreground Vivid Red]
    putStrLn message
    setSGR[Reset]
    exitFailure

syntaxError :: (Stream s, ShowErrorComponent e) => ParseErrorBundle s e -> IO ()
syntaxError = troutError . errorBundlePretty