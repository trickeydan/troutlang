module Language.Trout.Interpreter 
(
    executeProgram
)
where

import Language.Trout.Interpreter.State
import Language.Trout.Grammar

executeProgram :: Program -> TroutState ()
executeProgram program = do
    evalProgram program
    return ()

evalProgram :: Program -> TroutState ()
evalProgram [] = troutPrint "Execution complete."
evalProgram (x:xs) = do 
    evalStatement x
    evalProgram xs

evalStatement :: Statement -> TroutState ()
evalStatement (Assignment _ _) = troutPrint "Assignment is currently unimplemented."
evalStatement (NullAssignment _) = troutPrint "Null Assignment is currently unimplemented."
evalStatement (ConditionalIf _ _ ) = troutPrint "ConditionalIf is currently unimplemented."
evalStatement (Print _) = troutPrint "Print is currently unimplemented."
evalStatement (Break) = troutPrint "Breal is currently unimplemented."
