module Language.Trout.Interpreter.State where

type Var = (String, Int)
type VarStorage = [Var]

newtype TroutState = TroutState VarStorage


-- TroutState functions

getVarState :: TroutState -> String -> Maybe Int
getVarState (TroutState state) = getVarInStore state

setVarState :: TroutState -> String -> Int -> TroutState
setVarState _ _ _ = TroutState []

-- VarStorage Functions

getVarInStore :: VarStorage -> String -> Maybe Int
getVarInStore [] _ = Nothing
getVarInStore (x:xs) varname
    | fst x == varname = Just (snd x)
    | otherwise = getVarInStore xs varname

updateVarInStore :: VarStorage -> String -> Int -> VarStorage -> VarStorage
updateVarInStore (x:xs) name value new
    | fst x == name = updateVarInStore xs name value newVarStorage
    | otherwise = updateVarInStore xs name value new
    where
        newVarStorage = (name, value):new
updateVarInStore [] _ _ new = new