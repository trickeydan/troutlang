module Language.Trout.Interpreter.State where

type Var = (String, Int)
type VarStorage = [Var]

newtype TroutState = TroutState VarStorage


-- TroutState functions

getVarState :: TroutState -> String -> Maybe Int
getVarState (TroutState state) = getVarInStore state

setVarState :: TroutState -> String -> Int -> TroutState
setVarState (TroutState state) name value = TroutState newstore
    where 
        newstore = updateVarInStore state name value False []

-- VarStorage Functions

getVarInStore :: VarStorage -> String -> Maybe Int
getVarInStore [] _ = Nothing
getVarInStore (x:xs) varname
    | fst x == varname = Just (snd x)
    | otherwise = getVarInStore xs varname

updateVarInStore :: VarStorage -> String -> Int -> Bool -> VarStorage -> VarStorage
updateVarInStore (x:xs) name value found new
    | fst x == name = updateVarInStore xs name value True newVarStorage
    | otherwise = updateVarInStore xs name value False new
    where
        newVarStorage = (name, value):new
updateVarInStore [] name value found new
    | found = new
    | otherwise = newVarStorage
    where
        newVarStorage = (name, value):new