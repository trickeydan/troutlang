module Language.Trout.Interpreter.Store where

type Var = (String, Int)
type VarStorage = [Var]

newtype TroutStore = TroutStore VarStorage deriving Show

-- TroutStore functions

getVarFromStore :: TroutStore -> String -> Int
getVarFromStore (TroutStore state) = getVarInVarStore state

setVarInStore :: TroutStore -> String -> Int -> TroutStore
setVarInStore (TroutStore state) name value = TroutStore newstore
    where 
        newstore = updateVarInVarStore state name value False []

-- VarStorage Functions

getVarInVarStore :: VarStorage -> String -> Int
getVarInVarStore [] _ = error "FUCK"
getVarInVarStore (x:xs) varname
    | fst x == varname = snd x
    | otherwise = getVarInVarStore xs varname

updateVarInVarStore :: VarStorage -> String -> Int -> Bool -> VarStorage -> VarStorage
updateVarInVarStore (x:xs) name value found new
    | fst x == name = updateVarInVarStore xs name value True newVarStorage
    | otherwise = updateVarInVarStore xs name value found new
    where
        newVarStorage = (name, value):new
updateVarInVarStore [] name value found new
    | found = new
    | otherwise = newVarStorage
    where
        newVarStorage = (name, value):new