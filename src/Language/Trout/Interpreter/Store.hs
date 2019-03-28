module Language.Trout.Interpreter.Store where

-- Everything in this file should be pure.

data VarType = StreamType | FrameType | IntType deriving Show
data VarValue = StreamVal [[Int]] | FrameVal [Int] | IntVal Int deriving Show

type StoreEntry = (String, VarValue)
newtype TroutStore = TroutStore [StoreEntry] deriving Show

getVar :: TroutStore -> String -> VarType -> VarValue
getVar (TroutStore store) name vartype = getValFromList store name vartype
    where
        getValFromList :: [StoreEntry] -> String -> VarType -> VarValue
        getValFromList [] name _ = error ("Undefined variable: " ++ name)
        getValFromList (x:xs) name vartype
            | fst x == name = confirmType vartype (snd x) 
            | otherwise = getValFromList xs name vartype
            where
                confirmType :: VarType -> VarValue -> VarValue
                confirmType StreamType (StreamVal s) = StreamVal s
                confirmType FrameType (FrameVal s) = FrameVal s
                confirmType IntType (IntVal s) = IntVal s
                confirmType expected actual = error ("Type mismatch: expecting " ++ show expected ++ "but got " ++ show actual)

setVar :: TroutStore -> String -> VarValue -> TroutStore
setVar e _ _ = e




-- TroutStore functions

-- setVarInStore :: TroutStore -> String -> Int -> TroutStore
-- setVarInStore (TroutStore state) name value = TroutStore newstore
--     where 
--         newstore = updateVarInVarStore state name value False []


-- updateVarInVarStore :: VarStorage -> String -> Int -> Bool -> VarStorage -> VarStorage
-- updateVarInVarStore (x:xs) name value found new
--     | fst x == name = updateVarInVarStore xs name value True newVarStorage
--     | otherwise = updateVarInVarStore xs name value found new
--     where
--         newVarStorage = (name, value):new
-- updateVarInVarStore [] name value found new
--     | found = new
--     | otherwise = newVarStorage
--     where
--         newVarStorage = (name, value):new