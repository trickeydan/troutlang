module Language.Trout.Interpreter.Store where



-- Everything in this file should be pure.

data VarType = StreamType | FrameType | IntType deriving Show
data VarValue = StreamVal [[Int]] | FrameVal [Int] | IntVal Int deriving Show

type StoreEntry = (String, VarValue)
newtype TroutStore = TroutStore [StoreEntry] deriving Show

getVar :: TroutStore -> String -> VarType -> VarValue
getVar (TroutStore store) name vartype = getValList store name vartype
    where
        getValList :: [StoreEntry] -> String -> VarType -> VarValue
        getValList [] n _ = error ("Undefined variable: " ++ n)
        getValList (x:xs) n vt
            | fst x == n = confirmType vt (snd x) 
            | otherwise = getValList xs n vt
            where
                confirmType :: VarType -> VarValue -> VarValue
                confirmType StreamType (StreamVal s) = StreamVal s
                confirmType FrameType (FrameVal s) = FrameVal s
                confirmType IntType (IntVal s) = IntVal s
                confirmType expected actual = error ("Type mismatch: expecting " ++ show expected ++ " but got " ++ show actual)

setVar :: TroutStore -> String -> VarValue -> TroutStore
setVar (TroutStore store) name value = TroutStore $ setValList store name value False []
    where
        setValList :: [StoreEntry] -> String -> VarValue -> Bool -> [StoreEntry] -> [StoreEntry]
        setValList [] n v found new
            | found = new
            | otherwise = newVarStorage
            where
                -- Note: This does not do a type check before setting the variable.
                newVarStorage = (n, v):new
        setValList (x:xs) nam val found new
            | fst x == nam = setValList xs nam val True newVarStorage
            | otherwise = setValList xs nam val found new
            where
                -- Note: This does not do a type check before setting the variable.
                newVarStorage = (nam, val):new
