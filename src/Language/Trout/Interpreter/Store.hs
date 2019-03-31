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
        getValList [] name _ = error ("Undefined variable: " ++ name)
        getValList (x:xs) name vartype
            | fst x == name = confirmType vartype (snd x) 
            | otherwise = getValList xs name vartype
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
        setValList [] name value found new
            | found = new
            | otherwise = newVarStorage
            where
                -- Note: This does not do a type check before setting the variable.
                newVarStorage = (name, value):new
        setValList (x:xs) name value found new
            | fst x == name = setValList xs name value True newVarStorage
            | otherwise = setValList xs name value found new
            where
                -- Note: This does not do a type check before setting the variable.
                newVarStorage = (name, value):new
