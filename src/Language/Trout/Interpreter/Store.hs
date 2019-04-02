module Language.Trout.Interpreter.Store where

-- Everything in this file should be pure.

data VarType = StreamType | FrameType | IntType deriving Show
data VarValue = StreamVal [[Int]] | FrameVal [Int] | IntVal Int

type StoreEntry = (String, VarValue)
newtype TroutStore = TroutStore [StoreEntry] deriving Show

showFrame :: [Int] -> String
showFrame [] = ""
showFrame (x:xs) = show x ++ " " ++ showFrame xs

instance Show VarValue where
    show (IntVal val) = show val
    show (FrameVal xs) = showFrame xs
    show (StreamVal []) = ""
    show (StreamVal (x:xs)) = showFrame x ++ "\n" ++ show (StreamVal xs)

getVar :: TroutStore -> String -> VarType -> VarValue
getVar (TroutStore store) = getValList store
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

-- TODO: Reduce duplicated code.
getVarAny :: TroutStore -> String -> VarValue
getVarAny (TroutStore store) = getValList store
    where
        getValList :: [StoreEntry] -> String -> VarValue
        getValList [] n = error ("Undefined variable: " ++ n)
        getValList (x:xs) n
            | fst x == n = snd x
            | otherwise = getValList xs n


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
