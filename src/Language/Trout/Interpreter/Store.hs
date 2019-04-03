module Language.Trout.Interpreter.Store where

-- Everything in this file should be pure.

data VarType = StreamType | FrameType | IntType deriving (Eq, Show)
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

instance Eq VarValue where
    IntVal a == IntVal b = a == b
    FrameVal [a] == IntVal b = a == b
    IntVal a == FrameVal [b] = a == b
    StreamVal [[a]] == IntVal b = a == b
    IntVal a == StreamVal [[b]] = a == b

    FrameVal a == FrameVal b = a == b
    StreamVal [a] == FrameVal b = a == b
    FrameVal a == StreamVal [b] = a == b

    StreamVal a == StreamVal b = a == b

    _ == _ = False

getValList :: [StoreEntry] -> (VarType -> VarValue -> VarValue) -> String -> VarType -> VarValue
getValList [] _ n _ = error ("Undefined variable: " ++ n)
getValList (x:xs) check n vt
    | fst x == n = check vt (snd x) 
    | otherwise = getValList xs check n vt

getVar :: TroutStore -> String -> VarType -> VarValue
getVar (TroutStore store) = getValList store confirmType
    where
        confirmType :: VarType -> VarValue -> VarValue
        confirmType StreamType (StreamVal s) = StreamVal s
        confirmType FrameType (FrameVal s) = FrameVal s
        confirmType IntType (IntVal s) = IntVal s
        confirmType expected actual = error ("Type mismatch: expecting " ++ show expected ++ " but got " ++ show actual)

getVarAny :: TroutStore -> String -> VarValue
getVarAny (TroutStore store) name = getValList store confirmType name StreamType -- The type is ignored here.
    where
        confirmType :: VarType -> VarValue -> VarValue
        confirmType _ = id

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
