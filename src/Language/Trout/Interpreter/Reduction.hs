module Language.Trout.Interpreter.Reduction 
(
    reduceIntExpr,
    reduceIdentifier
)
where

    import Language.Trout.Grammar

    reduceIntExpr :: IntExpr -> Int
    reduceIntExpr (IntNum n) = n
    reduceIntExpr (IntIdentifier ident) = reduceIdentifier ident
    reduceIntExpr (IntAdd expr1 expr2) = reduceIntExpr expr1 + reduceIntExpr expr2
    reduceIntExpr (IntSubtract expr1 expr2) = reduceIntExpr expr1 - reduceIntExpr expr2
    reduceIntExpr (IntDivide expr1 expr2) = reduceIntExpr expr1 `div` reduceIntExpr expr2
    reduceIntExpr (IntMultiply expr1 expr2) = reduceIntExpr expr1 * reduceIntExpr expr2

    reduceIdentifier :: Identifier -> Int
    reduceIdentifier (Variable name) = 0
    reduceIdentifier (InputIndex expr) = 0
        where
            val = reduceIntExpr expr
    reduceIdentifier (ReturnIndex expr) = 0
        where
            val = reduceIntExpr expr
        