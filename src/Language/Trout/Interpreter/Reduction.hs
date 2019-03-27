module Language.Trout.Interpreter.Reduction 
(
    reduceIntExpr,
    reduceIdentifier
)
where

    import Language.Trout.Grammar
    import Language.Trout.Interpreter.State

    reduceIntExpr :: IntExpr -> TroutState Int
    reduceIntExpr (IntNum n) = return n
    reduceIntExpr (IntIdentifier ident) = reduceIdentifier ident
    reduceIntExpr (IntAdd expr1 expr2) = do
        val1 <- reduceIntExpr expr1
        val2 <- reduceIntExpr expr2
        return (val1 + val2)
    reduceIntExpr (IntSubtract expr1 expr2) = do
        val1 <- reduceIntExpr expr1
        val2 <- reduceIntExpr expr2
        return (val1 - val2)
    reduceIntExpr (IntDivide expr1 expr2) = do
        val1 <- reduceIntExpr expr1
        val2 <- reduceIntExpr expr2
        return (val1 `div` val2)
    reduceIntExpr (IntMultiply expr1 expr2) = do
        val1 <- reduceIntExpr expr1
        val2 <- reduceIntExpr expr2
        return (val1 * val2)

    reduceIdentifier :: Identifier -> TroutState Int
    reduceIdentifier (Variable name) = do
        val <- troutGetVar name
        return val
    reduceIdentifier (InputIndex _) = return 0 -- Requires state
    reduceIdentifier (ReturnIndex _) = return 0 -- Requires state
