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
    reduceIntExpr (IntPositive e) = reduceIntExpr e
    reduceIntExpr (IntNegative e) = - reduceIntExpr e
    reduceIntExpr (IntAdd expr1 expr2) = reduceIntExpr expr1 + reduceIntExpr expr2
    reduceIntExpr (IntSubtract expr1 expr2) = reduceIntExpr expr1 - reduceIntExpr expr2
    reduceIntExpr (IntDivide expr1 expr2) = reduceIntExpr expr1 `div` reduceIntExpr expr2
    reduceIntExpr (IntMultiply expr1 expr2) = reduceIntExpr expr1 * reduceIntExpr expr2

    reduceIdentifier :: Identifier -> Int
    reduceIdentifier (Variable _) = 0 -- Requires state
    reduceIdentifier (InputIndex _) = 0 -- Requires state
    reduceIdentifier (ReturnIndex _) = 0 -- Requires state
