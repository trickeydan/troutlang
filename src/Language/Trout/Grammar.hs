module Language.Trout.Grammar
  (
  ) where

data InfixOperator = 
    AppendStream -- ++
  | AppendFrame -- , 
  | IntAdd -- +
  | IntSubtract -- -
  | IntDivide -- /
  | IntMultiply -- *
  | AssignmentOperator -- =
  | Equals -- ==
  | NotEquals -- !=
  deriving (Show)

data Identifier =
   Variable String -- bees
  | InputIndex Int -- [0]
  | OutputIndex Int -- <0>

data Expr =
    Int
  | IntAdd Expr Expr
  | IntSubtract Expr Expr
  | IntDivide Expr Expr
  | IntMultiply Expr Expr

-- Something that can be evaluated.
data Condition =
    Equality Int Int
  | NotEqual Int Int

data Statement = 
    Iterator { 
      iteratorIdentifier :: Identifier,
      iteratorStatementList :: [Statement] 
    }
  | Assignment {
      assignmentIdentifier :: Identifier,
      assignmentExpr :: Expr
    }
  | ConditionalIf {
      condition :: Condition,
      statement :: Statement
    }
