module Language.Trout.Grammar
  (
  ) where

data InfixOperator
  = AppendStream -- ++
  | AppendFrame -- , 
  | IntAdd -- +
  | IntSubstract -- -
  | IntDivide -- /
  | IntMultiply -- *
  | AssignmentOperator -- =
  | Equals -- ==
  | NotEquals -- !=
  deriving (Show)

data Identifier
  = Variable String
  | InputIndex Int
  | OutputIndex Int

data Expr =
  Expr

data Condition
  = Equality Int
             Int
  | NotEqual Int
             Int

data Statement
  = Iterator { iteratorIdentifier :: Identifier
             , iteratorStatementList :: [Statement] }
  | Assignment { assignmentIdentifier :: Identifier
               , assignmentExpr :: Expr }
  | ConditionalIf { condition :: Condition
                  , statement :: Statement }
