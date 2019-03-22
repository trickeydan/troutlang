module SchemaTemp where

data InfixOperator
  = AppendStream
  | IntAdd
  | IntSubstract
  | IntDivide
  | IntMultiply
  | AssignmentOperator
  | Equals
  | NotEquals
  deriving (Show)

data Identifier
  = Identifier String
  | InputIndex
  | OutputIndex

newtype InputIndex =
  InputIndex Int

newtype OutputIndex =
  OutputIndex Int

data Statement
  = Assignment
  | NumExpr
  | StatementGroup
  deriving (Show)

data StatementGroup =
  StatementGroup [Statement]
  deriving (Show)

data Assignment = Assignment
  { assignmentIdentifier :: Identifier
  , assignmentExpr :: NumExpr
  } deriving (Show)

data Iterator = Iterator
  { iteratorIdentifier :: Identifier
  , iteratorStatement :: Statement
  } deriving (Show)

-- Currently only used in Conditionals
data BExpr =
  BExpr

data Conditional = Conditional
  { condition :: BExpr
  , statement :: Statement
  }
