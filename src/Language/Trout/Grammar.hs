module Language.Trout.Grammar where

data StreamExpr = 
  Stream [FrameExpr]
  | AppendStream StreamExpr StreamExpr
  deriving(Eq, Show)

data FrameExpr =
  Frame [IntExpr]
  | AppendFrame FrameExpr FrameExpr
  deriving(Eq, Show)

data IntExpr =
  IntNum Int
  | IntIdentifier Identifier
  | IntAdd IntExpr IntExpr
  | IntSubtract IntExpr IntExpr
  | IntDivide IntExpr IntExpr
  | IntMultiply IntExpr IntExpr
  deriving(Eq, Show)

data Identifier =
  Variable String
  | InputIndex IntExpr
  | ReturnIndex IntExpr
  deriving(Eq, Show)

data Condition =
  Equals IntExpr IntExpr
  | NotEquals IntExpr IntExpr
  deriving(Eq, Show)

data Statement = 
  Iterator Identifier [Statement]
  | Assignment Identifier IntExpr
  | ConditionalIf Condition Statement
  | PrintExpr IntExpr
  | PrintIdentifier Identifier
  deriving(Eq, Show)

type Program = [Statement]
