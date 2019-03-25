module Language.Trout.Grammar where

data StreamExpr = 
  Stream [FrameExpr]
  | AppendStream StreamExpr StreamExpr

data FrameExpr =
  Frame [IntExpr]
  | AppendFrame FrameExpr FrameExpr

data IntExpr =
  IntNum Int
  | IntAdd IntExpr IntExpr
  | IntSubtract IntExpr IntExpr
  | IntDivide IntExpr IntExpr
  | IntMultiply IntExpr IntExpr

data Identifier =
  Variable String
  | InputIndex IntExpr
  | ReturnIndex IntExpr

data Condition =
  Equals IntExpr IntExpr
  | NotEquals IntExpr IntExpr

data Statement = 
  Iterator Identifier [Statement]
  | Assignment Identifier IntExpr
  | ConditionalIf Condition Statement

type Program = [Statement]
