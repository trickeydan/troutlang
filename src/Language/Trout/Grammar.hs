module Language.Trout.Grammar where

data StreamExpr = 
  Stream [FrameExpr]
  | StreamIdentifier Identifier
  | AppendStream StreamExpr StreamExpr
  | Iterator StreamExpr [Statement]
  deriving(Eq, Show)

data FrameExpr =
  Frame [IntExpr]
  | FrameIdentifier Identifier
  | AppendFrame FrameExpr FrameExpr
  deriving(Eq, Show)

data IntExpr =
  IntNum Int
  | IntIdentifier Identifier
  | IntPositive IntExpr
  | IntNegative IntExpr
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

data ExprPair =
  StreamPair StreamExpr StreamExpr
  | FramePair FrameExpr FrameExpr
  | IntPair IntExpr IntExpr
  | IdPair Identifier Identifier
  deriving(Eq, Show)

data Condition =
  Equals ExprPair
  | NotEquals ExprPair
  deriving(Eq, Show)

data Statement = 
  Assignment Identifier IntExpr
  | ConditionalIf Condition Statement
  | PrintStream StreamExpr
  | PrintFrame FrameExpr
  | PrintInt IntExpr
  | PrintIdentifier Identifier
  deriving(Eq, Show)

type Program = [Statement]
