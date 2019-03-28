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

data Expr =
  SExpr StreamExpr
  | FExpr FrameExpr
  | IExpr IntExpr
  | VExpr Identifier
  deriving(Eq, Show)

data Condition =
  Equals Expr Expr
  | NotEquals Expr Expr
  deriving(Eq, Show)

data Statement = 
  Assignment Identifier Expr
  | ConditionalIf Condition Statement
  | Print Expr
  deriving(Eq, Show)

type Program = [Statement]
