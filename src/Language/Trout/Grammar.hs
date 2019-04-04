module Language.Trout.Grammar where

data StreamExpr = 
  Stream [FrameExpr]
  | InputStream
  | InfiniteStream
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

data BoolExpr =
  Boolean Bool
  | Equals Expr Expr
  | NotEquals Expr Expr
  | LessThan Expr Expr
  | GreaterThan Expr Expr
  | Or BoolExpr BoolExpr
  | And BoolExpr BoolExpr
  | Not BoolExpr
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

data Statement = 
  Assignment Identifier Expr
  | NullAssignment Expr
  | ConditionalIf BoolExpr Statement
  | Print Expr
  | Break
  deriving(Eq, Show)

type Program = [Statement]
