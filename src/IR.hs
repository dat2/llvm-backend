module IR where

data Expr =
    ConstInt Integer
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show, Eq)

data FloatExpr =
    ConstFloat Float
  | FAdd FloatExpr FloatExpr
  | FSub FloatExpr FloatExpr
  deriving (Show, Eq)
