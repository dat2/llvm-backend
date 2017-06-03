module IR where

data Expr =
    Const Integer
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show, Eq)
