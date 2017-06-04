module IR where

data Expr =
    Int32 Integer
  | Float32 Float
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show, Eq)
