module IR where

import Data.ByteString.Short (ShortByteString)


data Expr =
    Int32 Integer
  | Float32 Float
  | Add Expr Expr
  | Sub Expr Expr
  deriving (Show, Eq)

data Type =
    TInt32
  | TFloat32
  deriving (Show, Eq)

data Param =
    Param { pName :: ShortByteString, pType :: Type }
  deriving (Show, Eq)

data Function =
    Function { fName :: ShortByteString, fReturnType :: Type, fParams :: [Param], fExpr :: Expr }
  deriving (Show, Eq)
