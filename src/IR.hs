module IR where

data Expr
  = Int32 Integer
  | Float32 Float
  | Add Expr
        Expr
  | Sub Expr
        Expr
  | Ref Type
        String
  | Call String
         Type
         [Type]
         [Expr]
  deriving (Show, Eq)

data Type
  = TInt32
  | TFloat32
  deriving (Show, Eq)

data Param = Param
  { pName :: String
  , pType :: Type
  } deriving (Show, Eq)

data Function = Function
  { fName :: String
  , fReturnType :: Type
  , fParams :: [Param]
  , fExpr :: Expr
  } deriving (Show, Eq)

data Module = Module
  { mName :: String
  , mSource :: String
  , mFunctions :: [Function]
  } deriving (Show, Eq)
