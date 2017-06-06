module Main where

import           Codegen
import           IR

fadd :: Function
fadd =
  Function
  { fName = "fadd"
  , fReturnType = TFloat32
  , fParams = [Param "x" TFloat32, Param "y" TFloat32]
  , fExpr =
      Add (Add (Float32 1.0) (Float32 2.0)) (Sub (Float32 5.0) (Float32 3.0))
  }

iadd :: Function
iadd =
  Function
  { fName = "iadd"
  , fReturnType = TInt32
  , fParams = [Param "x" TInt32, Param "y" TInt32]
  , fExpr = Add (Ref TInt32 "x") (Sub (Ref TInt32 "y") (Int32 3))
  }

inc :: Function
inc =
  Function
  { fName = "inc"
  , fReturnType = TInt32
  , fParams = [Param "x" TInt32]
  , fExpr = Add (Ref TInt32 "x") (Int32 1)
  }

doubleInc :: Function
doubleInc =
  Function
  { fName = "doubleInc"
  , fReturnType = TInt32
  , fParams = [Param "x" TInt32]
  , fExpr =
      Call "inc" TInt32 [TInt32] [Call "inc" TInt32 [TInt32] [Ref TInt32 "x"]]
  }

mainFunc :: Function
mainFunc =
  Function
  { fName = "main"
  , fReturnType = TInt32
  , fParams = []
  , fExpr = Call "doubleInc" TInt32 [TInt32] [Int32 5]
  }

irModule :: Module
irModule =
  Module
  { mName = "assembly"
  , mSource = "source.silver"
  , mFunctions = [fadd, iadd, inc, doubleInc, mainFunc]
  }

-- TODO build symbol table
-- TODO execution engine
-- TODO all integer types
-- TODO string type
main :: IO ()
main
  -- result <- codegen irModule
 = do
  result <- execute irModule
  either print print result
