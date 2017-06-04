module Main where

import Control.Monad.Cont

import IR
import Codegen

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as S

fadd = Function {
    fName = "fadd"
  , fReturnType = TFloat32
  , fParams = [Param "x" TFloat32, Param "y" TFloat32]
  , fExpr = Add (Add (Float32 1.0) (Float32 2.0)) (Sub (Float32 5.0) (Float32 3.0))
  }
iadd = Function {
    fName = "iadd"
  , fReturnType = TInt32
  , fParams = [Param "x" TInt32, Param "y" TInt32]
  , fExpr = Add (Ref TInt32 "x") (Sub (Ref TInt32 "y") (Int32 3))
  }
inc = Function {
    fName = "inc"
  , fReturnType = TInt32
  , fParams = [Param "x" TInt32]
  , fExpr = Add (Ref TInt32 "x") (Int32 1)
  }
doubleInc = Function {
    fName = "doubleInc"
  , fReturnType = TInt32
  , fParams = [Param "x" TInt32]
  , fExpr = Call "inc" TInt32 [TInt32] [Call "inc" TInt32 [TInt32] [Ref TInt32 "x"]]
  }
mainFunc = Function {
    fName = "main"
  , fReturnType = TInt32
  , fParams = []
  , fExpr = Call "doubleInc" TInt32 [TInt32] [Int32 (-2)]
  }

irModule = Module { mName = "assembly", mSource = "source.silver", mFunctions = [fadd, iadd, inc, doubleInc, mainFunc] }

-- TODO IR Ref should check function params if it exists
-- TODO execution engine
-- TODO main function
-- TODO all integer types
-- TODO string type
-- TODO function call

main :: IO ()
main = do
  result <- codegen irModule
  either print return result
