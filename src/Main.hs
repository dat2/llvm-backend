{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Cont

import IR
import Codegen

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as S

intExpr = Add (Add (Int32 1) (Int32 2)) (Sub (Int32 5) (Int32 3))
floatExpr = Add (Add (Float32 1.0) (Float32 2.0)) (Sub (Float32 5.0) (Float32 3.0))

intParamX = Param { pName = "x", pType = TInt32 }
intParamY = Param { pName = "y", pType = TInt32 }

floatParamX = Param { pName = "x", pType = TFloat32 }
floatParamY = Param { pName = "y", pType = TFloat32 }

intFunc = Function { fName = "iadd", fReturnType = TInt32, fParams = [intParamX, intParamY], fExpr = intExpr }
floatFunc = Function { fName = "fadd", fReturnType = TFloat32, fParams = [floatParamX, floatParamY], fExpr = floatExpr }

irModule = Module { mName = "assembly", mSource = "source.silver", mFunctions = [floatFunc, intFunc] }

main :: IO ()
main = do
  result <- codegen irModule
  either print return result
