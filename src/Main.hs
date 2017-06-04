{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Cont

import IR
import Codegen

ast = Add (Add (Float32 1.0) (Float32 2.0)) (Sub (Float32 5.0) (Float32 3.0))

main :: IO ()
main = evalCodegen (codegen "assembly" (genExpr ast >> return ()))
