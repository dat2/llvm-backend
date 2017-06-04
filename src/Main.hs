{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Cont

import IR
import Codegen

ast = FAdd (FAdd (ConstFloat 1.0) (ConstFloat 2.0)) (FSub (ConstFloat 5.0) (ConstFloat 3.0))

main :: IO ()
main = evalCodegen (codegen "assembly" (genFloatExpr ast >> return ()))
