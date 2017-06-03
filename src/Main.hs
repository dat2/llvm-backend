{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Cont

import IR
import Codegen

ast :: Expr
ast = Add (Add (Const 1) (Const 2)) (Sub (Const 5) (Const 3))

main :: IO ()
main = evalCodegen (codegen ast)
