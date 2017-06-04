{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Cont

import IR
import Codegen

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as S

ast = Add (Add (Float32 1.0) (Float32 2.0)) (Sub (Float32 5.0) (Float32 3.0))

main :: IO ()
main = do
  let llfile = "assembly.ll"
  let sourcefile = "source.silver"
  let mod = evalCodegen sourcefile llfile $ mkAstModule ast
  codegen mod
