{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Codegen where

import Control.Monad.Cont
import Control.Monad.State
import System.Process

import LLVM.Analysis
import LLVM.Internal.Context
import LLVM.Internal.Target

import LLVM.Prelude
import LLVM.AST (Definition(..))
import LLVM.AST.Constant (Constant(Int, Float))
import LLVM.AST.Float
import LLVM.AST.Global
import LLVM.AST.Instruction
import LLVM.AST.Linkage
import LLVM.AST.Name
import LLVM.AST.Operand
import LLVM.AST.Type

import qualified IR as I
import qualified LLVM.Module as M
import qualified LLVM.AST as A
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Short as S

-- types
int32Type :: Type
int32Type = IntegerType 32

float32Type :: Type
float32Type = FloatingPointType FloatFP

-- definitions
func :: Type -> ShortByteString -> [BasicBlock] -> Definition
func returnType name blocks = GlobalDefinition $
    functionDefaults {
      returnType = returnType
    , name = Name name
    , basicBlocks = blocks }

-- blocks
block :: ShortByteString -> [Named Instruction] -> Named Terminator -> BasicBlock
block name instructions terminator = BasicBlock (Name name) instructions terminator

-- instructions
iadd32 :: Operand -> Operand -> Instruction
iadd32 a b = Add False True a b []

fadd :: Operand -> Operand -> Instruction
fadd a b = FAdd (FastMathFlags True True True True) a b []

isub32 :: Operand -> Operand -> Instruction
isub32 a b = Sub False True a b []

fsub :: Operand -> Operand -> Instruction
fsub a b = FSub (FastMathFlags True True True True) a b []

-- terminators
ret :: Operand -> Named Terminator
ret o = Do $ Ret (Just o) []

-- operands
ref :: Name -> Type -> Operand
ref n t = LocalReference t n

int32 :: Integer -> Operand
int32 = ConstantOperand . Int 32

float32 :: Float -> Operand
float32 = ConstantOperand . Float . Single

-- create a fake module
initialModule :: ShortByteString -> ShortByteString -> A.Module
initialModule source name = A.defaultModule {
    A.moduleName = name
  , A.moduleSourceFileName = source
  , A.moduleDefinitions = [ ]
  }

-- monad
data CodegenState = CodegenState { cId :: Word, cInstructions :: [Named Instruction], cModule :: A.Module }
  deriving (Show, Eq)

initialState :: ShortByteString -> ShortByteString -> CodegenState
initialState sourceName moduleName = CodegenState { cId = 0, cInstructions = [], cModule = initialModule sourceName moduleName  }

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

evalCodegen :: ShortByteString -> ShortByteString -> Codegen () -> A.Module
evalCodegen sourceName moduleName c = cModule $ execState (runCodegen c) (initialState sourceName moduleName)

nextId :: Codegen Word
nextId = do
  id <- gets cId
  modify $ \state -> state { cId = id + 1 }
  return id

append :: Named Instruction -> Codegen ()
append i = do
  instructions <- gets cInstructions
  modify $ \state -> state { cInstructions = i : instructions }

makeFunc :: Word -> [Named Instruction] -> Definition
makeFunc returnId instructions = func float32Type "main" [ block "entry" instructions (ret (ref (UnName returnId) float32Type)) ]

addFunc :: Word -> [Named Instruction] -> Codegen ()
addFunc returnId instructions = do
  let f = makeFunc returnId instructions
  oldModule <- gets cModule
  let moduleDefinitions = A.moduleDefinitions oldModule
  let newModule = oldModule { A.moduleDefinitions = moduleDefinitions ++ [f] }

  modify $ \state -> state { cModule = newModule }

typedAdd :: Type -> Operand -> Operand -> Instruction
typedAdd t a b
  | t == int32Type = iadd32 a b
  | t == float32Type = fadd a b
  | otherwise = undefined

typedSub :: Type -> Operand -> Operand -> Instruction
typedSub t a b
  | t == int32Type = isub32 a b
  | t == float32Type = fsub a b
  | otherwise = undefined

genExpr :: I.Expr -> Codegen (Operand, Type)
genExpr (I.Int32 i) = return $ (int32 i, int32Type)
genExpr (I.Float32 f) = return $ (float32 f, float32Type)
genExpr (I.Add a b) = do
  (aOp, aType) <- genExpr a
  (bOp, bType) <- genExpr b
  -- assuming aType == bType
  id <- nextId
  append $ UnName id := typedAdd aType aOp bOp
  return $ (LocalReference aType (UnName id), aType)
genExpr (I.Sub a b) = do
  (aOp, aType) <- genExpr a
  (bOp, bType) <- genExpr b
  id <- nextId
  -- assuming aType == bType
  append $ UnName id := typedSub aType aOp bOp
  return $ (LocalReference aType (UnName id), aType)

mkAstModule :: I.Expr -> Codegen ()
mkAstModule expr = do
  genExpr expr
  instructions <- gets cInstructions
  id <- gets cId
  addFunc (id - 1) instructions

context :: ContT a IO Context
context = ContT withContext

makeModule :: Context -> A.Module -> ContT a IO M.Module
makeModule c m = ContT $ M.withModuleFromAST c m

hostTargetMachine :: ContT a IO TargetMachine
hostTargetMachine = ContT withHostTargetMachine

codegen :: A.Module -> IO ()
codegen mod = (flip runContT) return $ do

  -- make a context
  ctx <- context
  llModule <- makeModule ctx mod

  -- check that it works
  liftIO $ verify llModule
  liftIO $ M.writeLLVMAssemblyToFile (M.File "assembly.ll") llModule

  -- create an object file
  target <- hostTargetMachine
  liftIO $ M.writeObjectToFile target (M.File "assembly.o") llModule

  -- link the object file
  liftIO $ callCommand $ "ld -macosx_version_min 10.12 -arch x86_64 -lSystem -o assembly assembly.o"
