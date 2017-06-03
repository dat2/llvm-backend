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
import LLVM.AST.Constant (Constant(Int))
import LLVM.AST.Instruction
import LLVM.AST.Global
import LLVM.AST.Linkage
import LLVM.AST.Name
import LLVM.AST.Operand (Operand(ConstantOperand, LocalReference))
import LLVM.AST.Type (Type(IntegerType))

import qualified IR as I
import qualified LLVM.Module as M
import qualified LLVM.AST as A

-- types
int32Type :: Type
int32Type = IntegerType 32

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
add :: Operand -> Operand -> Instruction
add a b = Add False False a b []

sub :: Operand -> Operand -> Instruction
sub a b = Sub False False a b []

-- terminators
ret :: Operand -> Named Terminator
ret o = Do $ Ret (Just o) []

-- operands
ref :: Name -> Type -> Operand
ref n t = LocalReference t n

int32 :: Integer -> Operand
int32 = ConstantOperand . Int 32

-- create a fake module
astModule :: ShortByteString -> ShortByteString -> [Named Instruction] -> Word -> A.Module
astModule source name instructions result = A.defaultModule {
    A.moduleName = name
  , A.moduleSourceFileName = source
  , A.moduleDefinitions = [ func int32Type "main" [ block "entry" instructions (ret (ref (UnName result) int32Type)) ] ]
  }

-- monad
data CodegenState = CodegenState { codegenId :: Word, codegenInstructions :: [Named Instruction] }
  deriving (Show, Eq)

initialState :: CodegenState
initialState = CodegenState { codegenId = 0, codegenInstructions = [] }

newtype Codegen a b = Codegen { runCodegen :: StateT CodegenState (ContT a IO) b }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadCont, MonadIO)

evalCodegen :: Codegen () () -> IO ()
evalCodegen = (flip runContT) return . (flip evalStateT) initialState . runCodegen

liftCont :: ContT a IO b -> Codegen a b
liftCont cont = Codegen $ StateT $ \state -> do
  r <- cont
  return (r, state)

nextId :: Codegen a Word
nextId = do
  id <- gets codegenId
  modify $ \state -> state { codegenId = id + 1 }
  return id

append :: Named Instruction -> Codegen a ()
append i = do
  instructions <- gets codegenInstructions
  modify $ \state -> state { codegenInstructions = i : instructions }

genExpr :: I.Expr -> Codegen a Operand
genExpr (I.Const i) = return $ int32 i
genExpr (I.Add a b) = do
  a_op <- genExpr a
  b_op <- genExpr b
  id <- nextId
  append $ UnName id := add a_op b_op
  return $ LocalReference int32Type (UnName id)
genExpr (I.Sub a b) = do
  a_op <- genExpr a
  b_op <- genExpr b
  id <- nextId
  append $ UnName id := sub a_op b_op
  return $ LocalReference int32Type (UnName id)

context :: ContT a IO Context
context = ContT withContext

makeModule :: Context -> A.Module -> ContT a IO M.Module
makeModule c m = ContT $ M.withModuleFromAST c m

hostTargetMachine :: ContT a IO TargetMachine
hostTargetMachine = ContT withHostTargetMachine

codegen :: I.Expr -> Codegen a ()
codegen expr = do
  genExpr expr
  instructions <- gets codegenInstructions
  id <- gets codegenId

  -- make a context
  ctx <- liftCont context

  let mod = astModule "source.silver" "assembly.ll" instructions (id - 1)
  llModule <- liftCont $ makeModule ctx mod

  -- check that it works
  liftIO $ verify llModule
  liftIO $ M.writeLLVMAssemblyToFile (M.File "assembly.ll") llModule

  -- create an object file
  target <- liftCont $ hostTargetMachine
  liftIO $ M.writeObjectToFile target (M.File "assembly.o") llModule

  -- link the object file
  liftIO $ callCommand "ld -macosx_version_min 10.12 -arch x86_64 -lSystem -o assembly assembly.o"
