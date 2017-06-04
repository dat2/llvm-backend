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
add :: Operand -> Operand -> Instruction
add a b = Add False False a b []

fadd :: Operand -> Operand -> Instruction
fadd a b = FAdd NoFastMathFlags a b []

sub :: Operand -> Operand -> Instruction
sub a b = Sub False False a b []

fsub :: Operand -> Operand -> Instruction
fsub a b = FSub NoFastMathFlags a b []

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
astModule :: ShortByteString -> ShortByteString -> [Named Instruction] -> Word -> A.Module
astModule source name instructions result = A.defaultModule {
    A.moduleName = name
  , A.moduleSourceFileName = source
  , A.moduleDefinitions = [ func float32Type "main" [ block "entry" instructions (ret (ref (UnName result) float32Type)) ] ]
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

genExpr :: I.Expr -> Codegen a (Operand, Type)
genExpr (I.ConstInt i) = return $ (int32 i, int32Type)
genExpr (I.Add a b) = do
  (a_op, a_type) <- genExpr a
  (b_op, b_type) <- genExpr b
  -- assuming a_type == b_type
  id <- nextId
  append $ UnName id := add a_op b_op
  return $ (LocalReference a_type (UnName id), a_type)
genExpr (I.Sub a b) = do
  (a_op, a_type) <- genExpr a
  (b_op, b_type) <- genExpr b
  id <- nextId
  -- assuming a_type == b_type
  append $ UnName id := sub a_op b_op
  return $ (LocalReference a_type (UnName id), a_type)

genFloatExpr :: I.FloatExpr -> Codegen a (Operand, Type)
genFloatExpr (I.ConstFloat f) = return $ (float32 f, float32Type)
genFloatExpr (I.FAdd a b) = do
  (a_op, a_type) <- genFloatExpr a
  (b_op, b_type) <- genFloatExpr b
  id <- nextId
  append $ UnName id := fadd a_op b_op
  return $ (LocalReference a_type (UnName id), a_type)
genFloatExpr (I.FSub a b) = do
  (a_op, a_type) <- genFloatExpr a
  (b_op, b_type) <- genFloatExpr b
  id <- nextId
  append $ UnName id := fsub a_op b_op
  return $ (LocalReference a_type (UnName id), a_type)

context :: ContT a IO Context
context = ContT withContext

makeModule :: Context -> A.Module -> ContT a IO M.Module
makeModule c m = ContT $ M.withModuleFromAST c m

hostTargetMachine :: ContT a IO TargetMachine
hostTargetMachine = ContT withHostTargetMachine

codegen :: String -> Codegen a () -> Codegen a ()
codegen out genBody = do
  let llfile = S.toShort $ C.pack $ out ++ ".ll"

  -- generate the expression
  genBody
  instructions <- gets codegenInstructions
  id <- gets codegenId

  let mod = astModule "source.silver" llfile instructions (id - 1)

  -- make a context
  ctx <- liftCont context
  llModule <- liftCont $ makeModule ctx mod

  -- check that it works
  liftIO $ verify llModule
  liftIO $ M.writeLLVMAssemblyToFile (M.File (out ++ ".ll")) llModule

  -- create an object file
  target <- liftCont $ hostTargetMachine
  liftIO $ M.writeObjectToFile target (M.File (out ++ ".o")) llModule

  -- link the object file
  liftIO $ callCommand $ "ld -macosx_version_min 10.12 -arch x86_64 -lSystem -o " ++ out ++ " " ++ out ++ ".o"
