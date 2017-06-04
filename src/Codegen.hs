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

genExpr :: I.Expr -> Codegen a (Operand, Type)
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
