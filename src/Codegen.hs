{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
module Codegen where

import Control.Monad.Cont
import Control.Monad.Except
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

data CodegenError =
    MismatchType I.Expr Type I.Expr Type
  deriving (Show, Eq)

newtype Codegen a = Codegen { runCodegen :: ExceptT CodegenError (State CodegenState) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadError CodegenError)

evalCodegen :: ShortByteString -> ShortByteString -> Codegen () -> Either CodegenError A.Module
evalCodegen sourceName moduleName c =
  let
    s = (initialState sourceName moduleName)
  in
    -- runExceptT (runCodegen c) :: State CodegenState (Either CodegenError a)
    case runState (runExceptT (runCodegen c)) (initialState sourceName moduleName) of
      (Left e, _) -> Left e
      (Right _, s') -> Right (cModule s')

-- | get the next id to use for generation
nextId :: Codegen Word
nextId = do
  id <- gets cId
  modify $ \state -> state { cId = id + 1 }
  return id

-- | append an instruction to the current list of instructions
append :: Named Instruction -> Codegen ()
append i = do
  instructions <- gets cInstructions
  modify $ \state -> state { cInstructions = i : instructions }

-- | this function will throwError if the types do not match
checkType :: (I.Expr, Type) -> (I.Expr, Type) -> Codegen ()
checkType (aExpr, aType) (bExpr, bType) = do
  if aType == bType
    then return ()
    else throwError (MismatchType aExpr aType bExpr bType)

-- | create a "main" function for the given ir expression
codegenAstModule :: I.Expr -> Codegen ()
codegenAstModule expr = do
  codegenExpr expr
  instructions <- gets cInstructions
  id <- gets cId
  addFunc (id - 1) instructions

-- | given an IR expression, create the llvm instructions for it.
codegenExpr :: I.Expr -> Codegen (Operand, Type)
codegenExpr (I.Int32 i) = return $ (int32 i, int32Type)
codegenExpr (I.Float32 f) = return $ (float32 f, float32Type)
codegenExpr (I.Add a b) = do
  (aOp, aType) <- codegenExpr a
  (bOp, bType) <- codegenExpr b
  checkType (a, aType) (b, bType)
  id <- nextId
  append $ UnName id := addType aType aOp bOp
  return $ (LocalReference aType (UnName id), aType)
codegenExpr (I.Sub a b) = do
  (aOp, aType) <- codegenExpr a
  (bOp, bType) <- codegenExpr b
  id <- nextId
  checkType (a, aType) (b, bType)
  append $ UnName id := subType aType aOp bOp
  return $ (LocalReference aType (UnName id), aType)

-- | based on the llvm type, determine which llvm instruction to call for the operands
addType :: Type -> Operand -> Operand -> Instruction
addType t a b
  | t == int32Type = iadd32 a b
  | t == float32Type = fadd a b
  | otherwise = undefined

-- | based on the llvm type, determine which llvm instruction to call for the operands
subType :: Type -> Operand -> Operand -> Instruction
subType t a b
  | t == int32Type = isub32 a b
  | t == float32Type = fsub a b
  | otherwise = undefined

addFunc :: Word -> [Named Instruction] -> Codegen ()
addFunc returnId instructions = do
  let f = makeFunc returnId instructions
  oldModule <- gets cModule
  let moduleDefinitions = A.moduleDefinitions oldModule
  let newModule = oldModule { A.moduleDefinitions = moduleDefinitions ++ [f] }

  modify $ \state -> state { cModule = newModule }

makeFunc :: Word -> [Named Instruction] -> Definition
makeFunc returnId instructions = func float32Type "main" [ block "entry" instructions (ret (ref (UnName returnId) float32Type)) ]

-- | This function turns the `withContext` function into a continuation.
makeContext :: ContT a IO Context
makeContext = ContT withContext

-- | This function turns the `withModuleFromAST` function into a continuation.
makeLLVMModule :: Context -> A.Module -> ContT a IO M.Module
makeLLVMModule c m = ContT $ M.withModuleFromAST c m

-- | This function turns the `withHostTargetMachine` function into a continuation.
makeHostTargetMachine :: ContT a IO TargetMachine
makeHostTargetMachine = ContT withHostTargetMachine

-- | This function will take an ast module, and link it into a runnable executable.
-- It will create 3 files, "assembly.ll" for debugging, "assembly.o" for linking
-- and "assembly" an executable.
codegen :: A.Module -> IO ()
codegen astModule = (flip runContT) return $ do

  -- make a context
  ctx <- makeContext
  llModule <- makeLLVMModule ctx astModule

  -- check that it works
  liftIO $ verify llModule
  liftIO $ M.writeLLVMAssemblyToFile (M.File "assembly.ll") llModule

  -- create an object file
  target <- makeHostTargetMachine
  liftIO $ M.writeObjectToFile target (M.File "assembly.o") llModule

  -- link the object file
  liftIO $ callCommand $ "ld -macosx_version_min 10.12 -arch x86_64 -lSystem -o assembly assembly.o"
