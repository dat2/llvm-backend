{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Codegen
  ( codegen
  , execute
  ) where

import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Int
import           Data.String
import           Foreign.Ptr
import           System.Process

import           LLVM.Analysis
import           LLVM.Internal.Context
import           LLVM.Internal.Target
import           LLVM.OrcJIT
import           LLVM.OrcJIT.IRCompileLayer (IRCompileLayer, withIRCompileLayer)

import           LLVM.AST                   (Definition (..))
import           LLVM.AST.CallingConvention
import           LLVM.AST.Constant          (Constant (Float, GlobalReference, Int))
import           LLVM.AST.Float
import           LLVM.AST.Global
import           LLVM.AST.Instruction
import           LLVM.AST.Name
import           LLVM.AST.Operand
import           LLVM.AST.Type

import qualified IR                         as I
import qualified LLVM.AST                   as A
import qualified LLVM.Module                as M
import qualified LLVM.OrcJIT.IRCompileLayer as IRCompileLayer

-- types
int32Type :: Type
int32Type = IntegerType 32

float32Type :: Type
float32Type = FloatingPointType FloatFP

functionType :: Type -> [Type] -> Type
functionType returnType paramTypes = FunctionType returnType paramTypes False

irToLlvmType :: I.Type -> Type
irToLlvmType I.TInt32   = int32Type
irToLlvmType I.TFloat32 = float32Type

irToLlvmParam :: I.Param -> Parameter
irToLlvmParam I.Param {I.pName, I.pType} =
  Parameter (irToLlvmType pType) (Name (fromString pName)) []

-- definitions
func :: Type -> [Parameter] -> String -> [BasicBlock] -> Definition
func returnType params name blocks =
  GlobalDefinition $
  functionDefaults
  { returnType = returnType
  , parameters = (params, False)
  , name = Name (fromString name)
  , basicBlocks = blocks
  }

-- blocks
block :: String -> [Named Instruction] -> Named Terminator -> BasicBlock
block name = BasicBlock (Name (fromString name))

-- instructions
iadd32 :: Operand -> Operand -> Instruction
iadd32 a b = Add False True a b []

fadd :: Operand -> Operand -> Instruction
fadd a b = FAdd (FastMathFlags True True True True) a b []

isub32 :: Operand -> Operand -> Instruction
isub32 a b = Sub False True a b []

fsub :: Operand -> Operand -> Instruction
fsub a b = FSub (FastMathFlags True True True True) a b []

call :: String -> Type -> [Operand] -> Instruction
call fName fType ops =
  Call
    Nothing
    C
    []
    (Right (ConstantOperand $ GlobalReference fType (fromString fName)))
    (map (\op -> (op, [])) ops)
    []
    []

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
initialModule :: String -> String -> A.Module
initialModule source name =
  A.defaultModule
  { A.moduleName = fromString name
  , A.moduleSourceFileName = fromString source
  , A.moduleDefinitions = []
  }

-- monad
data CodegenState = CodegenState
  { cId           :: Word
  , cInstructions :: [Named Instruction]
  , cModule       :: A.Module
  } deriving (Show, Eq)

initialState :: String -> String -> CodegenState
initialState sourceName moduleName =
  CodegenState
  {cId = 0, cInstructions = [], cModule = initialModule sourceName moduleName}

data CodegenError =
  MismatchType I.Expr
               Type
               I.Expr
               Type
  deriving (Show, Eq)

newtype Codegen a = Codegen
  { runCodegen :: ExceptT CodegenError (State CodegenState) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState CodegenState
             , MonadError CodegenError
             )

evalCodegen :: String -> String -> Codegen () -> Either CodegenError A.Module
evalCodegen sourceName moduleName c =
  case runState (runExceptT (runCodegen c)) (initialState sourceName moduleName) of
    (Left e, _)   -> Left e
    (Right _, s') -> Right (cModule s')

-- | get the next id to use for generation
nextId :: Codegen Word
nextId = do
  i <- gets cId
  modify $ \s -> s {cId = i + 1}
  return i

-- | append an instruction to the current list of instructions
append :: Named Instruction -> Codegen ()
append i = do
  instructions <- gets cInstructions
  modify $ \s -> s {cInstructions = instructions ++ [i]}

-- | clear function state
clear :: Codegen ()
clear = modify $ \s -> s {cInstructions = [], cId = 0}

-- | this function will throwError if the types do not match
checkType :: (I.Expr, Type) -> (I.Expr, Type) -> Codegen ()
checkType (aExpr, aType) (bExpr, bType) =
  unless (aType == bType) $ throwError (MismatchType aExpr aType bExpr bType)

codegenModule :: I.Module -> Either CodegenError A.Module
codegenModule I.Module {I.mName, I.mSource, I.mFunctions} =
  evalCodegen mSource mName (codegenFuncs mFunctions)

codegenFuncs :: [I.Function] -> Codegen ()
codegenFuncs fs = forM_ fs (\f -> codegenFunc f >> clear)

-- | create a "main" function for the given ir expression
codegenFunc :: I.Function -> Codegen ()
codegenFunc I.Function {I.fName, I.fReturnType, I.fParams, I.fExpr} = do
  _ <- codegenExpr fExpr
  instructions <- gets cInstructions
  i <- gets cId
  addFunc
    (fromString fName)
    (irToLlvmType fReturnType)
    (map irToLlvmParam fParams)
    (i - 1)
    instructions

-- | given an IR expression, create the llvm instructions for it.
codegenExpr :: I.Expr -> Codegen (Operand, Type)
codegenExpr (I.Ref t n) =
  return (LocalReference (irToLlvmType t) (Name (fromString n)), irToLlvmType t)
codegenExpr (I.Int32 i) = return (int32 i, int32Type)
codegenExpr (I.Float32 f) = return (float32 f, float32Type)
codegenExpr (I.Add a b) = do
  (aOp, aType) <- codegenExpr a
  (bOp, bType) <- codegenExpr b
  checkType (a, aType) (b, bType)
  i <- nextId
  append $ UnName i := addType aType aOp bOp
  return (LocalReference aType (UnName i), aType)
codegenExpr (I.Sub a b) = do
  (aOp, aType) <- codegenExpr a
  (bOp, bType) <- codegenExpr b
  i <- nextId
  checkType (a, aType) (b, bType)
  append $ UnName i := subType aType aOp bOp
  return (LocalReference aType (UnName i), aType)
codegenExpr (I.Call s fReturnType fParamTypes args) = do
  refs <- mapM codegenExpr args
  -- TODO check types between args?
  let refOps = map fst refs
  let _ = map snd refs
  i <- nextId
  append $
    UnName i :=
    call
      (fromString s)
      (functionType (irToLlvmType fReturnType) (map irToLlvmType fParamTypes))
      refOps
  return
    ( LocalReference (irToLlvmType fReturnType) (UnName i)
    , irToLlvmType fReturnType)

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

addFunc ::
     String -> Type -> [Parameter] -> Word -> [Named Instruction] -> Codegen ()
addFunc name rType params returnId instructions = do
  let f =
        func
          rType
          params
          name
          [block "entry" instructions (ret (ref (UnName returnId) rType))]
  oldModule <- gets cModule
  let moduleDefinitions = A.moduleDefinitions oldModule
  let newModule = oldModule {A.moduleDefinitions = moduleDefinitions ++ [f]}
  modify $ \s -> s {cModule = newModule}

codegen :: I.Module -> IO (Either CodegenError ())
codegen irModule =
  case codegenModule irModule of
    Left e -> return $ Left e
    Right astModule ->
      llvmCodegen (I.mName irModule) astModule >> return (Right ())

execute :: I.Module -> IO (Either CodegenError Int32)
execute irModule =
  case codegenModule irModule of
    Left e          -> return $ Left e
    Right astModule -> Right <$> llvmExecute astModule

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
llvmCodegen :: String -> A.Module -> IO ()
llvmCodegen moduleName astModule =
  flip runContT return $
  -- make a context
   do
    ctx <- makeContext
    llModule <- makeLLVMModule ctx astModule
  -- check that it works
    liftIO $ M.writeLLVMAssemblyToFile (M.File (moduleName ++ ".ll")) llModule
    liftIO $ verify llModule
  -- create an object file
    target <- makeHostTargetMachine
    liftIO $ M.writeObjectToFile target (M.File (moduleName ++ ".o")) llModule
  -- link the object file
    liftIO $
      callCommand
        "ld -macosx_version_min 10.12 -arch x86_64 -lSystem -o assembly assembly.o"

foreign import ccall "dynamic" mkMain ::
               FunPtr (IO Int32) -> IO Int32

makeObjectLinkingLayer :: ContT a IO ObjectLinkingLayer
makeObjectLinkingLayer = ContT withObjectLinkingLayer

makeIRCompileLayer ::
     ObjectLinkingLayer -> TargetMachine -> ContT a IO IRCompileLayer
makeIRCompileLayer objectLayer target =
  ContT $ withIRCompileLayer objectLayer target

resolver :: IRCompileLayer -> MangledSymbol -> IO JITSymbol
resolver compileLayer symbol =
  IRCompileLayer.findSymbol compileLayer symbol True

nullResolver :: MangledSymbol -> IO JITSymbol
nullResolver _ = return (JITSymbol 0 (JITSymbolFlags False False))

makeModuleSet ::
     IRCompileLayer -> M.Module -> ContT a IO IRCompileLayer.ModuleSet
makeModuleSet compileLayer llModule =
  ContT $
  IRCompileLayer.withModuleSet
    compileLayer
    [llModule]
    (SymbolResolver (resolver compileLayer) nullResolver)

llvmExecute :: A.Module -> IO Int32
llvmExecute astModule =
  flip runContT return $
  -- make a context
   do
    ctx <- makeContext
    llModule <- makeLLVMModule ctx astModule
  -- check that it works
    liftIO $ verify llModule
    target <- makeHostTargetMachine
    objectLayer <- makeObjectLinkingLayer
    compileLayer <- makeIRCompileLayer objectLayer target
    mainFunc <- liftIO $ IRCompileLayer.mangleSymbol compileLayer "main"
    _moduleSet <- makeModuleSet compileLayer llModule
    JITSymbol mainFn _ <- liftIO $ resolver compileLayer mainFunc
    liftIO $ mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
