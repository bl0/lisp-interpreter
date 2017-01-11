-- TODO: This file needs to be modified to meet our AST afterwards

{-# LANGUAGE OverloadedStrings #-}

module Compiler.Emit where

import LLVM.General.Module
import LLVM.General.Context

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.FloatingPointPredicate as FP

import Data.Word
import Data.Int
import Data.Scientific
import Control.Monad.Except
import Control.Applicative
import qualified Data.Map as Map

import Compiler.Codegen
import qualified AST as S

toSig :: [String] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name x))


codegenTop :: S.Expr -> LLVM ()
{-
codegenTop (S.Function name args body) = do
  define double name fnargs bls
  where
    fnargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      forM args $ \a -> do
        var <- alloca double
        store var (local (AST.Name a))
        assign a var
      cgen body >>= ret
-}

{-
codegenTop (S.Extern name args) = do
  external double name fnargs
  where fnargs = toSig args
-}

codegenTop exp = do
  define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock entryBlockName
      setBlock entry
      cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

feq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
feq a b = do
  test <- fcmp FP.UEQ a b
  uitofp double test

flt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
flt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

fle :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fle a b = do
  test <- fcmp FP.ULE a b
  uitofp double test

fgt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fgt a b = do
  test <- fcmp FP.UGT a b
  uitofp double test

fge :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fge a b = do
  test <- fcmp FP.UGE a b
  uitofp double test

cgen :: S.Expr -> Codegen AST.Operand
{-
cgen (S.UnaryOp op a) = do
  cgen $ S.Call ("unary" ++ op) [a]
-}
{-
cgen (S.BinaryOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  return cval
-}

cgen (S.Add a b) = do
      ca <- cgen a
      cb <- cgen b
      fadd ca cb

cgen (S.Sub a b) = do
      ca <- cgen a
      cb <- cgen b
      fsub ca cb

cgen (S.Mult a b) = do
      ca <- cgen a
      cb <- cgen b
      fmul ca cb

cgen (S.Div a b) = do
      ca <- cgen a
      cb <- cgen b
      fdiv ca cb

cgen (S.Eq a b) = do
      ca <- cgen a
      cb <- cgen b
      feq ca cb

cgen (S.Lt a b) = do
      ca <- cgen a
      cb <- cgen b
      flt ca cb

cgen (S.Le a b) = do
      ca <- cgen a
      cb <- cgen b
      fle ca cb

cgen (S.Gt a b) = do
      ca <- cgen a
      cb <- cgen b
      fgt ca cb

cgen (S.Ge a b) = do
      ca <- cgen a
      cb <- cgen b
      fge ca cb

cgen (S.VarRef x) = getvar x >>= load
cgen (S.ScientificLit n) = return $ cons $ C.Float (F.Double (toRealFloat n))
cgen (S.Call fn args) = do
  largs <- mapM cgen args
  call (externf (AST.Name fn)) largs

-------------------------------------------------------------------------------
-- Compilation
-------------------------------------------------------------------------------

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod fns = withContext $ \context ->
  liftError $ withModuleFromAST context newast $ \m -> do
    llstr <- moduleLLVMAssembly m
    putStrLn llstr
    return newast
  where
    modn    = mapM codegenTop fns
    newast  = runLLVM mod modn
