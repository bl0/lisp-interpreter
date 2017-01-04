module Eval.Program(eval) where

import qualified Data.Map as Map
-- my module
import AST
import Eval.Expr
import Memory


-- Evaluation function
-- Given an initial memory, execute program and return the memory afterwards
eval :: Prog -> Mem -> Mem
eval Skip mem = mem
eval (VarSet var expr) mem = Map.insert var (eval_expr expr mem) mem 
eval (If expr stmt1 stmt2) mem
  | eval_expr expr mem == BoolVal True = eval stmt1 mem
  | otherwise = eval stmt2 mem
eval (While expr stmt) mem
  | eval_expr expr mem == BoolVal True = eval (While expr stmt) (eval stmt mem)
  | otherwise = mem
eval (StmtList stmtList) mem
  | null stmtList = mem
  | otherwise = eval (StmtList $ tail stmtList) (eval (head stmtList) mem)
