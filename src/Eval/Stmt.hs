module Eval.Stmt where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
-- my module
import AST
import Eval.Expr
import Memory


-- Evaluation function
-- Given an initial memory, execute program and return the memory afterwards
eval_stmt :: Stmt -> Mem -> Mem
eval_stmt Skip mem = mem
eval_stmt (VarSet var expr) mem = Map.insert var (eval_expr expr mem) mem
eval_stmt (If expr stmt1 stmt2) mem
  | eval_expr expr mem == BoolVal True = eval_stmt stmt1 mem
  | otherwise = eval_stmt stmt2 mem
eval_stmt (While expr stmt) mem
  | eval_expr expr mem == BoolVal True = eval_stmt (While expr stmt) (eval_stmt stmt mem)
  | otherwise = mem
eval_stmt (StmtList stmtList) mem
  | null stmtList = mem
  | otherwise = eval_stmt (StmtList $ tail stmtList) (eval_stmt (head stmtList) mem)
eval_stmt (MakeVector var expr) mem =
  let n = get_int $ eval_expr expr mem
      vec = Vector.replicate n Undefined in
    Map.insert var (VectorVal vec) mem
eval_stmt (VectorSet var expr1 expr2) mem =
  let index = get_int $ eval_expr expr1 mem
      value = eval_expr expr2 mem
      vec = get_vec $ eval_expr (VarRef var) mem
      new_vec = vec Vector.// [(index, value)] in
    Map.insert var (VectorVal new_vec) mem
