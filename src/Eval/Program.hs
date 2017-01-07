module Eval.Program(eval) where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
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
eval (MakeVector var expr) mem =
  let n = get_int $ eval_expr expr mem
      vec = Vector.replicate n Undefined in
    Map.insert var (VectorVal vec) mem
eval (VectorSet var expr1 expr2) mem =
  let index = get_int $ eval_expr expr1 mem
      value = eval_expr expr2 mem
      vec = get_vec $ eval_expr (VarRef var) mem
      new_vec = vec Vector.// [(index, value)] in
    Map.insert var (VectorVal new_vec) mem

-- (make-vector variable expression)
-- | (vector -set! variable expression expression)
