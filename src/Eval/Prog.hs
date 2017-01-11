module Eval.Prog(eval) where

import qualified Data.Map as Map
-- my module
import AST
import Eval.Expr
import Memory

-- Evaluation function
-- Given an initial memory, execute program and return the memory afterwards
eval :: Prog -> Mem -> Mem
eval prog mem =
  let new_mem = Map.fromList $ map (\f -> func2tuple f) prog
      mainFunc = memLookup "main" new_mem in
  Map.singleton "returnValue" $ eval_expr (Call "main" []) new_mem
