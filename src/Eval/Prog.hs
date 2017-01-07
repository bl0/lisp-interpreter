module Eval.Prog(eval) where

-- my module
import AST
import Eval.Stmt
import Memory


-- Evaluation function
-- Given an initial memory, execute program and return the memory afterwards
eval :: Prog -> Mem -> Mem
eval = eval_stmt
