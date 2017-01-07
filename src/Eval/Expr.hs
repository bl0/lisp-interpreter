module Eval.Expr(eval_expr) where

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import AST
import Memory

eval_expr :: Expr -> Mem -> Val
eval_expr FalseLit _ = BoolVal False
eval_expr TrueLit _ = BoolVal True
eval_expr (Not expr) mem
  | v == False = BoolVal True
  | otherwise = BoolVal False
  where v = get_bool $ eval_expr expr mem
eval_expr (And e1 e2) mem
  | v1 == True && v2 == True = BoolVal True
  | otherwise = BoolVal False
  where
    v1 = get_bool $ eval_expr e1 mem
    v2 = get_bool $ eval_expr e2 mem
eval_expr (Or e1 e2) mem
  | v1 == True || v2 == True = BoolVal True
  | otherwise = BoolVal False
  where
    v1 = get_bool $ eval_expr e1 mem
    v2 = get_bool $ eval_expr e2 mem
-- TODO asume bool is smaller than scientific
eval_expr (Eq e1 e2) mem = BoolVal $ eval_expr e1 mem == eval_expr e2 mem
eval_expr (Lt e1 e2) mem = BoolVal $ eval_expr e1 mem <  eval_expr e2 mem
eval_expr (Le e1 e2) mem = BoolVal $ eval_expr e1 mem <= eval_expr e2 mem
eval_expr (Gt e1 e2) mem = BoolVal $ eval_expr e1 mem >  eval_expr e2 mem
eval_expr (Ge e1 e2) mem = BoolVal $ eval_expr e1 mem >= eval_expr e2 mem
-- TODO error handy
eval_expr (VarRef var) mem = case Map.lookup var mem of
  Nothing -> error $ "error: var " ++ var ++ "does't exist in mem."
  Just v  -> v
-- scientific expr
eval_expr (ScientificLit n) _ = ScientificVal n
eval_expr (Add e1 e2)  mem =
  let v1 = get_scientific $ eval_expr e1 mem in
  let v2 = get_scientific $ eval_expr e2 mem in
  ScientificVal $ v1 + v2
eval_expr (Sub e1 e2)  mem =
  let v1 = get_scientific $ eval_expr e1 mem in
  let v2 = get_scientific $ eval_expr e2 mem in
  ScientificVal $ v1 - v2
eval_expr (Mult e1 e2) mem =
  let v1 = get_scientific $ eval_expr e1 mem in
  let v2 = get_scientific $ eval_expr e2 mem in
  ScientificVal $ v1 * v2
eval_expr (Div e1 e2)  mem =
  let v1 = get_scientific $ eval_expr e1 mem in
  let v2 = get_scientific $ eval_expr e2 mem in
  ScientificVal $ v1 / v2
-- list and String
eval_expr Nil _ = ListVal []
eval_expr (Cons e1 e2) mem =
  let v1 = eval_expr e1 mem in
  let v2 = get_list $ eval_expr e2 mem in
  ListVal $ v1 : v2
eval_expr (Car e) mem
  | v == ListVal [] = error $ "error when trying to get head of empty list."
  | otherwise = head $ get_list v
  where v = eval_expr e mem
eval_expr (Cdr e) mem
  | v == ListVal [] = error $ "error when trying to get tail of empty list."
  | otherwise = ListVal $ tail $ get_list v
  where v = eval_expr e mem
eval_expr (CharLit c) mem = CharVal c
eval_expr (StringLit s) mem
  | s == "" = ListVal []
  | otherwise = eval_expr (Cons (CharLit $ head s) (StringLit $ tail s)) mem
-- vector
eval_expr (VectorRef var e) mem
  | index < 0 = error $ "index should not less than 0."
  | index >= (Vector.length vec) = error $ "index out of range: index is " ++ show(index) ++ " while length of " ++ show(vec) ++ " is " ++ show(Vector.length vec)
  | otherwise = vec Vector.! index 
  where
    vec = get_vec $ eval_expr (VarRef var) mem
    index = get_int $ eval_expr e mem
