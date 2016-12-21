module Eval.Expr where

import qualified Data.Map as Map
import Data.Scientific
import Data.Maybe

import AST
import Parser.Expr

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
-- asume bool is smaller than scientific
eval_expr (Eq e1 e2) mem = BoolVal $ eval_expr e1 mem == eval_expr e2 mem
eval_expr (Lt e1 e2) mem = BoolVal $ eval_expr e1 mem <  eval_expr e2 mem
eval_expr (Le e1 e2) mem = BoolVal $ eval_expr e1 mem <= eval_expr e2 mem
eval_expr (Gt e1 e2) mem = BoolVal $ eval_expr e1 mem >  eval_expr e2 mem
eval_expr (Ge e1 e2) mem = BoolVal $ eval_expr e1 mem >= eval_expr e2 mem
eval_expr (VarRef var) mem
  | l == [] = error $ "error: var " ++ var ++ "does't exist in mem."
  | otherwise = head $ l
  where l = maybeToList $ Map.lookup var mem
-- scientific expr
eval_expr (ScientificLit n) mem = ScientificVal n
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
