module Lib where
    -- (
    -- someFunc,
    -- eval
    -- )where

-- So we don't pollute the global namespace
import qualified Data.Map as Map
import Data.Maybe
import AST
import Parser

someFunc :: IO ()
someFunc = putStrLn "HelloWo"

eval_expr :: Expr -> Mem -> Val
eval_expr FalseLit _ = BoolVal False
eval_expr TrueLit _ = BoolVal True
eval_expr (Not expr) mem
    | (eval_expr expr mem == (BoolVal False)) = BoolVal True
    | otherwise = BoolVal False
eval_expr (And e1 e2) mem
    | (v1 == BoolVal True && v2 == BoolVal True) = BoolVal True
    | otherwise = BoolVal False
    where
        v1 = eval_expr e1 mem
        v2 = eval_expr e2 mem
eval_expr (Or e1 e2) mem
    | v1 == BoolVal True || v2 == BoolVal True = BoolVal True
    | otherwise = BoolVal False
    where
        v1 = eval_expr e1 mem
        v2 = eval_expr e2 mem
eval_expr (VarRef var) mem = fromMaybe (BoolVal False) $ Map.lookup var mem

-- Evaluation function
-- Given an initial memory, execute program and return the memory afterwards
eval :: Prog -> Mem -> Mem
eval Skip mem = mem
eval (VarSet var expr) mem = Map.insert var (eval_expr expr mem) mem -- let f _ = Just $ eval_expr mem expr in Map.update f var mem
eval (If expr stmt1 stmt2) mem
    | eval_expr expr mem == BoolVal True = eval stmt1 mem
    | otherwise = eval stmt2 mem
eval (While expr stmt) mem
    | eval_expr expr mem == BoolVal True = eval (While expr stmt) (eval stmt mem)
    | otherwise = mem
eval (StmtList stmtList) mem
    | null stmtList = mem
    | otherwise = eval (StmtList $ tail stmtList) (eval (head stmtList) mem)

eval_helper :: String -> Mem -> Mem
eval_helper s mem0 =
    let prog = read s :: Prog in
    eval prog mem0
