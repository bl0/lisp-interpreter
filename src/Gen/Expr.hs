module Gen.Expr(gen_expr) where

import qualified Data.Map as Map

-- my modules
import AST

op_gen :: String -> Expr -> Expr -> String
op_gen op e1 e2 = "(" ++ gen_expr e1 ++ " " ++ op ++ " " ++ gen_expr e2 ++ ")"

gen_expr :: Expr -> String
gen_expr FalseLit = "False"
gen_expr TrueLit = "True"
gen_expr (Not expr) = "(not " ++ gen_expr expr ++ ")"
gen_expr (And e1 e2) = op_gen "and" e1 e2
gen_expr (Or e1 e2) = op_gen "or" e1 e2
gen_expr (Add e1 e2) = op_gen "+" e1 e2
gen_expr (Sub e1 e2) = op_gen "-" e1 e2
gen_expr (Mult e1 e2) = op_gen "*" e1 e2
gen_expr (Div e1 e2) = op_gen "/" e1 e2
gen_expr (Eq e1 e2) = op_gen "==" e1 e2
gen_expr (Lt e1 e2) = op_gen "<" e1 e2
gen_expr (Le e1 e2) = op_gen "<=" e1 e2
gen_expr (Gt e1 e2) = op_gen ">" e1 e2
gen_expr (Ge e1 e2) = op_gen ">=" e1 e2
gen_expr (ScientificLit n) = show n
gen_expr (VarRef var) = "var_" ++ var
