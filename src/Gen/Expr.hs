module Gen.Expr(gen_expr) where

import qualified Data.Map as Map

-- my modules
import AST
import Gen.Base

op_gen :: String -> Expr -> Expr -> String
op_gen op e1 e2 = gen_parent $ unwords [gen_expr e1, op, gen_expr e2]

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
gen_expr (VarRef var) = gen_var var
gen_expr Nil = "[]"
gen_expr (Cons e1 e2) = unwords [gen_brackets $ gen_expr e1,  "+", gen_expr e1]
gen_expr (Car e) = gen_expr e ++ "[0]"
gen_expr (Cdr e) = gen_expr e ++ "[1:]"
gen_expr (CharLit c) = show c
gen_expr (StringLit s) = show s
-- buggy: we use list to represent vector too.
gen_expr (VectorRef var expr) = gen_var var ++ (gen_brackets $ gen_expr expr)
gen_expr (Call funcName exprList) =
  let valList = map gen_expr exprList
      valString = gen_plist valList in
      (gen_var funcName) ++ valString
gen_expr (Let var e1 e2) = unwords [gen_var var, "=", gen_expr e1, ";", gen_expr e2]
gen_expr (Lambda var expr) = unwords ["lambda", gen_var var ++ ":", gen_expr expr]
gen_expr (LambdaCall e1 e2) =
  let s1 = gen_parent $ gen_expr e1
      s2 = gen_parent $ gen_expr e2 in
      s1 ++ s2
