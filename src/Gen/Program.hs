module Gen.Program
    (
    gen_program
    ) where

-- my modules
import AST
import Gen.Expr
import Gen.Config

gen_stmt :: Int -> Stmt -> String
gen_stmt ident (StmtList stmtList) = concat $ map (gen_stmt ident) stmtList
gen_stmt ident (VarSet var expr) = gen_newline ident $ v ++ " = " ++ e
  where
    v = gen_expr (VarRef var)
    e = gen_expr expr
gen_stmt ident (If expr stmt1 stmt2) = e1 ++ s1 ++ elsecode ++ s2
  where
    e1 = gen_newline ident $ "if " ++ gen_expr expr ++ ":"
    s1 = gen_stmt (ident + 1) stmt1
    elsecode = gen_newline ident "else:"
    s2 = gen_stmt (ident + 1) stmt2
gen_stmt ident (While expr stmt) = e ++ s
  where
    e = gen_newline ident $ "while " ++ gen_expr expr ++ ":"
    s = gen_stmt (ident+1) stmt
gen_stmt ident Skip = gen_newline ident "pass"

gen_program :: Prog -> String
gen_program program = gen_stmt 0 program
