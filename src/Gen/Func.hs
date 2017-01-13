module Gen.Func where

import AST
import Gen.Base
import Gen.Stmt
import Gen.Expr

gen_func :: Func -> String
gen_func (Function funcName varList stmt) = def_str ++ stmt_str
  where
    -- TODO
    plist = gen_plist $ map gen_var varList
    -- plist = gen_parent $ init $ tail $ show varList
    def_str = gen_newline 0 $ "def " ++ gen_var funcName ++ plist ++ ":"
    stmt_str = gen_stmt 1 stmt
