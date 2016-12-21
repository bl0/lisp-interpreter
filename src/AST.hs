module AST where

import qualified Data.Map as Map
import Data.Maybe
import Data.Scientific
-- We need to represent a variable name
type Var = String

data Expr

  -- Boolean expression
  = FalseLit
  | TrueLit
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  -- double
  | ScientificLit Scientific
  -- operator
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  -- compare operator
  | Eq Expr Expr
  | Lt Expr Expr
  | Le Expr Expr
  | Gt Expr Expr
  | Ge Expr Expr

  -- Reference the value of a variable. If the variable doesn't exist, it's an error
  | VarRef Var
  deriving (Show, Read)

data Stmt

  -- Execute a list of statements from left to right
  = StmtList [Stmt]

  -- Evaluate an expression, assign the result value to a variable; create the variable if it doesn't exist
  | VarSet Var Expr

  -- Evaluate the expression, if result is true, execute the left statement, otherwise if it's false, execute the right statement. If the expression doesn't return a boolean, it's an error
  | If Expr Stmt Stmt

  -- Repeatedly evaluate the expression, if result is true then execute the statement and repeat. The expression must return a boolean
  | While Expr Stmt

  -- Skip out one level of "while" loop. It's an error if currently we are not in a loop
  | Skip
  deriving (Show, Read)

-- A program is a single statement
type Prog = Stmt

-- Different kinds of Expr evaluate to different Val
data Val

  -- If you support other kinds of expressions, add the cases by yourself
  = BoolVal Bool
  | ScientificVal Scientific
  deriving (Show, Read, Eq, Ord)

get_bool :: Val -> Bool
get_bool (BoolVal b) = b
get_bool val = error $ "error: " ++ show(val) ++ "is not a BoolVal"

get_scientific :: Val -> Scientific
get_scientific (ScientificVal n) = n
get_scientific val = error $ "error: " ++ show(val) ++ "is not a ScientificVal"

-- A memory is a mapping from variable names to values
type Mem = Map.Map Var Val
