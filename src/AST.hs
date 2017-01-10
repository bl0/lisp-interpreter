{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module AST where

import Data.Scientific
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty

-- my modules
-- import Memory

-- We need to represent a variable name
type Var = String
type FuncName = String

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

  -- String and List
  | Nil
  | Cons Expr Expr
  | Car Expr
  | Cdr Expr
  | CharLit Char
  -- TODO show string
  | StringLit String
  -- Array
  | VectorRef Var Expr
  -- Function Call
  | Call FuncName [Expr]
  -- let
  | Let Var Expr Expr
  deriving (Show, Read, Eq, Ord, Out, Generic)

-- Scientific is not a Out instance, so we need to inplement it
instance Out Scientific where
  docPrec _ s = text $ show s
  doc = docPrec 0

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

  -- Vector
  | MakeVector Var Expr
  | VectorSet Var Expr Expr
  -- Function
  | Return Expr
  deriving (Show, Read, Eq, Ord, Out, Generic)

-- Function
data Func = Function FuncName [Var] Stmt
  deriving (Show, Read, Eq, Out, Generic)
-- A program is a single statement
type Prog = [Func]
