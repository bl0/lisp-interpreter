module Lib
    (
    someFunc,
    eval
    )where

-- So we don't pollute the global namespace
import qualified Data.Map as Map

-- We need to represent a variable name
type Var = String

data Expr

    -- Constructors for "Boolean expression"
    -- Omitting other kinds of simple expressions. You may add them back.
    = FalseLit
    | TrueLit
    | Not Expr
    | And Expr Expr
    | Or Expr Expr

    -- Reference the value of a variable. If the variable doesn't exist, it's an error
    | VarRef Var
    deriving Show

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
    deriving Show

-- A program is a single statement
type Prog = Stmt

-- Different kinds of Expr evaluate to different Val
data Val

    -- If you support other kinds of expressions, add the cases by yourself
    = BoolVal Bool
    deriving Show

-- A memory is a mapping from variable names to values
type Mem = Map.Map Var Val

-- Evaluation function
-- Given an initial memory, execute program and return the memory afterwards
eval :: Prog -> Mem -> Mem
eval = undefined


someFunc :: IO ()
someFunc = putStrLn "HelloWo"
