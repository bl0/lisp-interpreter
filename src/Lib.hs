module Lib where
  -- (
  -- someFunc,
  -- eval
  -- )where

-- So we don't pollute the global namespace
import AST
import Parser.Base
import Parser.Expr
import Parser.Program
import Eval.Expr
import Eval.Program

someFunc :: IO ()
someFunc = putStrLn "HelloWo"
