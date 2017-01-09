{-# LANGUAGE OverloadedStrings #-}

module Parser.Func where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as Text


-- my module
import AST
import Parser.Base
import Parser.Expr
import Parser.Stmt

funcParser :: Parser Func
funcParser = do
  lexeme $ string "("
  lexeme $ string "define"
  lexeme $ string "("
  funName <- varParser
  varList <- many' funcNameParser
  lexeme $ string ")"
  stmt <- stmtParser
  lexeme $ string ")"
  return $ Function funName varList stmt
