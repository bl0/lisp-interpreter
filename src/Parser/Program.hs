{-# LANGUAGE OverloadedStrings #-}

module Parser.Program where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
-- my module
import AST
import Parser.Base
import Parser.Expr

get_ast :: Either String Prog -> Prog
get_ast (Right ast) = ast
get_ast s = error $ "error in get_ast: the parse result is " ++ show(s)

myParser :: Text -> Prog
myParser s = get_ast $ parseOnly programParser s

programParser :: Parser Stmt
programParser = stmtParser

stmtParser :: Parser Stmt
stmtParser =
  stmtListParser <|>
  varSetParser <|>
  skipParser <|>
  ifParser <|>
  whileParser

stmtListParser :: Parser Stmt
stmtListParser = do
  lexeme $ char '('
  lexeme $ string "begin"
  stmtList <- lexeme $ many1 stmtListParser
  lexeme $ char ')'
  return $ StmtList stmtList

varSetParser :: Parser Stmt
varSetParser = do
  lexeme $ char '('
  lexeme $ string "set!"
  v <- varParser
  e <- exprParser
  lexeme $ char ')'
  return $ VarSet v e

skipParser :: Parser Stmt
skipParser = do
  lexeme $ string "skip"
  return $ Skip

ifParser :: Parser Stmt
ifParser = do
  lexeme $ string "("
  expr <- exprParser
  s1 <- stmtParser
  s2 <- stmtParser
  lexeme $ string ")"
  return $ If expr s1 s2

whileParser :: Parser Stmt
whileParser = do
  lexeme $ string "("
  lexeme $ string "string"
  expr <- exprParser
  s <- stmtParser
  lexeme $ string ")"
  return $ While expr s
