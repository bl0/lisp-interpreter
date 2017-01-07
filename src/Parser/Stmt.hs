{-# LANGUAGE OverloadedStrings #-}

module Parser.Stmt where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as Text


-- my module
import AST
import Parser.Base
import Parser.Expr

stmtParser :: Parser Stmt
stmtParser =
  stmtListParser
  <|> varSetParser 
  <|> skipParser 
  <|> ifParser 
  <|> whileParser 
  <|> makeVectorParser 
  <|> vectorSetParser 
  <|> returnParser

stmtListParser :: Parser Stmt
stmtListParser = do
  lexeme $ char '('
  lexeme $ string "begin"
  stmtList <- many1 stmtParser
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
  lexeme $ string "if"
  expr <- exprParser
  s1 <- stmtParser
  s2 <- stmtParser
  lexeme $ string ")"
  return $ If expr s1 s2

whileParser :: Parser Stmt
whileParser = do
  lexeme $ string "("
  lexeme $ string "while"
  expr <- exprParser
  s <- stmtParser
  lexeme $ string ")"
  return $ While expr s

makeVectorParser :: Parser Stmt
makeVectorParser = do
  lexeme $ string "("
  lexeme $ string "make-vector"
  var <- varParser
  expr <- exprParser
  lexeme $ string ")"
  return $ MakeVector var expr

vectorSetParser :: Parser Stmt
vectorSetParser = do
  lexeme $ string "("
  lexeme $ string "vector-set!"
  var <- varParser
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ string ")"
  return $ VectorSet var expr1 expr2

returnParser :: Parser Stmt
returnParser = do
  lexeme $ string "("
  lexeme $ string "return"
  expr <- exprParser
  lexeme $ string ")"
  return $ Return expr
