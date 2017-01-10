{-# LANGUAGE OverloadedStrings #-}

module Parser.Expr(exprParser) where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as Text
import Data.Functor
-- my module
import AST
import Parser.Base

exprParser :: Parser Expr
exprParser =
  -- logic const
  falseParser
  <|> trueParser
  -- unit operator expr parser
  <|> uopExprParser
  -- binary operator expr Parser
  <|> bopExprParser
  -- double
  <|> numberParser
  -- String && List
  <|> nilParser
  <|> charParser
  <|> stringParser
  -- vector
  <|> vectorRefParser
  -- let
  <|> letExprParser
  -- lambda
  <|> lambdaExprParser
  <|> lambdaCallExprParser
  -- var
  <|> varExprParser
  -- function call
  <|> callExprParser

falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

uopParser :: Parser Text.Text
uopParser = string "not"
  <|> "car"
  <|> "cdr"

uopExprParser :: Parser Expr
uopExprParser = do
  lexeme $ char '('
  op <- lexeme $ uopParser
  expr <- exprParser
  lexeme $ char ')'
  case op of
    "not" -> return (Not expr)
    "car" -> return (Car expr)
    "cdr" -> return (Cdr expr)

bopParser :: Parser Text.Text
bopParser = string "and"
  <|> string "or"
  <|> string "+"
  <|> string "-"
  <|> string "*"
  <|> string "/"
  <|> string "="
  <|> string "<="
  <|> string "<"
  <|> string ">="
  <|> string ">"
  <|> string "cons"

bopExprParser :: Parser Expr
bopExprParser = do
  lexeme $ char '('
  op <- lexeme $ bopParser
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  case op of
    "and"  -> return (And expr1 expr2)
    "or"   -> return (Or expr1 expr2)
    "+"    -> return (Add expr1 expr2)
    "-"    -> return (Sub expr1 expr2)
    "*"    -> return (Mult expr1 expr2)
    "/"    -> return (Div expr1 expr2)
    "="    -> return (Eq expr1 expr2)
    "<"    -> return (Lt expr1 expr2)
    "<="   -> return (Le expr1 expr2)
    ">"    -> return (Gt expr1 expr2)
    ">="   -> return (Ge expr1 expr2)
    "cons" -> return (Cons expr1 expr2)

numberParser :: Parser Expr
numberParser = do
  d1 <- lexeme $ scientific
  return $ ScientificLit d1

varExprParser :: Parser Expr
varExprParser = do
  v <- varParser
  return $ VarRef v

nilParser :: Parser Expr
nilParser = lexeme $ string "nil" $> Nil

charParser :: Parser Expr
charParser = do
  lexeme $ char '\''
  c <- anyChar
  lexeme $ char '\''
  return (CharLit c)

stringParser :: Parser Expr
stringParser = do
  lexeme $ char '\"'
  -- TODO buggy
  s <- takeTill (\c -> c == '\"')
  lexeme $ char '\"'
  return (StringLit $ Text.unpack s)

vectorRefParser :: Parser Expr
vectorRefParser = do
  lexeme $ string "("
  lexeme $ string "vector-ref"
  var <- varParser
  expr <- exprParser
  lexeme $ char ')'
  return (VectorRef var expr)

callExprParser :: Parser Expr
callExprParser = do
  lexeme $ string "("
  funcname <- funcNameParser
  exprList <- many' exprParser
  lexeme $ char ')'
  return (Call funcname exprList)

letExprParser :: Parser Expr
letExprParser = do
  lexeme $ string "("
  lexeme $ string "let"
  var <- varParser
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Let var expr1 expr2)


lambdaExprParser :: Parser Expr
lambdaExprParser = do
  lexeme $ string "("
  lexeme $ string "lambda"
  var <- varParser
  expr <- exprParser
  lexeme $ char ')'
  return (Lambda var expr)

lambdaCallExprParser :: Parser Expr
lambdaCallExprParser = do
  lexeme $ string "("
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return $ LambdaCall expr1 expr2
