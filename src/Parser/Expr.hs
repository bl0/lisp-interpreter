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
  falseParser <|>
  trueParser <|>
  -- logic operator
  notParser <|>
  andParser <|>
  orParser <|>
  -- double
  numberParser <|>
  -- operator
  addParser <|>
  subParser <|>
  multParser <|>
  divParser <|>
  -- compare
  eqParser <|>
  ltParser <|>
  leParser <|>
  gtParser <|>
  geParser <|>
  -- String && List
  nilParser <|>
  consParser <|>
  carParser <|>
  cdrParser <|>
  charParser <|>
  stringParser <|>
  -- vector
  vectorRefParser <|>
  -- var
  varExprParser
falseParser :: Parser Expr
falseParser = lexeme $ string "False" $> FalseLit

trueParser :: Parser Expr
trueParser = lexeme $ string "True" $> TrueLit

notParser :: Parser Expr
notParser = do
  lexeme $ char '('
  lexeme $ string "not"
  expr <- exprParser
  lexeme $ char ')'
  return (Not expr)

andParser :: Parser Expr
andParser = do
  lexeme $ char '('
  lexeme $ string "and"
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (And expr1 expr2)

orParser :: Parser Expr
orParser = do
  lexeme $ char '('
  lexeme $ string "or"
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Or expr1 expr2)


numberParser :: Parser Expr
numberParser = do
  d1 <- lexeme $ scientific
  return $ ScientificLit d1


addParser :: Parser Expr
addParser = do
  lexeme $ char '('
  lexeme $ char '+'
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Add expr1 expr2)

subParser :: Parser Expr
subParser = do
  lexeme $ char '('
  lexeme $ string "-"
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Sub expr1 expr2)

multParser :: Parser Expr
multParser = do
  lexeme $ char '('
  lexeme $ string "*"
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Mult expr1 expr2)

divParser :: Parser Expr
divParser = do
  lexeme $ char '('
  lexeme $ string "/"
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Div expr1 expr2)

eqParser :: Parser Expr
eqParser = do
  lexeme $ char '('
  lexeme $ string "="
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Eq expr1 expr2)

ltParser :: Parser Expr
ltParser = do
  lexeme $ char '('
  lexeme $ string "<"
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Lt expr1 expr2)

leParser :: Parser Expr
leParser = do
  lexeme $ char '('
  lexeme $ string "<="
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Le expr1 expr2)

gtParser :: Parser Expr
gtParser = do
  lexeme $ char '('
  lexeme $ string ">"
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Gt expr1 expr2)

geParser :: Parser Expr
geParser = do
  lexeme $ char '('
  lexeme $ string ">="
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Ge expr1 expr2)

varExprParser :: Parser Expr
varExprParser = do
  v <- varParser
  return $ VarRef v

nilParser :: Parser Expr
nilParser = do
  lexeme $ string "nil"
  return Nil

consParser :: Parser Expr
consParser = do
  lexeme $ string "("
  lexeme $ string "cons"
  expr1 <- exprParser
  expr2 <- exprParser
  lexeme $ char ')'
  return (Cons expr1 expr2)

carParser :: Parser Expr
carParser = do
  lexeme $ string "("
  lexeme $ string "car"
  expr1 <- exprParser
  lexeme $ char ')'
  return (Car expr1)

cdrParser :: Parser Expr
cdrParser = do
  lexeme $ string "("
  lexeme $ string "cdr"
  expr1 <- exprParser
  lexeme $ char ')'
  return (Cdr expr1)

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
