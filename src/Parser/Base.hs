{-# LANGUAGE OverloadedStrings #-}

module Parser.Base where

import Control.Applicative
import Data.Attoparsec.Text
-- my module
import AST
import Memory

skipComment :: Parser ()
skipComment = skipMany comment
  where
    comment = do
      char ';'
      skipWhile (\c -> not $ isEndOfLine c)
      endOfLine

lexeme :: Parser a -> Parser a
lexeme p = do
  skipSpace
  skipComment
  p

varParser :: Parser Var
varParser = do
  c <- lexeme $ choice [letter, char '_']
  s <- many $ choice [digit, letter, char '_']
  return $ c:s

funcNameParser :: Parser FuncName
funcNameParser = varParser
