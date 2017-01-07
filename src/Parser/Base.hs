{-# LANGUAGE OverloadedStrings #-}

module Parser.Base where

import Control.Applicative
import Data.Attoparsec.Text
-- my module
import AST
import Memory


lexeme :: Parser a -> Parser a
lexeme p = do
  skipSpace
  p

varParser :: Parser Var
varParser = lexeme $ many1 letter

funcNameParser :: Parser FuncName
funcNameParser = varParser
