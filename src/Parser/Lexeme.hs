{-# LANGUAGE OverloadedStrings #-}

module Parser.Lexeme where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p
