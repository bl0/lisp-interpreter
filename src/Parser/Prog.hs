{-# LANGUAGE OverloadedStrings #-}

module Parser.Prog where

import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text as Text


-- my module
import AST
import Parser.Base
import Parser.Expr
import Parser.Stmt

programParser :: Text.Text -> Either String Stmt
programParser = parseOnly stmtParser
