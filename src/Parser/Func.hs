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
