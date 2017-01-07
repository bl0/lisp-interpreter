{-# LANGUAGE OverloadedStrings #-}

module Parser.Prog where

import Data.Attoparsec.Text
import qualified Data.Text as Text


-- my module
import AST
import Parser.Func

programParser :: Text.Text -> Either String Prog
programParser = parseOnly $ many1 funcParser
