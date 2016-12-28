{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Options.Applicative
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Text as Text
import System.IO
import System.Exit
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PrettyPrint
import qualified Text.PrettyPrint.GenericPretty as GenericPretty

-- my module
import Eval.Program
import Parser.Program
import AST
import Memory

data ArgOptions = ArgOptions
  { input :: String
  , output :: String
  , target :: String
  , replMode :: Bool}

argParser :: Parser ArgOptions
argParser = ArgOptions
     <$> strOption
         ( short 'i'
        <> long "input"
        <> metavar "INPUTPATH"
        <> value "stdin"
        <> help "path of input file" )
     <*> strOption
         ( short 'o'
        <> long "output"
        <> metavar "OUTPUTPATH"
        <> value "stdout"
        <> help "path to output" )
     <*> strOption
         ( short 't'
        <> long "target"
        <> metavar "TARGET"
        <> value "stdin"
        <> help "path of input program")
     <*> switch
         ( short 'r'
         <> long "repl"
         <> help "REPL mode")


repl :: Stmt -> Mem -> Bool -> IO ()
repl last_stmt mem has_history_instruction = do
  putStr "ki >>> "
  hFlush stdout -- IMPORTANT
  input <- getLine
  case words input of
    [] -> putStr ""
    -- excute the interpret
    ":i":others -> let program = Text.pack $ unwords others in
      let stmt_or_str = AttoText.parseOnly programParser program in do
      case stmt_or_str of
        -- syntax error
        (Left str) -> do -- error handy
          putStrLn $ "syntax error in interpret: " ++ Text.unpack program
          repl last_stmt mem has_history_instruction
        -- syntax right, eval the stmt, update stmt and memory, then run again
        (Right stmt) -> do
          let new_mem = eval stmt mem in do
            mempp new_mem
            repl stmt new_mem True
    -- output the parse result of last interpret
    ":t":others -> do
      if (has_history_instruction) then
        GenericPretty.pp last_stmt
      else
        putStrLn "No history instruction!"
      repl last_stmt mem has_history_instruction
    -- quit
    "q":others -> do
      putStrLn "Bye~"
    -- command syntax error
    otherwise -> do
      putStrLn $ "syntax error in input: " ++ input ++ ". We only support :i :t and :q"
      repl last_stmt mem has_history_instruction

interpret :: ArgOptions -> IO ()
interpret (ArgOptions _ _ _ True) = do
  putStrLn "Enter REPL mode"
  repl undefined Map.empty False
  return ()
interpret _ = return ()

main :: IO ()
main = execParser opts >>= interpret
  where
    opts = info (helper <*> argParser )
      ( fullDesc
     <> progDesc ""
     <> header "一个使用haskell写的解释器" )
