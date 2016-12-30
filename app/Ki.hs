{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Options.Applicative
import qualified Data.Attoparsec.Text as AttoText
import qualified Data.Text as Text
import System.IO
import System.Exit
import qualified Data.Map as Map
import qualified Text.PrettyPrint as PrettyPrint
import qualified Text.PrettyPrint.GenericPretty as GenericPretty
import System.Console.Haskeline

-- my module
import Eval.Program
import Parser.Program
import AST
import Memory
import FileIO

data ArgOptions = ArgOptions
  { instructions :: String
  , output :: String
  , tree :: String
  , replMode :: Bool}

argParser :: Parser ArgOptions
argParser = ArgOptions
     <$> strOption
         ( short 'i'
        <> long "instructions"
        <> metavar "INPUT_PATH"
        <> value ""
        <> help "path of instructions to excute." )
     <*> strOption
         ( short 'o'
        <> long "output"
        <> metavar "OUTPUT_PATH"
        <> value ""
        <> help "path to output" )
     <*> strOption
         ( short 't'
        <> long "tree"
        <> metavar "INPUT_PATH"
        <> value ""
        <> help "path of program to parse")
     <*> switch
         ( short 'r'
         <> long "repl"
         <> help "REPL mode")

-- excute the interpret
excute_instruction :: Maybe Stmt -> Mem -> Text.Text -> InputT IO()
excute_instruction last_stmt mem program =
  let stmt_or_str = programParser program in do
  case stmt_or_str of
    -- syntax error
    (Left str) -> do -- error handy
      outputStrLn $ error_msg program str
      repl last_stmt mem
    -- syntax right, eval the stmt, update stmt and memory, then run again
    (Right stmt) -> do
      let new_mem = eval stmt mem in do
        outputStrLn $ mempp new_mem
        repl (Just stmt) new_mem

-- output the parse result of last interpret
show_tree :: Maybe Stmt -> Mem -> InputT IO()
show_tree Nothing mem = do
  outputStrLn "No history instruction!"
  repl Nothing mem
show_tree last_stmt mem = do
  outputStrLn $ PrettyPrint.render $ GenericPretty.doc $ last_stmt
  repl last_stmt mem

repl :: Maybe Stmt -> Mem -> InputT IO ()
repl last_stmt mem = do
  minput <- runInputT defaultSettings $ getInputLine "ki >>> "
  case minput of
    Nothing -> return ()
    Just input -> do
      case words input of
        ":i":others -> excute_instruction last_stmt mem $ Text.pack $ unwords others
        ":t":others -> show_tree last_stmt mem
        -- quit
        ":q":others -> outputStrLn "Bye~"
        -- command syntax error
        otherwise -> do
          outputStrLn $ "syntax error in: " ++ input ++ ". We only support :i :t and :q"
          repl last_stmt mem

interpret :: ArgOptions -> IO ()
-- repl
interpret (ArgOptions _ _ _ True) = do
  putStrLn "Enter REPL mode"
  runInputT defaultSettings $ repl Nothing Map.empty

-- excute instruction
interpret (ArgOptions i o "" False) = do
  program <- readWholeFile i
  case programParser program of
    -- syntax error
    (Left str) -> putStrLn $ error_msg program str
    -- syntax right, eval the statement
    (Right stmt) -> writeToFile o $ mempp (eval stmt Map.empty)

-- tree
interpret (ArgOptions "" o t False) = do
  program <- readWholeFile t
  case programParser program of
    -- syntax error
    (Left str) -> putStrLn $ error_msg program str
    -- syntax right, output the parse tree.
    (Right stmt) -> writeToFile o $ PrettyPrint.render $ GenericPretty.doc $ stmt
interpret _ = putStrLn "syntax error."

main :: IO ()
main = execParser opts >>= interpret
  where
    opts = info (helper <*> argParser )
      ( fullDesc
     <> progDesc ""
     <> header "一个使用haskell写的解释器" )
