module Main where

import Control.Applicative
import Control.Monad.State
import System.Environment
-- my modules
import Gen.Program
import FileIO
import Parser.Program

data Option = Option {
    inPath :: String,
    outPath :: String,
    help :: Bool
}
    deriving Show

type Parser a = StateT [String] Maybe a

parseFlag :: String -> Parser String
parseFlag f = do
    args <- get
    case args of
        [] -> empty
        (arg : args')
            | arg == "-" ++ f -> do
                put args'
                return f
            | otherwise -> empty

parseField :: Parser String
parseField = do
  args <- get
  case args of
      [] -> empty
      (arg : args') -> do
          put args'
          return arg

parseInPath :: Parser String
parseInPath = parseField

parseOutPath :: Parser String
parseOutPath = do
  parseFlag "o"
  parseField

parseHelp :: Parser String
parseHelp = parseFlag "h"

parseOption :: Parser Option
parseOption = ph <|> p0 <|> p1 where
    p0 = do
        i <- parseInPath
        o <- parseOutPath
        return (Option i o False)

    p1 = do
        o <- parseOutPath
        i <- parseInPath
        return (Option i o False)

    ph = do
      h <- parseHelp
      return (Option "" "" True)

helpMessage ="\n"
  ++ "Usage: kc [INPUT] [-o OUTPUT] [-h] \n"
  ++ "example:\n"
  ++ "  kc input.txt -o output.py\n"
  ++ "Avaliable options:\n"
  ++ "  -o   path to output\n"
  ++ "  -h   show this help message\n"

main :: IO ()
main = do
    args <- getArgs
    case runStateT parseOption args of
      Nothing -> putStrLn $ "Invalid argument " ++ unwords args ++ "\n" ++ helpMessage
      Just (Option _ _ True, _) -> putStrLn helpMessage
      Just (Option input output False, _) -> do
        program <- readWholeFile input
        case programParser program of
          -- syntax error
          (Left str) -> putStrLn $ error_msg program str
          -- syntax right, output the parse tree.
          (Right stmt) -> writeToFile output $ gen_program stmt
