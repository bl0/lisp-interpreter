module Main where

import Control.Monad
import Options.Applicative
import System.IO

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

repl :: IO ()
repl = forever $ do
  putStr "ki >>> "
  hFlush stdout
  input <- getLine
  let inp = words input in
    let cmd = (inp !! 0) in
      case cmd of
        ":i" -> putStrLn "i"
        ":t" -> putStrLn "t"
        otherwise -> putStrLn "wrong"
  -- if inp == ":i" then
  --   putStr "i"
  -- else if inp == ":q" then
  --   putStr "q"
  -- else if inp ==

interpret :: ArgOptions -> IO ()
interpret (ArgOptions _ _ _ True) = do
  putStr "int"
  repl
  return ()
interpret _ = return ()

main :: IO ()
main = execParser opts >>= interpret
  where
    opts = info (helper <*> argParser )
      ( fullDesc
     <> progDesc ""
     <> header "一个使用haskell写的解释器" )
