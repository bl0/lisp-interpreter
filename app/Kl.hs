module Main where

import Parser.Expr
import Compiler.Codegen
import Compiler.Emit

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST
import qualified AST as S
import qualified Data.Text as Text

initModule :: AST.Module
initModule = emptyModule "my first llvm program"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = expressionParser $ Text.pack source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: FilePath -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname] -> processFile fname >> return ()
