import Control.Monad
import Data.Char
import System.IO

main = forever $ do
  putStr ">>> "
  hFlush stdout
  l <- getLine
  putStrLn $ map toUpper l
