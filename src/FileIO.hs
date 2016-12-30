module FileIO where
  -- (
  -- someFunc,
  -- eval
  -- )where

import qualified Data.Text as Text
import System.IO

readWholeFile :: FilePath -> IO Text.Text
readWholeFile path = do
  handle <- openFile path ReadMode
  content <- hGetContents handle
  return $ Text.pack content

writeToFile :: FilePath -> String -> IO ()
writeToFile "" content = putStrLn content
writeToFile path content = writeFile path content

error_msg :: Text.Text -> String -> String
error_msg program str =
  "syntax error in: " ++ Text.unpack program ++ "\nerror message: " ++ str
