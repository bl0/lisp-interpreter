module Memory (
    Var
  , Val(BoolVal, ScientificVal)
  , Mem
  , mempp
  )where

import qualified Data.Map as Map
import Data.List (transpose, intercalate)
import Data.Scientific


-- We need to represent a variable name
type Var = String

-- Different kinds of Expr evaluate to different Val
data Val

  -- If you support other kinds of expressions, add the cases by yourself
  = BoolVal Bool
  | ScientificVal Scientific
  deriving (Show, Read, Eq, Ord)

-- A memory is a mapping from variable names to values
type Mem = Map.Map Var Val



-- pretty printer
-- a type for records
data T = T { var  :: String
           , val :: String}
    deriving Show

-- a type for fill functions
type Filler = Int -> String -> String

-- a type for describing table columns
data ColDesc t = ColDesc { colTitleFill :: Filler
                         , colTitle     :: String
                         , colValueFill :: Filler
                         , colValue     :: t -> String
                         }

-- functions that fill a string (s) to a given width (n) by adding pad
-- character (c) to align left, right, or center
fillLeft c n s = s ++ replicate (n - length s) c
fillRight c n s = replicate (n - length s) c ++ s
fillCenter c n s = replicate l c ++ s ++ replicate r c
    where x = n - length s
          l = x `div` 2
          r = x - l

-- functions that fill with spaces
left = fillLeft ' '
right = fillRight ' '
center = fillCenter ' '

-- converts a list of items into a table according to a list
-- of column descriptors
showTable :: [ColDesc t] -> [t] -> String
showTable cs ts =
    let header = map colTitle cs
        rows = [[colValue c t | c <- cs] | t <- ts]
        widths = [maximum $ map length col | col <- transpose $ header : rows]
        separator = intercalate "-+-" [replicate width '-' | width <- widths]
        fillCols fill cols = intercalate " | " [fill c width col | (c, width, col) <- zip3 cs widths cols]
    in
        unlines $ fillCols colTitleFill header : separator : map (fillCols colValueFill) rows

tuple2T :: (String, Val) -> T
tuple2T tup = T (fst tup) (show $ snd tup)

map2ColDesc :: Mem -> [T]
map2ColDesc mem = map tuple2T (Map.toAscList mem)

mempp :: Mem -> IO()
mempp mem = putStrLn $ showTable [ ColDesc center "variable name"  left  var
                     , ColDesc center "value" left  val
                     ] (map2ColDesc mem)
