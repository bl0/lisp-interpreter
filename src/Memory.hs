module Memory (
    Var
  , Val(..)
  , Mem
  , FuncName
  , memLookup
  , mempp
  , get_bool
  , get_scientific
  , get_list
  , get_vec
  , get_int
  , get_varList
  , get_stmt
  , func2tuple
  , insertFunc
  )where

import qualified Data.Map as Map
import Data.List (transpose, intercalate)
import Data.Scientific
import qualified Data.Vector as Vector
import AST

type Vec = Vector.Vector Val

-- Different kinds of Expr evaluate to different Val
data Val

  -- If you support other kinds of expressions, add the cases by yourself
  = BoolVal Bool
  | ScientificVal Scientific
  | ListVal [Val]
  | CharVal Char
  | VectorVal Vec
  | Undefined
  | FunctionVal [Var] Stmt
  | LambdaVal Var Expr
  deriving (Read, Eq, Ord)

instance Show Val where
  show (BoolVal bool) = show bool
  show (ScientificVal scientific) = show scientific
  show (ListVal listVal) = show listVal
  show (CharVal char) = show char
  show (VectorVal vec) = show vec
  show (Undefined) = "Undefined"
  show (FunctionVal varList stmt) = "Function:\\ " ++ show varList ++ " -> " ++ show stmt
  show (LambdaVal var expr) = "Anonymous_function:\\" ++ var ++ " -> " ++ show expr

-- A memory is a mapping from variable names to values
type Mem = Map.Map Var Val

memLookup :: String -> Mem -> Val
memLookup var mem = case Map.lookup var mem of
  Nothing -> error $ "error: var " ++ var ++ " does not exist in mem."
  Just v  -> v

get_bool :: Val -> Bool
get_bool (BoolVal b) = b
get_bool val = error $ "error: " ++ show(val) ++ "is not a BoolVal"

get_scientific :: Val -> Scientific
get_scientific (ScientificVal n) = n
get_scientific val = error $ "error: " ++ show(val) ++ "is not a ScientificVal"

get_int :: Val -> Int
get_int (ScientificVal n) = case floatingOrInteger n of
  (Left f) -> error $ "error: " ++ show(n) ++ "is a float."
  (Right n) -> n
get_int val = error $ "error: " ++ show(val) ++ "is not a integer."

get_list :: Val -> [Val]
get_list (ListVal l) = l
get_list val = error $ "error: " ++ show(val) ++ "is not a ListVal"

get_vec :: Val -> Vec
get_vec (VectorVal vec) = vec
get_vec val = error $ "error: " ++ show(val) ++ "is not a VectorVal"

get_varList :: Val -> [Var]
get_varList (FunctionVal varList _) = varList
get_varList val = error $ "error: " ++ show(val) ++ "is not a FunctionVal"

get_stmt :: Val -> Stmt
get_stmt (FunctionVal _ stmt) = stmt
get_stmt val = error $ "error: " ++ show(val) ++ "is not a FunctionVal"

func2tuple :: Func -> (FuncName, Val)
func2tuple (Function name varList stmt) = (name, FunctionVal varList stmt)

insertFunc :: Func -> Mem -> Mem
insertFunc (Function name varList stmt) mem = Map.insert name (FunctionVal varList stmt) mem
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

mempp :: Mem -> String
mempp mem = showTable [ ColDesc center "variable name"  left  var
                     , ColDesc center "value" left  val
                     ] (map2ColDesc mem)
