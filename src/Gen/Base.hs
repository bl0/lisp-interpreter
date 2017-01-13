module Gen.Base
    (
      gen_newline
    , gen_parent
    , gen_brackets
    , gen_plist
    , gen_var
    ) where

spacesToIdent = 2

gen_space :: Int -> String
gen_space ident = concat $ take (spacesToIdent * ident) $ repeat " "

gen_newline :: Int -> String -> String
gen_newline ident str = gen_space ident ++ str ++ "\n"

gen_parent :: String -> String
gen_parent s = "(" ++ s ++ ")"

gen_brackets :: String -> String
gen_brackets s = "[" ++ s ++ "]"

gen_plist :: [String] -> String
gen_plist [] = "()"
-- gen_plist (str:[]) = str
gen_plist strList = gen_parent $ foldl1 (\s1 s2 -> (s1 ++ ", " ++ s2)) strList

gen_var :: String -> String
gen_var var = "_" ++ var
