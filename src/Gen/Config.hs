module Gen.Config
    (
    gen_newline
    ) where

spacesToIdent = 2

gen_space :: Int -> String
gen_space ident = concat $ take (spacesToIdent * ident) $ repeat " "

gen_newline :: Int -> String -> String
gen_newline ident str = gen_space ident ++ str ++ "\n"
