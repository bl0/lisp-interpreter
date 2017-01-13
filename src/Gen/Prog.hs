module Gen.Prog
    (
    gen_program
    ) where

-- my modules
import AST
import Gen.Func

gen_program :: Prog -> String
gen_program program = funcDefs ++ main
  where
    funcDefs = concat $ map gen_func program
    main = "print(\"result of program = %d\" %_main())"
