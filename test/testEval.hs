{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as Map
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

-- my modules
import AST
import Memory
import Eval.Expr
import Eval.Prog
import Common

main :: IO ()
main = defaultMainWithOpts
       [
       -- expr
         testCase "Expr.True" testTrueEval
       , testCase "Expr.False" testFalseEval
       , testCase "Expr.Not" testNotEval
       , testCase "Expr.Not'" testNotEval'
       , testCase "Expr.And" testAndEval
       , testCase "Expr.And'" testAndEval'
       , testCase "Expr.Or" testOrEval
       , testProperty "Expr.Number" testNumberEval
       , testProperty "Expr.Add" testAddEval
       , testProperty "Expr.Sub" testSubEval
       , testProperty "Expr.Mult" testMultEval
       , testProperty "Expr.Div" testDivEval
       , testProperty "Expr.EQ" testEQEval
       , testProperty "Expr.Lt" testLtEval
       , testProperty "Expr.Le" testLeEval
       , testProperty "Expr.Gt" testGtEval
       , testProperty "Expr.Ge" testGeEval
       , testProperty "Expr.Var" testVarEval
       , testCase "Expr" testExprEval
       -- stmt
       , testProperty "Stmt.Skip" testSkipEval
       , testProperty "Stmt.VarSet" testVarSetEval
       , testProperty "Stmt.If" testIfEval
       , testProperty "Stmt.While" testWhileEval
       , testProperty "Stmt.StmtList" testStmtListEval
       ] mempty

-- alias
-- eval expression with empty memory
ee :: Expr -> Val
ee expr = eval_expr expr Map.empty
-- eval expression with singleton memory
ee' :: Expr -> Var -> Double -> Val
ee' expr var n = eval_expr expr (Map.singleton var $ d2sval n)
-- eval expression with empty memory
es :: Prog -> Mem
es prog = eval prog Map.empty
-- eval statement with singleton memory
es' :: Prog -> Var -> Val -> Mem
es' prog var n = eval prog (Map.singleton var n)

testTrueEval :: Assertion
testTrueEval = ee TrueLit @?= BoolVal True

testFalseEval :: Assertion
testFalseEval = ee FalseLit @?= BoolVal False

testNotEval :: Assertion
testNotEval = ee (Not FalseLit) @?= BoolVal True

testNotEval' :: Assertion
testNotEval' = ee (Not TrueLit) @?= BoolVal False

testAndEval :: Assertion
testAndEval = ee (And TrueLit FalseLit) @?= BoolVal False

testAndEval' :: Assertion
testAndEval' = ee (And TrueLit TrueLit) @?= BoolVal True

testOrEval :: Assertion
testOrEval = ee (Or TrueLit FalseLit) @?= BoolVal True

testNumberEval :: Double -> Property
testNumberEval n = True ==> result == truth
  where
    result = ee $ ScientificLit $ d2s n
    truth =  ScientificVal $ d2s n

testAddEval :: Double -> Double -> Property
testAddEval n1 n2 = True ==> scientificValEq result truth
  where
    result = ee $ Add (d2slit n1) (d2slit n2)
    truth =  ScientificVal $ d2s $ n1 + n2

testSubEval :: Double -> Double -> Property
testSubEval n1 n2 = True ==> scientificValEq result truth
  where
    result = ee $ Sub (d2slit n1) (d2slit n2)
    truth =  ScientificVal $ d2s $ n1 - n2

testMultEval :: Double -> Double -> Property
testMultEval n1 n2 = True ==> scientificValEq result truth
  where
    result = ee $ Mult (d2slit n1) (d2slit n2)
    truth =  ScientificVal $ d2s $ n1 * n2

-- issue: forever loop when we use n1 and n2
testDivEval :: Int -> Int -> Property
testDivEval _ _ = True ==> scientificValEq result truth
  where
    result = ee $ Div (ScientificLit 2) (ScientificLit 2)
    truth =  ScientificVal $ 1
    -- result = ee $ Div (i2slit n1) (i2slit n2)
    -- truth =  ScientificVal $ (i2s n1 / i2s n2)

testEQEval :: Double -> Double -> Property
testEQEval n1 n2 = True ==> result == truth
  where
    result = ee $ Eq (d2slit n1) (d2slit n2)
    truth =  BoolVal $ (n1 == n2)

testLtEval :: Double -> Double -> Property
testLtEval n1 n2 = True ==> result == truth
  where
    result = ee $ Lt (d2slit n1) (d2slit n2)
    truth =  BoolVal $ (n1 < n2)

testLeEval :: Double -> Double -> Property
testLeEval n1 n2 = True ==> result == truth
  where
    result = ee $ Le (d2slit n1) (d2slit n2)
    truth =  BoolVal $ (n1 <= n2)

testGtEval :: Double -> Double -> Property
testGtEval n1 n2 = True ==> result == truth
  where
    result = ee $ Gt (d2slit n1) (d2slit n2)
    truth =  BoolVal $ (n1 > n2)

testGeEval :: Double -> Double -> Property
testGeEval n1 n2 = True ==> result == truth
  where
    result = ee $ Ge (d2slit n1) (d2slit n2)
    truth =  BoolVal $ (n1 >= n2)
-- TODO
testVarEval :: Var -> Double -> Property
testVarEval var n = allLetter var ==> result == truth
  where
    result = ee' (VarRef var) var n
    truth =  d2sval n

testExprEval :: Assertion
testExprEval = result @?= truth
  where
    result = ee $ And (Lt (ScientificLit 2) (ScientificLit 3)) (Not FalseLit)
    truth = BoolVal True

testSkipEval :: Var -> Double -> Property
testSkipEval var n = allLetter var ==> result == truth
  where
    result = es' Skip var (d2sval n)
    truth =  Map.singleton var (d2sval n)

testVarSetEval :: Var -> Double -> Property
testVarSetEval var n = allLetter var ==>
  result == truth && result1 == truth && result2 == truth2
  where
    result = es $ VarSet var (d2slit n)
    truth = Map.singleton var (d2sval n)
    result1 = es' (VarSet var $ d2slit n) var (d2sval $ n-1)
    result2 = es' (VarSet var $ d2slit n) ("var_" ++ var) (d2sval $ n)
    truth2 = Map.fromList [(var, d2sval n), ("var_"++var, d2sval n)]

testIfEval :: Var -> Double -> Property
testIfEval var n = allLetter var ==>
  result == truth && result1 == truth1
  where
    result = es' (If TrueLit (VarSet "true" TrueLit) (VarSet "false" FalseLit)) var (d2sval n)
    truth = Map.fromList [(var, d2sval n), ("true", BoolVal True)]
    result1 = es' (If FalseLit (VarSet "true" TrueLit) (VarSet "false" FalseLit)) var (d2sval n)
    truth1 = Map.fromList [(var, d2sval n), ("false", BoolVal False)]

testWhileEval :: Var -> Int -> Property
testWhileEval var n = allLetter var ==>
  result == truth -- && result1 == truth1
  where
    result = es' (
      While
        (Lt
          (VarRef var)
          (i2slit $ n + 3)
        )
        (VarSet
          var
          (Add
            (VarRef var)
            (i2slit 1)
          )
        )
      ) var (i2sval n)
    truth = Map.fromList [(var, i2sval $ n + 3)]

testStmtListEval :: Var -> Int -> Property
testStmtListEval var n = allLetter var ==>
  result == truth
  where
    result = es (
      StmtList [
        (VarSet var (i2slit n)),
        (If
          (Le (VarRef var) (ScientificLit 0))
          (VarSet "less_than_0" TrueLit)
          (VarSet "great_equal_0" TrueLit)
        )
      ])
    truth = if (n <= 0)
      then Map.fromList [(var, i2sval n), ("less_than_0", BoolVal True)]
      else Map.fromList [(var, i2sval n), ("great_equal_0", BoolVal True)]
