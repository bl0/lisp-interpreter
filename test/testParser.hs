{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Data.Attoparsec.Text
import qualified Data.Text as Text

-- my modules
import Parser.Expr
import Parser.Program
import AST
import Common

main :: IO ()
main = defaultMainWithOpts
       [
       -- expr
         testCase "Expr.True" testTrueParser
       , testCase "Expr.False" testFalseParser
       , testCase "Expr.Not" testNotParser
       , testCase "Expr.And" testAndParser
       , testCase "Expr.Or" testOrParser
       , testProperty "Expr.Number" testNumberParser
       , testProperty "Expr.Add" testAddParser
       , testProperty "Expr.Sub" testSubParser
       , testProperty "Expr.Mult" testMultParser
       , testProperty "Expr.Div" testDivParser
       , testProperty "Expr.EQ" testEQParser
       , testProperty "Expr.Lt" testLtParser
       , testProperty "Expr.Le" testLeParser
       , testProperty "Expr.Gt" testGtParser
       , testProperty "Expr.Ge" testGeParser
       , testProperty "Expr.Var" testVarParser
       , testCase "Expr" testExprParser
       -- stmt
       , testCase "Stmt.Skip" testSkipParser
       , testProperty "Stmt.VarSet" testVarSetParser
       , testCase "Stmt.If" testIfParser
       , testCase "Stmt.While" testWhileParser
       , testCase "Stmt.StmtList" testStmtListParser
       ] mempty

-- alias
-- expression parser
ep :: Text.Text -> Either String Expr
ep = parseOnly Parser.Expr.exprParser

testTrueParser :: Assertion
testTrueParser = ep "True" @?= Right TrueLit

testFalseParser :: Assertion
testFalseParser = ep "False" @?= Right FalseLit

testNotParser :: Assertion
testNotParser = ep "(not False)" @?= Right (Not FalseLit)

testAndParser :: Assertion
testAndParser = ep "(and False True)" @?= Right (And FalseLit TrueLit)

testOrParser :: Assertion
testOrParser = ep "(or False True)" @?= Right (Or FalseLit TrueLit)

testNumberParser :: Double -> Property
testNumberParser n = True ==> result == truth
  where
    result = ep $ d2t n
    truth =  Right (d2slit n)

testAddParser :: Double -> Double -> Property
testAddParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(+", d2t n1, d2t n2, ")"]
    truth =  Right (Add (d2slit n1) (d2slit n2))

testSubParser :: Double -> Double -> Property
testSubParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(-", d2t n1, d2t n2, ")"]
    truth =  Right (Sub (d2slit n1) (d2slit n2))

testMultParser :: Double -> Double -> Property
testMultParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(*", d2t n1, d2t n2, ")"]
    truth =  Right (Mult (d2slit n1) (d2slit n2))

testDivParser :: Double -> Double -> Property
testDivParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(/", d2t n1, d2t n2, ")"]
    truth =  Right (Div (d2slit n1) (d2slit n2))

testEQParser :: Double -> Double -> Property
testEQParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(=", d2t n1, d2t n2, ")"]
    truth =  Right (Eq (d2slit n1) (d2slit n2))

testLtParser :: Double -> Double -> Property
testLtParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(<", d2t n1, d2t n2, ")"]
    truth =  Right (Lt (d2slit n1) (d2slit n2))

testLeParser :: Double -> Double -> Property
testLeParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(<=", d2t n1, d2t n2, ")"]
    truth =  Right (Le (d2slit n1) (d2slit n2))

testGtParser :: Double -> Double -> Property
testGtParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(>", d2t n1, d2t n2, ")"]
    truth =  Right (Gt (d2slit n1) (d2slit n2))

testGeParser :: Double -> Double -> Property
testGeParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(>=", d2t n1, d2t n2, ")"]
    truth =  Right (Ge (d2slit n1) (d2slit n2))

testVarParser :: String -> Property
testVarParser str = allLetter str ==> result == truth
  where
    result = ep $ Text.unwords [Text.pack str]
    truth =  Right (VarRef str)

testExprParser :: Assertion
testExprParser = result @?= truth
  where
    result = ep "(and (< 2 3) (not False))"
    truth = Right (And (Lt (ScientificLit 2) (ScientificLit 3)) (Not FalseLit))

testSkipParser :: Assertion
testSkipParser = result @?= truth
  where
    result = programParser "skip"
    truth = Right $ Skip

testVarSetParser :: String -> Double -> Property
testVarSetParser str n = allLetter str ==> result == truth
  where
    result = programParser $ Text.unwords ["(set!", Text.pack str, d2t n, ")"]
    truth = Right $ VarSet str (d2slit n)

testIfParser :: Assertion
testIfParser = result @?= truth
  where
    result = programParser "(if True skip skip)"
    truth = Right $ If TrueLit Skip Skip

testWhileParser :: Assertion
testWhileParser = result @?= truth
  where
    result = programParser "(while True skip)"
    truth = Right $ While TrueLit Skip

testStmtListParser :: Assertion
testStmtListParser = result @?= truth
  where
    result = programParser "(begin skip skip)"
    truth = Right $ StmtList [Skip, Skip]
