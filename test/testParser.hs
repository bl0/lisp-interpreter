{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Data.Attoparsec.Text
import Data.Functor
import qualified Data.Text as Text
import Data.Scientific

-- my modules
import Parser.Expr
import Parser.Program
import AST
import Memory

main :: IO ()
main = defaultMainWithOpts
       [ testCase "Expr.True" testTrueParser
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
       ] mempty

-- alias
ep = parseOnly Parser.Expr.exprParser
-- double to Text
d2t = Text.pack . show
-- double to ScientificLit
d2slit = ScientificLit . fromFloatDigits

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
    result = ep $ Text.unwords ["(+ ", d2t n1, d2t n2, ")"]
    truth =  Right (Add (d2slit n1) (d2slit n2))

testSubParser :: Double -> Double -> Property
testSubParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(- ", d2t n1, d2t n2, ")"]
    truth =  Right (Sub (d2slit n1) (d2slit n2))

testMultParser :: Double -> Double -> Property
testMultParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(* ", d2t n1, d2t n2, ")"]
    truth =  Right (Mult (d2slit n1) (d2slit n2))

testDivParser :: Double -> Double -> Property
testDivParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(/ ", d2t n1, d2t n2, ")"]
    truth =  Right (Div (d2slit n1) (d2slit n2))

testEQParser :: Double -> Double -> Property
testEQParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(= ", d2t n1, d2t n2, ")"]
    truth =  Right (Eq (d2slit n1) (d2slit n2))

testLtParser :: Double -> Double -> Property
testLtParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(< ", d2t n1, d2t n2, ")"]
    truth =  Right (Lt (d2slit n1) (d2slit n2))

testLeParser :: Double -> Double -> Property
testLeParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(<= ", d2t n1, d2t n2, ")"]
    truth =  Right (Le (d2slit n1) (d2slit n2))

testGtParser :: Double -> Double -> Property
testGtParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(> ", d2t n1, d2t n2, ")"]
    truth =  Right (Gt (d2slit n1) (d2slit n2))

testGeParser :: Double -> Double -> Property
testGeParser n1 n2 = True ==> result == truth
  where
    result = ep $ Text.unwords ["(>= ", d2t n1, d2t n2, ")"]
    truth =  Right (Ge (d2slit n1) (d2slit n2))
