{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Data.Attoparsec.Text
import qualified Data.Text as Text
import Data.Char

-- my modules
import Parser.Expr
import Parser.Stmt
import Parser.Prog
import Parser.Base
import Parser.Func
import AST
import Common

main :: IO ()
main = defaultMainWithOpts
       [
       -- base
         testCase     "Base.SkipComment" testSkipComment
       -- expr
       , testCase     "Expr.True" testTrueParser
       , testCase     "Expr.False" testFalseParser
       , testCase     "Expr.Not" testNotParser
       , testCase     "Expr.And" testAndParser
       , testCase     "Expr.Or" testOrParser
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
       , testCase     "Expr.Nil" testNilParser
       , testProperty "Expr.Car" testCarParser
       , testProperty "Expr.Cdr" testCdrParser
       , testProperty "Expr.Cons" testConsParser
       , testProperty "Expr.Char" testCharParser
       , testProperty "Expr.String" testStringParser
       , testCase     "Expr.String.Transferred" testTransferredStringParser
       , testProperty "Expr.VectorRef" testVectorRefParser
       , testProperty "Expr.Call" testCallParser
       , testProperty "Expr.Let" testLetParser
       , testProperty "Expr.Lambda" testLambdaParser
       , testProperty "Expr.LambdaCall" testLambdaCallParser
       , testCase     "Expr" testExprParser
       -- stmt
       , testCase     "Stmt.Skip" testSkipParser
       , testProperty "Stmt.VarSet" testVarSetParser
       , testCase     "Stmt.If" testIfParser
       , testCase     "Stmt.While" testWhileParser
       , testCase     "Stmt.StmtList" testStmtListParser
       , testCase     "Stmt.MakeVector" testMakeVectorParser
       , testCase     "Stmt.VectorSet" testVectorSetParser
       , testCase     "Stmt.Return" testReturnParser
       ] mempty

-- alias
-- expression parser
ep :: Text.Text -> Either String Expr
ep = parseOnly Parser.Expr.exprParser

-- statement parser
sp :: Text.Text -> Either String Stmt
sp = parseOnly Parser.Stmt.stmtParser

testSkipComment :: Assertion
testSkipComment = parseOnly skipComment "; comment\nsome text" @?= Right ()

testTrueParser :: Assertion
testTrueParser = ep "True" @?= Right TrueLit

testFalseParser :: Assertion
testFalseParser = ep "False" @?= Right FalseLit

-- testNotParser :: Assertion
-- testNotParser = ep "(not False)" @?= Right (Not FalseLit)
--
-- testAndParser :: Assertion
-- testAndParser = ep "(and False True)" @?= Right (And FalseLit TrueLit)
--
-- testOrParser :: Assertion
-- testOrParser = ep "(or False True)" @?= Right (Or FalseLit TrueLit)

testNotParser :: Assertion
testNotParser = allRight @?= True
  where
    allRight = c1 && c2
    c1 = ep "(not False)" == Right (Not FalseLit)
    c2 = ep "(not True)" == Right (Not TrueLit)

testAndParser :: Assertion
testAndParser = allRight @?= True
  where
    allRight = c1 && c2 && c3 && c4
    c1 = ep "(and False False)" == Right (And FalseLit FalseLit)
    c2 = ep "(and True False)" == Right (And TrueLit FalseLit)
    c3 = ep "(and False True)" == Right (And FalseLit TrueLit)
    c4 = ep "(and True True)" == Right (And TrueLit TrueLit)

testOrParser :: Assertion
testOrParser = allRight @?= True
  where
    allRight = c1 && c2 && c3 && c4
    c1 = ep "(or False False)" == Right (Or FalseLit FalseLit)
    c2 = ep "(or True False)" == Right (Or TrueLit FalseLit)
    c3 = ep "(or False True)" == Right (Or FalseLit TrueLit)
    c4 = ep "(or True True)" == Right (Or TrueLit TrueLit)

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
testVarParser var = allLetter var ==> result == truth
  where
    result = ep $ Text.unwords [Text.pack var]
    truth =  Right (VarRef var)

testNilParser :: Assertion
testNilParser = ep "nil" @?= Right Nil

testConsParser :: Char -> String -> Property
testConsParser c str = isCommonChar c && allLetter str ==> result == truth
  where
    result = ep $ Text.unwords ["(cons", Text.pack $ show c, Text.pack $ show str, ")"]
    truth =  Right $ Cons (CharLit c) (StringLit str)

testCarParser :: String -> Property
testCarParser var = allLetter var ==> result == truth
  where
    result = ep $ Text.unwords ["(car", Text.pack var, ")"]
    truth =  Right (Car (VarRef var))

testCdrParser :: String -> Property
testCdrParser var = allLetter var ==> result == truth
  where
    result = ep $ Text.unwords ["(cdr", Text.pack var, ")"]
    truth =  Right (Cdr (VarRef var))

testCharParser :: Char -> Property
testCharParser c = isCommonChar c ==> result == truth
  where
    result = ep $ Text.pack $ show c
    truth =  Right (CharLit c)

testStringParser :: String -> Property
testStringParser str = str == "" || allLetter str ==> result == truth
  where
    result = ep $ Text.pack $ show str
    truth =  Right (StringLit str)

testTransferredStringParser :: Assertion
testTransferredStringParser = ep (Text.pack $ show s) @?= Right (StringLit s)
  where
    s = "S\"tring"

testVectorRefParser :: String -> Int -> Property
testVectorRefParser var index = allLetter var ==> result == truth
  where
    result = ep $ Text.unwords ["(vector-ref",  Text.pack var, i2t index, ")"]
    truth =  Right (VectorRef var (i2slit index))

testCallParser :: FuncName -> Int -> Property
testCallParser funcName value = allLetter funcName ==> result == truth
  where
    result = ep $ Text.unwords ["(",  Text.pack funcName, i2t value, ")"]
    truth =  Right (Call funcName [i2slit value])

testLetParser :: String -> Int -> Int -> Property
testLetParser var n1 n2 = allLetter var ==> result == truth
  where
    result = ep $ Text.unwords ["(let", Text.pack var, i2t n1, i2t n2, ")"]
    truth =  Right (Let var (i2slit n1) (i2slit n2))

testLambdaParser :: String -> Int -> Property
testLambdaParser var n = allLetter var ==> result == truth
  where
    result = ep $ Text.unwords ["(lambda",  Text.pack var, i2t n, ")"]
    truth =  Right (Lambda var (i2slit n))

testLambdaCallParser :: String -> Int -> Property
testLambdaCallParser lambda n = allLetter lambda ==> result == truth
  where
    expression1 = Text.unwords ["(lambda", "a", i2t n, ")"]
    result = ep $ Text.unwords ["(",  expression1, i2t n, ")"]
    truth =  Right (LambdaCall (Lambda "a" (i2slit n)) (i2slit n))

testExprParser :: Assertion
testExprParser = result @?= truth
  where
    result = ep "(and (< 2 3) (not False))"
    truth = Right (And (Lt (ScientificLit 2) (ScientificLit 3)) (Not FalseLit))

-- statement
testSkipParser :: Assertion
testSkipParser = result @?= truth
  where
    result = parseOnly stmtParser "skip"
    truth = Right $ Skip

testVarSetParser :: String -> Double -> Property
testVarSetParser str n = allLetter str ==> result == truth
  where
    result = parseOnly stmtParser $ Text.unwords ["(set!", Text.pack str, d2t n, ")"]
    truth = Right $ VarSet str (d2slit n)

testIfParser :: Assertion
testIfParser = result @?= truth
  where
    result = parseOnly stmtParser "(if True skip skip)"
    truth = Right $ If TrueLit Skip Skip

testWhileParser :: Assertion
testWhileParser = result @?= truth
  where
    result = parseOnly stmtParser "(while True skip)"
    truth = Right $ While TrueLit Skip

testStmtListParser :: Assertion
testStmtListParser = result @?= truth
  where
    result = parseOnly stmtParser "(begin skip skip)"
    truth = Right $ StmtList [Skip, Skip]

testMakeVectorParser :: Assertion
testMakeVectorParser = result @?= truth
  where
    result = parseOnly stmtParser "(make-vector a 3)"
    truth = Right $ MakeVector "a" (i2slit 3)

testVectorSetParser :: Assertion
testVectorSetParser = result @?= truth
  where
    result = parseOnly stmtParser "(vector-set! a 1 True)"
    truth = Right $ VectorSet "a" (i2slit 1) TrueLit

testReturnParser :: Assertion
testReturnParser = result @?= truth
  where
    result = parseOnly stmtParser "(return 2333)"
    truth = Right $ Return (i2slit 2333)
