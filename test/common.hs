module Common where

-- my modules
import AST
import qualified Data.Text as Text
import Data.Scientific
import Data.Char
import Memory

-- double to Text
d2t :: Double -> Text.Text
d2t = Text.pack . show
-- double to ScientificLit
d2slit :: Double -> Expr
d2slit = ScientificLit . fromFloatDigits
-- double to Scientific
d2s :: Double -> Scientific
d2s = fromFloatDigits
-- double to ScientificVal
d2sval :: Double -> Val
d2sval = ScientificVal . fromFloatDigits
-- int to Text
i2t :: Int -> Text.Text
i2t = Text.pack . show
-- int to Scientific
i2s :: Int -> Scientific
i2s n = scientific (toInteger n) 0
-- int to ScientificLit
i2slit :: Int -> Expr
i2slit = ScientificLit . i2s
-- int to ScientificVal
i2sval :: Int -> Val
i2sval = ScientificVal . i2s

allLetter :: String -> Bool
allLetter str = (str /= "") && (all isLetter str)

scientificValEq :: Val -> Val -> Bool
scientificValEq (ScientificVal n1) (ScientificVal n2) = (n1 - n2 < 1e-9)
