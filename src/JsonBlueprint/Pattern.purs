module JsonBlueprint.Pattern where

import Prelude
import Data.Generic (class Generic, gEq)
import Data.Maybe (Maybe)
import JsonBlueprint.Pattern (Pattern(..))
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, oneOf)

data Pattern = Empty
             | BooleanLiteral Boolean
             | BooleanDataType
             | StringLiteral String
             | StringDataType { minLength :: Maybe Int, maxLength :: Maybe Int }
--             | Choice Pattern Pattern

instance showPattern :: Show Pattern where
  show Empty = "Empty"
  show (BooleanLiteral b)    = show b
  show BooleanDataType       = "Boolean"
  show (StringLiteral str)   = show str
  show (StringDataType opts) = "String"           -- TODO: append props
--  show (Choice p1 p2)        = (show p1) <> " | " <> (show p2)

-- or :: Pattern -> Pattern -> Pattern
-- or = Choice

derive instance genericPattern :: Generic Pattern

instance eqPattern :: Eq Pattern where
  eq = gEq

genBooleanLiteral :: Gen Pattern
genBooleanLiteral = BooleanLiteral <$> arbitrary

genStringLiteral :: Gen Pattern
genStringLiteral = StringLiteral <$> arbitrary

instance arbPattern :: Arbitrary Pattern where
  arbitrary = oneOf genBooleanLiteral [genStringLiteral]
