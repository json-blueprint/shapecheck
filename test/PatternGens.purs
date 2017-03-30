module Test.PatternGens where

import Prelude
import Data.String (fromCharArray)
import Jack (Gen, arrayOf, chooseChar, chooseInt, elements, maybeOf, oneOf)
import JsonBlueprint.Pattern (Pattern(..))

genNonNegativeInt :: Gen Int
genNonNegativeInt = chooseInt 0 top

genString :: Gen String
genString = fromCharArray <$> arrayOf (chooseChar ' ' top)

genBooleanLiteral :: Gen Pattern
genBooleanLiteral = BooleanLiteral <$> elements [true, false]

genBooleanDataType :: Gen Pattern
genBooleanDataType = pure BooleanDataType

genStringLiteral :: Gen Pattern
genStringLiteral = StringLiteral <$> genString

genStringDataType :: Gen Pattern
genStringDataType = do
  lb <- maybeOf genNonNegativeInt
  ub <- maybeOf genNonNegativeInt
  pure $ StringDataType { minLength: lb, maxLength: ub }

genValuePattern :: Gen Pattern
genValuePattern = oneOf [
  genBooleanLiteral,
  genBooleanDataType,
  genStringLiteral,
  genStringDataType]
