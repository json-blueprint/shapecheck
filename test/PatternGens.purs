module Test.PatternGens where

import Prelude
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Jack (Gen, arrayOf, chooseChar, chooseInt, elements, frequency, maybeOf, oneOf)
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

genNonChoiceValue :: Gen Pattern
genNonChoiceValue = oneOf [
  genBooleanLiteral,
  genBooleanDataType,
  genStringLiteral,
  genStringDataType]

genValueChoice :: Gen Pattern
genValueChoice = do
  first <- genNonChoiceValue
  second <- oneOf [genNonChoiceValue, genValueChoice]
  pure $ Choice first second

genValuePattern :: Gen Pattern
genValuePattern = frequency [
  Tuple 9 genNonChoiceValue,
  Tuple 1 genValueChoice]
