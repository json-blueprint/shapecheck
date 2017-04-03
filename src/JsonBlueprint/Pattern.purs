module JsonBlueprint.Pattern where

import Prelude
import Data.Array (catMaybes, intercalate)
import Data.Generic (class Generic, gEq)
import Data.Maybe (Maybe)
import JsonBlueprint.Pattern (Pattern(..))

data Pattern = Empty
             | BooleanLiteral Boolean
             | BooleanDataType
             | StringLiteral String
             | StringDataType { minLength :: Maybe Int, maxLength :: Maybe Int }
             | Choice Pattern Pattern
             | ArrayPattern (Array Pattern)

instance showPattern :: Show Pattern where
  show Empty = "Empty"
  show (BooleanLiteral b)   = show b
  show BooleanDataType      = "Boolean"
  show (StringLiteral str)  = show str
  show (StringDataType { minLength, maxLength }) = "String" <> showProps [
      prop "minLength" minLength,
      prop "maxLength" maxLength
    ]
  show (Choice p1 p2)       = (show p1) <> " | " <> (show p2)
  show (ArrayPattern ps)    = "[" <> intercalate ", " (show <$> ps) <> "]"

showProps :: forall a. Show a => Array (Maybe { name :: String, value :: a }) -> String
showProps xs =
  case printedProps of
    [] -> ""
    ps -> "(" <> intercalate ", " ps <> ")"
  where
    printedProps = (\p -> p.name <> " = " <> show p.value) <$> catMaybes xs

prop :: forall a. String -> Maybe a -> Maybe { name :: String, value :: a }
prop n optV = { name: n, value: _ } <$> optV

-- or :: Pattern -> Pattern -> Pattern
-- or = Choice

derive instance genericPattern :: Generic Pattern

instance eqPattern :: Eq Pattern where
  eq = gEq
