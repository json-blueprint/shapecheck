module JsonBlueprint.Pattern where

import Prelude
import Data.String as Str
import Data.Array (catMaybes, intercalate)
import Data.Generic (class Generic, gEq)
import Data.List (concat, List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import JsonBlueprint.Pattern (Pattern(..))

newtype RepeatCount = RepeatCount { min :: Int, max :: Maybe Int }

instance showRepeatCount :: Show RepeatCount where
  show (RepeatCount { min: 0, max: Just 1 })  = "?"
  show (RepeatCount { min: 0, max: Nothing }) = "*"
  show (RepeatCount { min: 1, max: Nothing }) = "+"
  show (RepeatCount { min, max: Nothing }) = "{" <> show min <> ",}"
  show (RepeatCount { min, max: Just max }) | min == max = "{" <> show min <> "}"
                                            | otherwise = "{" <> show min <> ", " <> show max <> "}"

derive instance genericRepeatCount :: Generic RepeatCount

data PropertyNamePattern = LiteralName String
                         | WildcardName { minLength :: Maybe Int, maxLength :: Maybe Int }

simplePropName :: Regex
simplePropName = unsafeRegex "^[a-z_][a-z\\d_]*$" ignoreCase

propNameRequiresQuoting :: String -> Boolean
propNameRequiresQuoting = not $ test simplePropName

instance showPropertyNamePattern :: Show PropertyNamePattern where
  show (LiteralName name) | propNameRequiresQuoting name = show name
                          | otherwise = name
  show (WildcardName props) = show $ StringDataType props

derive instance genericPropertyNamePattern :: Generic PropertyNamePattern

data Pattern = Empty
             | BooleanLiteral Boolean
             | BooleanDataType
             | StringLiteral String
             | StringDataType { minLength :: Maybe Int, maxLength :: Maybe Int }
             | Choice Pattern Pattern
             | Group Pattern Pattern
             | Repeat Pattern RepeatCount
             | ArrayPattern (List Pattern)
             | Property { name :: PropertyNamePattern, value :: Pattern }
             | Object (List Pattern)

group :: Pattern -> Pattern -> Pattern
group Empty p2 = p2
group p1 Empty = p1
group p1 p2    = Group p1 p2

instance showPattern :: Show Pattern where
  show Empty = "Empty"
  show (BooleanLiteral b)   = show b
  show BooleanDataType      = "Boolean"
  show (StringLiteral str)  = show str
  show (StringDataType { minLength, maxLength }) = "String" <> showProps [
      prop "minLength" minLength,
      prop "maxLength" maxLength
    ]
  show (Choice p1 p2)       = showTerm p1 <> " | " <> showTerm p2
    where
      showTerm :: Pattern -> String
      showTerm p@(Property _) = "(" <> show p <> ")"
      showTerm p              = show p
  show (ArrayPattern ps)    = "[" <> intercalate ", " (show <$> ps) <> "]"

  show (Group p1 p2)        = "(" <> intercalate ", " (show <$> (flatten $ p1 : p2 : Nil)) <> ")"
    where
      flatten :: List Pattern -> List Pattern
      flatten Nil = Nil
      flatten (Cons (Group g1 g2) xs) = concat $ (flatten (g1 : g2 : Nil)) : (flatten xs) : Nil
      flatten (Cons x xs) = x : flatten xs

  show (Repeat p count)               = showRepeated p <> show count
    where
      showRepeated :: Pattern -> String
      showRepeated (Property _) = "(" <> show p <> ")"
      showRepeated (Choice _ _) = "(" <> show p <> ")"
      showRepeated _            = show p

  show (Property { name, value }) = show name <> ": " <> show value
  show (Object Nil)               = "{}"
  show (Object props)             = "{\n  " <> indent (intercalate ",\n" (show <$> props)) <> "\n}" where
    indent :: String -> String
    indent = Str.replaceAll (Str.Pattern "\n") (Str.Replacement "\n  ")

showProps :: forall a. Show a => Array (Maybe { name :: String, value :: a }) -> String
showProps xs =
  case printedProps of
    [] -> ""
    ps -> "(" <> intercalate ", " ps <> ")"
  where
    printedProps = (\p -> p.name <> " = " <> show p.value) <$> catMaybes xs

prop :: forall a. String -> Maybe a -> Maybe { name :: String, value :: a }
prop n optV = { name: n, value: _ } <$> optV

derive instance genericPattern :: Generic Pattern

instance eqPattern :: Eq Pattern where
  eq = gEq
