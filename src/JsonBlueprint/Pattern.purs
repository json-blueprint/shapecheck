module JsonBlueprint.Pattern where

import Prelude
import Data.Array (catMaybes, intercalate)
import Data.Generic (class Generic, gEq)
import Data.List (concat, List(..), (:))
import Data.Maybe (Maybe(..))
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

data Pattern = Empty
             | BooleanLiteral Boolean
             | BooleanDataType
             | StringLiteral String
             | StringDataType { minLength :: Maybe Int, maxLength :: Maybe Int }
             | Choice Pattern Pattern
             | Group Pattern Pattern
             | Repeat Pattern RepeatCount
             | ArrayPattern (List Pattern)

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
  show (Choice p1 p2)       = (show p1) <> " | " <> (show p2)
  show (ArrayPattern ps)    = "[" <> intercalate ", " (show <$> ps) <> "]"
  show (Group p1 p2)        = "(" <> intercalate ", " (show <$> (flatten $ p1 : p2 : Nil)) <> ")"
    where
      flatten :: List Pattern -> List Pattern
      flatten Nil = Nil
      flatten (Cons (Group g1 g2) xs) = concat $ (flatten (g1 : g2 : Nil)) : (flatten xs) : Nil
      flatten (Cons x xs) = x : flatten xs
  show (Repeat ch@(Choice _ _) count) = "(" <> show ch <> ")" <> show count
  show (Repeat p count)               = show p <> show count

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
