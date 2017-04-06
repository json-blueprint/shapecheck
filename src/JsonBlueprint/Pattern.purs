module JsonBlueprint.Pattern where

import Prelude
import Data.String as Str
import Data.Array (catMaybes, intercalate)
import Data.Either (Either(..))
import Data.Generic (class Generic, GenericSignature(..), GenericSpine(..), gEq)
import Data.List (concat, List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, flags, parseFlags, renderFlags, regex, source, test)
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

newtype GenRegex = GenRegex Regex

instance showGenRegex :: Show GenRegex where
  show (GenRegex r) = show r

instance genericRegex :: Generic GenRegex where
  toSignature proxy = SigProd "Data.String.Regex" [ { sigConstructor: "Data.String.Regex", sigValues: [ \_ -> SigString, \_ -> SigString ] } ]

  toSpine (GenRegex r) = SProd "Data.String.Regex" [
    \_ -> SString (source r),
    \_ -> SString (renderFlags $ flags r)]

  fromSpine (SProd "Data.String.Regex" [getPattern, getFlags]) = do
    p <- spine2String getPattern
    fs <- spine2String getFlags
    GenRegex <$> case regex p (parseFlags fs) of
      Right r -> Just r
      Left _  -> Nothing
    where
      spine2String lazySpine = case lazySpine unit of
        SString str -> Just str
        _           -> Nothing
  fromSpine _ = Nothing

data PropertyNamePattern = LiteralName String
                         | WildcardName { minLength :: Maybe Int, maxLength :: Maybe Int, pattern :: Maybe GenRegex }

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
             | StringDataType { minLength :: Maybe Int, maxLength :: Maybe Int, pattern :: Maybe GenRegex }
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

  show (StringDataType { minLength: Nothing, maxLength: Nothing, pattern: Just (GenRegex re) }) = show re
  show (StringDataType { minLength, maxLength, pattern }) = "String" <> showProps [
      prop "minLength" minLength,
      prop "maxLength" maxLength,
      prop "pattern"   pattern]

  show (Choice p1 p2) = showTerm p1 <> " | " <> showTerm p2
    where
      showTerm :: Pattern -> String
      showTerm p@(Property _) = "(" <> show p <> ")"
      showTerm p              = show p
  show (ArrayPattern ps) = "[" <> intercalate ", " (show <$> ps) <> "]"

  show (Group p1 p2) = "(" <> intercalate ", " (show <$> (flatten $ p1 : p2 : Nil)) <> ")"
    where
      flatten :: List Pattern -> List Pattern
      flatten Nil = Nil
      flatten (Cons (Group g1 g2) xs) = concat $ (flatten (g1 : g2 : Nil)) : (flatten xs) : Nil
      flatten (Cons x xs) = x : flatten xs

  show (Repeat p count) = showRepeated p <> show count
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

showProps :: Array (Maybe { name :: String, value :: String }) -> String
showProps xs =
  case printedProps of
    [] -> ""
    ps -> "(" <> intercalate ", " ps <> ")"
  where
    printedProps = (\p -> p.name <> " = " <> p.value) <$> catMaybes xs

prop :: forall a. Show a => String -> Maybe a -> Maybe { name :: String, value :: String }
prop n optV = { name: n, value: _ } <<< show <$> optV

derive instance genericPattern :: Generic Pattern

instance eqPattern :: Eq Pattern where
  eq = gEq
