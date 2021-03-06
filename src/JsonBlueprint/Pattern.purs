module JsonBlueprint.Pattern (
  Bound(..),
  emptyNumericDtProps,
  emptyStringDtProps,
  filterGroup,
  flattenChoice,
  flattenGroup,
  foldChoice,
  foldGroup,
  GenRegex(..),
  group,
  NumericDtProps(..),
  ObjectRefinement(..),
  Pattern(..),
  PropertyNamePattern(..),
  propertyNamePatterns,
  propertyNames,
  propNameRequiresQuoting,
  RepeatCount(..),
  StringDtProps(..),
  walk
) where

import Prelude
import Data.Array as Array
import Data.CatList as CatList
import Data.Int as Int
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Set as Set
import Data.String as Str
import Data.Array (catMaybes, intercalate)
import Data.CatList (CatList)
import Data.Either (Either(..))
import Data.Generic (class Generic, GenericSignature(..), GenericSpine(..), gEq)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Set (Set)
import Data.String.Regex (Regex, flags, parseFlags, renderFlags, regex, source, test)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import JsonBlueprint.Pattern (Pattern(..))

-- | Represents suffix that can be added to a pattern that can either be repeated
-- | or which is optional. Standard patterns without explicit repetition count are
-- | equivalent to "exactly once" ({1}) repetition
newtype RepeatCount = RepeatCount { min :: Int, max :: Maybe Int }

instance showRepeatCount :: Show RepeatCount where
  show (RepeatCount { min: 1, max: Just 1 }) = ""
  show (RepeatCount { min: 0, max: Just 1 })  = "?"
  show (RepeatCount { min: 0, max: Nothing }) = "*"
  show (RepeatCount { min: 1, max: Nothing }) = "+"
  show (RepeatCount { min, max: Nothing }) = "{" <> show min <> ",}"
  show (RepeatCount { min, max: Just max }) | min == max = "{" <> show min <> "}"
                                            | otherwise = "{" <> show min <> ", " <> show max <> "}"

derive instance genericRepeatCount :: Generic RepeatCount

-- | Library Regex doesn't have Generic instance derived and so it can't be used directly
-- | in an ADT which itself derives Generic
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

instance eqGenRegex :: Eq GenRegex where
  eq = gEq

-- | A numeric bound (minimum or maximum) indicating if it's inclusive or exclusive
newtype Bound a = Bound { value :: a, inclusive :: Boolean }

instance showBound :: Show { value :: a, inclusive :: Boolean } => Show (Bound a) where
  show (Bound props) = show props

derive instance genericBound :: Generic a => Generic (Bound a)

-- | Pattern for property names. This is equivalent to standard String patterns
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

derive instance eqPropertyNamePattern :: Eq PropertyNamePattern

instance ordPropertyNamePattern :: Ord PropertyNamePattern where
  compare (LiteralName l1) (LiteralName l2) = compare l1 l2
  compare (LiteralName l1) _                = LT
  compare _                (LiteralName l2) = GT
  compare (WildcardName { pattern: Just (GenRegex r1) }) (WildcardName { pattern: Just (GenRegex r2) }) = compare (source r1) (source r2)
  compare (WildcardName { pattern: Just _ }) (WildcardName { pattern: Nothing }) = LT
  compare (WildcardName { pattern: Nothing }) (WildcardName { pattern: Just _ }) = GT
  compare _ _ = EQ

data ObjectRefinement = NamedRefinement String
                      | ObjectRefinement Pattern

instance showObjectRefinement :: Show ObjectRefinement where
  show (ObjectRefinement p) = show (Object p)
  show (NamedRefinement n) = show (NamedPattern n)

derive instance genericObjectRefinement :: Generic ObjectRefinement

derive instance eqObjectRefinement :: Eq ObjectRefinement

data Pattern = Empty
             | Any
             | Null
             | BooleanLiteral Boolean
             | BooleanDataType
             | StringLiteral String
             | StringDataType { minLength :: Maybe Int, maxLength :: Maybe Int, pattern :: Maybe GenRegex }
             | NumberLiteral Number
             | IntDataType { min :: Maybe (Bound Int), max :: Maybe (Bound Int), multipleOf :: Maybe Int }
             | NumberDataType { min :: Maybe (Bound Number), max :: Maybe (Bound Number), multipleOf :: Maybe Number }
             | Choice Pattern Pattern
             | Group Pattern Pattern
             | Repeat Pattern RepeatCount
             | ArrayPattern Pattern
             | Property { name :: PropertyNamePattern, value :: Pattern }
             | Object Pattern
             | RefinedObject (NonEmptyList ObjectRefinement) -- sorted with highest priority first and lowest last
             | NamedPattern String

type StringDtProps = { minLength :: Maybe Int, maxLength :: Maybe Int, pattern :: Maybe GenRegex }
emptyStringDtProps :: StringDtProps
emptyStringDtProps = { minLength: Nothing, maxLength: Nothing, pattern: Nothing }

type NumericDtProps a = { min :: Maybe (Bound a), max :: Maybe (Bound a), multipleOf :: Maybe a }
emptyNumericDtProps :: forall a. NumericDtProps a
emptyNumericDtProps = { min: Nothing, max: Nothing, multipleOf: Nothing }

showNumericDtProps :: forall a. Show a => NumericDtProps a -> String
showNumericDtProps { min, max, multipleOf } = showProps [
  boundProp "min"        min,
  boundProp "max"        max,
  prop      "multipleOf" multipleOf]

group :: Pattern -> Pattern -> Pattern
group Empty         p2    = p2
group p1            Empty = p1
group (Group g1 g2) p2    = group g1 (group g2 p2) -- make group structure similar to List
group p1 p2               = Group p1 p2

instance showPattern :: Show Pattern where
  show Empty = "Empty"
  show Any   = "Any"
  show Null  = "null"
  show (BooleanLiteral b)  = show b
  show BooleanDataType     = "Boolean"
  show (StringLiteral str) = show str
  show (NumberLiteral num) = maybe (show num) show $ Int.fromNumber num
  show (IntDataType ps)    = "Int" <> showNumericDtProps ps
  show (NumberDataType ps) = "Number" <> showNumericDtProps ps

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

  show (ArrayPattern g@(Group _ _)) = "[" <> commaSeparatedGroup g <> "]"
  show (ArrayPattern p) = "[" <> show p <> "]"

  show g@(Group _ _) = "(" <> commaSeparatedGroup g <> ")"

  show (Repeat p count) = showRepeated p <> show count
    where
      showRepeated :: Pattern -> String
      showRepeated (Property _) = "(" <> show p <> ")"
      showRepeated (Choice _ _) = "(" <> show p <> ")"
      showRepeated _            = show p

  show (Property { name, value }) = show name <> ": " <> show value
  show (Object Empty)             = "{}"
  show (Object g@(Group _ _))     = "{\n  " <> indent (intercalate ",\n" (show <$> (flattenGroup g))) <> "\n}"
  show (Object p)                 = "{\n  " <> indent (show p) <> "\n}"
  show (RefinedObject rs)         = intercalate " with " (show <$> List.reverse (NonEmptyList.toList rs))
  show (NamedPattern name)        = "$" <> name

indent :: String -> String
indent = Str.replaceAll (Str.Pattern "\n") (Str.Replacement "\n  ")

commaSeparatedGroup :: Pattern -> String
commaSeparatedGroup g = intercalate ", " (show <$> (flattenGroup g))

showProps :: Array (Maybe { name :: String, value :: String }) -> String
showProps xs =
  case printedProps of
    [] -> ""
    ps -> "(" <> intercalate ", " ps <> ")"
  where
    printedProps = (\p -> p.name <> " = " <> p.value) <$> catMaybes xs

prop :: forall a. Show a => String -> Maybe a -> Maybe { name :: String, value :: String }
prop n optV = { name: n, value: _ } <<< show <$> optV

boundProp :: forall a. Show a => String -> Maybe (Bound a) -> Maybe { name :: String, value :: String }
boundProp baseName optBound = do
    bound <- optBound
    pure $ case bound of
      Bound { value, inclusive } -> { name: propName inclusive, value: show value }
  where
    propName inclusive = baseName <> if inclusive then "" else "Exclusive"

foldGroup :: forall o. (o -> Pattern -> o) -> o -> Pattern -> o
foldGroup f accu = case _ of
  Group p1 p2 -> foldGroup f (foldGroup f accu p1) p2
  other -> f accu other

filterGroup :: (Pattern -> Boolean) -> Pattern -> Pattern
filterGroup f = foldGroup (\acc p -> if f p then group acc p else acc) Empty

flattenGroup :: Pattern -> CatList Pattern
flattenGroup = foldGroup CatList.snoc CatList.empty

foldChoice :: forall o. (o -> Pattern -> o) -> o -> Pattern -> o
foldChoice f accu = case _ of
  Choice p1 p2 -> foldChoice f (foldChoice f accu p1) p2
  other -> f accu other

flattenChoice :: Pattern -> CatList Pattern
flattenChoice = foldChoice CatList.snoc CatList.empty

walk :: forall s. Semigroup s => (Pattern -> Boolean) -> (Pattern -> s) -> Pattern -> s
walk shouldOpen f pattern = case pattern of
    g@Group g1 g2 ->
      if shouldOpen g then (recur g1) <> (recur g2)
      else f g

    c@Choice c1 c2 ->
      if shouldOpen c then (recur c1) <> (recur c2)
      else f c

    r@Repeat p _ ->
      if shouldOpen r then recur p
      else f r

    a@ArrayPattern p ->
      if shouldOpen a then recur p
      else f a

    o@Object p ->
      if shouldOpen o then recur p
      else f o

    p@Property { value } ->
      if shouldOpen p then recur value
      else f p

    other -> f other
  where
    recur :: Pattern -> s
    recur = walk shouldOpen f

propertyNames :: Pattern -> Array String
propertyNames pattern = Array.sort $ show <$> (Array.fromFoldable $ propertyNamePatterns pattern)

propertyNamePatterns :: Pattern -> Set PropertyNamePattern
propertyNamePatterns = walk shouldOpen transform
  where
    shouldOpen (Property _) = false
    shouldOpen _            = true

    transform (Property { name }) = Set.singleton name
    transform _                   = mempty

derive instance genericPattern :: Generic Pattern

instance eqPattern :: Eq Pattern where
  eq = gEq
