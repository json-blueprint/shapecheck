module JsonBlueprint.Validator where

import Prelude
import Data.String as Str
import Data.String.Regex as Regex
import Data.Argonaut.Core (Json, foldJson, foldJsonBoolean, foldJsonNumber, foldJsonString)
import Data.Either (Either(..), fromRight)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Sequence (Seq, snoc)
import Data.Validation.Semigroup (invalid, V, unV)
import JsonBlueprint.Pattern (GenRegex(..), Pattern(..), propNameRequiresQuoting, RepeatCount(..))
import Partial.Unsafe (unsafePartial)

-- | simple jq-like path pointing to a locaiton in a JSON document
newtype JsonPath = JsonPath (Seq JsonPathNode)

instance semigroupJsonPath :: Semigroup JsonPath where
  append (JsonPath ns) (JsonPath ms) = JsonPath $ ns <> ms

instance monoidJsonPath :: Monoid JsonPath where
  mempty = JsonPath (mempty :: Seq JsonPathNode)

instance showJsonPath :: Show JsonPath where
  show (JsonPath ns) = "." <> intercalate "." (show <$> ns)

instance eqJsonPath :: Eq JsonPath where
  eq (JsonPath n1s) (JsonPath n2s) = n1s == n2s

appendNode :: JsonPath -> JsonPathNode -> JsonPath
appendNode (JsonPath ns) n = JsonPath $ snoc ns n

infixr 5 appendNode as \

data JsonPathNode = KeyNode String
                  | IdxNode Int

instance eqJsonPathNode :: Eq JsonPathNode where
  eq (KeyNode k1) (KeyNode k2) = k1 == k2
  eq (IdxNode i1) (IdxNode i2) = i2 == i2
  eq _ _ = false

instance showJsonPathNode :: Show JsonPathNode where
  show (KeyNode key) | propNameRequiresQuoting key = "[" <> show key <> "]"
                     | otherwise = key
  show (IdxNode idx) = "[" <> show idx <> "]"

type ValidationResult = V Errors Unit

-- | type of validation errors
type Error = { path :: JsonPath, pattern :: Pattern, message :: String }

type Errors = Seq Error

-- | nullable Pattern matches empty input (f.ex. in array content validation,
-- | the array content is valid if derivative of last array element is nullable)
nullable :: Pattern -> Boolean
nullable Empty = true
nullable (Group p1 p2) = nullable p1 && nullable p2
nullable (Choice p1 p2) = nullable p1 || nullable p2
nullable (Repeat p (RepeatCount { min })) = nullable p || min <= 0
nullable _ = false

validate :: Json -> Pattern -> ValidationResult
validate = validateValue mempty

validateValue :: JsonPath -> Json -> Pattern -> ValidationResult
validateValue path json pattern = case pattern of
    BooleanDataType -> fromEither $ expectBoolean json

    (BooleanLiteral bool) -> validateLiteral bool (expectBoolean json)

    (StringLiteral str) -> validateLiteral str (expectString json)

    (NumberLiteral num) -> validateLiteral num (expectNumber json)

    (StringDataType { minLength, maxLength, pattern: regex }) -> fromEither do
      actual <- expectString json
      toEither $ check (\min -> { ok: Str.length actual >= min, errMsg: "String is too short. Expected at least " <> show min <> " characters."}) minLength *>
                 check (\max -> { ok: Str.length actual <= max, errMsg: "String is too long. Epected at most " <> show max <> " characters." }) maxLength *>
                 check (\re -> { ok: matches re actual, errMsg: "String doesn't match regular expression: " <> show re }) regex
    _ -> pure unit
  where
    fromEither :: forall a. Either Errors a -> ValidationResult
    fromEither (Right _) = pure unit
    fromEither (Left err) = invalid err

    toEither :: ValidationResult -> Either Errors Unit
    toEither res = unV Left Right res

    fail :: forall a. String -> Either Errors a
    fail msg = Left $ pure $ error msg

    error :: String -> Error
    error = { path, pattern, message: _ }

    check :: forall a. (a -> { ok :: Boolean, errMsg :: String }) -> Maybe a -> ValidationResult
    check _ Nothing = pure unit
    check f (Just val) =
        if result.ok then pure unit
        else invalid $ pure $ error result.errMsg
      where
        result = f (val)

    expectX :: forall a. String -> (Either Errors a -> (a -> Either Errors a) -> Json -> Either Errors a) -> Json -> Either Errors a
    expectX targetTypeName foldFn js = foldFn (fail $ "Invalid type. Expected " <> targetTypeName <> " but found " <> describeType js) pure js

    expectBoolean :: Json -> Either Errors Boolean
    expectBoolean = expectX "Boolean" foldJsonBoolean

    expectString :: Json -> Either Errors String
    expectString = expectX "String" foldJsonString

    expectNumber :: Json -> Either Errors Number
    expectNumber = expectX "Number" foldJsonNumber

    validateLiteral :: forall a. Eq a => Show a => a -> Either Errors a -> ValidationResult
    validateLiteral expected actual = fromEither do
      act <- actual
      (if act == expected then pure unit
       else fail $ "Invalid value. Expected " <> show expected)

    matches :: GenRegex -> String -> Boolean
    matches (GenRegex regex) = Regex.test effRegex where
      charAtEq :: Int -> Char -> String -> Boolean
      charAtEq idx char str = case Str.charAt idx str of
        Just c -> c == char
        Nothing -> false

      regexStr = Regex.source regex

      effRegexStr = if charAtEq 0 '^' regexStr then regexStr else "^" <> regexStr
      effRegexStr' = if charAtEq ((Str.length effRegexStr) - 1) '$' effRegexStr then effRegexStr else effRegexStr <> "$"
      effRegex = unsafePartial $ fromRight $ Regex.regex effRegexStr' (Regex.flags regex)

describeType :: Json -> String
describeType = foldJson
  (const "null")
  (const "Boolean")
  (const "Number")
  (const "String")
  (const "Array")
  (const "Object")
