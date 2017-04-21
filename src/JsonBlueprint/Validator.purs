module JsonBlueprint.Validator (
  ValidationError(..),
  ValidationErrors,
  validate
) where

import Prelude
import Data.Int as Int
import Data.Sequence as Seq
import Data.String as Str
import Data.String.Regex as Regex
import Data.Argonaut.Core (Json, foldJson, foldJsonArray, foldJsonBoolean, foldJsonNull, foldJsonNumber, foldJsonString)
import Data.Array (mapWithIndex)
import Data.Either (Either(..), fromRight)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Sequence (Seq, empty)
import Data.Tuple (Tuple(..))
import JsonBlueprint.JsonPath (JsonPath, JsonPathNode(..), (\))
import JsonBlueprint.Pattern (Bound(..), GenRegex(..), Pattern(..), RepeatCount(..), group)
import Math (remainder)
import Partial.Unsafe (unsafePartial)

-- | type of validation errors; it's a newtype as type synonyms can't be recursive
newtype ValidationError = ValidationError { path :: JsonPath, pattern :: Pattern, message :: String, children :: Seq ValidationError }

instance showValidationError :: Show ValidationError where
  show (ValidationError { path, pattern, message, children }) =
    "{message: " <> show message <>
   ", path: " <> show path <>
   ", pattern: " <> show pattern <>
   ", children: " <> show children <> "}"

type ValidationErrors = Seq ValidationError

validate :: Json -> Pattern -> Either ValidationErrors Unit
validate = validateValue mempty

-- | validates given JSON value (null / Boolean / String / Number / Array / Object)
-- | against the given pattern; this does not produce a derivative each of these
-- | values have to fully match given pattern (unlike validation of array / object
-- | content, where the pattern usually matches each item / property only partially)
validateValue :: JsonPath -> Json -> Pattern -> Either ValidationErrors Unit
validateValue path json pattern = case pattern of
    Any -> pure unit

    Null -> const unit <$> expectX "null" foldJsonNull json

    BooleanDataType -> const unit <$> expectBoolean json

    BooleanLiteral bool -> validateLiteral bool (expectBoolean json)

    StringLiteral str -> validateLiteral str (expectString json)

    NumberLiteral num -> validateLiteral num (expectNumber json)

    StringDataType { minLength, maxLength, pattern: regex } -> do
      actual <- expectString json
      checkAll [check (\min -> { ok: Str.length actual >= min, errMsg: "String is too short. Expected at least " <> show min <> " characters."}) minLength,
                check (\max -> { ok: Str.length actual <= max, errMsg: "String is too long. Epected at most " <> show max <> " characters." }) maxLength,
                check (\re -> { ok: matches re actual, errMsg: "String doesn't match regular expression: " <> show re }) regex]

    NumberDataType props -> do
      actual <- expectNumber json
      validateNumeric props actual remainder

    IntDataType props -> do
      num <- expectNumber json
      case Int.fromNumber num of
        Just int -> validateNumeric props int mod
        Nothing -> fail "Expected Int but found decimal Number"

    Choice _ _ ->
      let
        result = choiceDeriv aggError (validateValueDeriv path json) pattern
      in
        if isValid result then pure unit
        else Left result.errors

    ArrayPattern contentP -> do
      arr <- expectX "Array" foldJsonArray json
      validateArray path arr contentP

    _ -> fail $ "Unexpected pattern type found in JSON value position (this is most likely a bug in the validator): " <> show pattern
  where
    fail :: forall a. String -> Either ValidationErrors a
    fail msg = Left $ pure $ error msg

    error :: String -> ValidationError
    error message = ValidationError { path, pattern, message, children: Seq.empty }

    aggError :: String -> ValidationErrors -> ValidationError
    aggError message children = ValidationError { path, pattern, message, children }

    checkAll :: Array (Either ValidationErrors Unit) -> Either ValidationErrors Unit
    checkAll = foldl combineErrors (pure unit) where
      combineErrors :: Either ValidationErrors Unit -> Either ValidationErrors Unit -> Either ValidationErrors Unit
      combineErrors (Left es1) (Left es2) = Left $ es1 <> es2
      combineErrors l@(Left _) _          = l
      combineErrors _          l@(Left _) = l
      combineErrors a          _          = a

    check :: forall a. (a -> { ok :: Boolean, errMsg :: String }) -> Maybe a -> Either ValidationErrors Unit
    check _ Nothing = pure unit
    check f (Just val) =
        if result.ok then pure unit
        else fail result.errMsg
      where
        result = f (val)

    expectX :: forall a. String -> (Either ValidationErrors a -> (a -> Either ValidationErrors a) -> Json -> Either ValidationErrors a) -> Json -> Either ValidationErrors a
    expectX targetTypeName foldFn js = foldFn (fail $ "Invalid type. Expected " <> targetTypeName <> " but found " <> describeType js) pure js

    expectBoolean :: Json -> Either ValidationErrors Boolean
    expectBoolean = expectX "Boolean" foldJsonBoolean

    expectString :: Json -> Either ValidationErrors String
    expectString = expectX "String" foldJsonString

    expectNumber :: Json -> Either ValidationErrors Number
    expectNumber = expectX "Number" foldJsonNumber

    validateLiteral :: forall a. Eq a => Show a => a -> Either ValidationErrors a -> Either ValidationErrors Unit
    validateLiteral expected actual = do
      act <- actual
      (if act == expected then pure unit
       else fail $ "Invalid value. Expected " <> show expected)

    checkBound :: forall n. n -> (n -> n -> Boolean) -> (n -> n -> Boolean) -> Maybe (Bound n) -> (n -> Boolean -> String) -> Either ValidationErrors Unit
    checkBound _ _ _ Nothing _ = pure unit
    checkBound actual exclOp inclOp (Just (Bound { value, inclusive })) createErrMsg =
      let
        effectiveOp = if inclusive then inclOp else exclOp
      in
        if actual `effectiveOp` value then pure unit
        else fail $ createErrMsg value inclusive

    validateNumeric :: forall n. Ord n => EuclideanRing n => Show n =>
                       { min :: Maybe (Bound n), max :: Maybe (Bound n), multipleOf :: Maybe n } -> n -> (n -> n -> n) -> Either ValidationErrors Unit
    validateNumeric { min, max, multipleOf } actual rem =
        checkAll [checkBound actual (>) (>=) min numberTooLowErr,
                  checkBound actual (<) (<=) max numberTooHighErr,
                  check (\factor -> { ok: actual `rem` factor == zero, errMsg: "Number is not multiple of " <> show factor }) multipleOf]
      where
        numberTooLowErr :: n -> Boolean -> String
        numberTooLowErr m inclusive = "Number is lower than minimal allowed value of " <> show m <> if inclusive then "" else " (exclusive)"

        numberTooHighErr :: n -> Boolean -> String
        numberTooHighErr m inclusive = "Number exceeds maximal allowed value of " <> show m <> if inclusive then "" else " (exclusive)"

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

validateValueDeriv :: JsonPath -> Json -> Pattern -> Derivative
validateValueDeriv path json = result2Deriv <<< (validateValue path json) where
  result2Deriv :: Either ValidationErrors Unit -> Derivative
  result2Deriv (Right _) = { deriv: Empty, errors: empty }
  result2Deriv (Left es) = { deriv: Empty, errors: es }

validateArray :: JsonPath -> Array Json -> Pattern -> Either ValidationErrors Unit
validateArray basePath is pattern =
    let
      zippedWithPath = mapWithIndex (\idx item -> { json: item, path: basePath \ IdxNode idx }) is
      result = foldl validateArrayItem { deriv: pattern, errors: empty } zippedWithPath
    in
      if nullable result.deriv then deriv2Either result
      else
        let
          message = "Unexpected end of array. Pattern for remaining items: " <> show result.deriv
          err = ValidationError { path: basePath, pattern: result.deriv, message, children: Seq.empty }
        in Left $ Seq.snoc result.errors err
  where
    deriv2Either :: Derivative -> Either ValidationErrors Unit
    deriv2Either der =
      if isValid der then pure unit
      else Left der.errors

validateArrayItem :: Derivative -> { json :: Json, path :: JsonPath } -> Derivative
validateArrayItem { deriv, errors } { json, path } =
  let result = validateArrayItem' path json deriv
  in { deriv: result.deriv, errors: errors <> result.errors }

validateArrayItem' :: JsonPath -> Json -> Pattern -> Derivative
validateArrayItem' path json pattern = case pattern of
    Group gHead gTail ->
      let headRes = recur gHead in
        if nullable gHead then
          let tailRes = recur gTail
          in
            case Tuple (isValid headRes) (isValid tailRes) of
              Tuple true  true  -> { deriv: Choice (group headRes.deriv gTail) tailRes.deriv, errors: empty }
              Tuple true  false -> { deriv: group headRes.deriv gTail, errors: empty }
              Tuple false true  -> { deriv: tailRes.deriv, errors: empty }
              Tuple false false -> { deriv: Empty, errors: Seq.singleton $ aggError "Invalid array item (two alternatives tried)" (headRes.errors <> tailRes.errors) }
        else
          headRes { deriv = group headRes.deriv gTail }

    ch@(Choice _ _) -> choiceDeriv aggError recur pattern

    Repeat p count -> repeatDeriv error recur p count

    Empty -> fail "Expected end of array"

    other -> validateValueDeriv path json other
  where
    recur :: Pattern -> Derivative
    recur = validateArrayItem' path json

    error :: String -> ValidationError
    error message = ValidationError { path, pattern, message, children: Seq.empty }

    fail :: String -> Derivative
    fail message = { deriv: Empty, errors: Seq.singleton $ error message }

    aggError :: String -> ValidationErrors -> ValidationError
    aggError message children = ValidationError { path, pattern, message, children }

-- | when matching Pattern against part of complex JSON value (an array item or
-- | a property of JSON object), it doesn't have to match fully - we can be left
-- | with some remainder of the pattern which for subsequent items / properties -
-- | this is called a "derivative" of the Pattern with respect to the given
-- | JSON node
type Derivative = { deriv :: Pattern, errors :: ValidationErrors }

isValid :: Derivative -> Boolean
isValid { errors } = Seq.null errors

choiceDeriv :: (String -> ValidationErrors -> ValidationError) -> (Pattern -> Derivative) -> Pattern -> Derivative
choiceDeriv createError doValidate pattern =
  let result = choiceDeriv' doValidate pattern
  in
    if isValid result then result
    else { deriv: Empty, errors: Seq.singleton $ createError "None of allowed patterns matches JSON value" result.errors }

choiceDeriv' :: (Pattern -> Derivative) -> Pattern -> Derivative
choiceDeriv' doValidate = case _ of
    Choice p1 p2 -> appendDeriv (choiceDeriv' doValidate p1) (choiceDeriv' doValidate p2)
    other -> doValidate other
  where
    appendDeriv :: Derivative -> Derivative -> Derivative
    appendDeriv d1 d2 = case Tuple (isValid d1) (isValid d2) of
      Tuple true  true  -> { deriv: Choice d1.deriv d2.deriv, errors: empty }
      Tuple true  false -> d1
      Tuple false true  -> d2
      Tuple false false -> { deriv: Empty, errors: d1.errors <> d2.errors }

repeatDeriv :: (String -> ValidationError) -> (Pattern -> Derivative) -> Pattern -> RepeatCount -> Derivative
repeatDeriv createError doValidate pattern (RepeatCount { min, max }) =
    if maybe false (\m -> m <= 0) max then
      { deriv: Empty, errors: Seq.singleton $ createError "Schema doesn't allow another repetition of this item or property." }
    else
      let
        result = doValidate pattern
        nextMax = countDown <$> max
        nextCount = RepeatCount { min: countDown min, max: nextMax }
        repeatTail = if maybe true (\m -> m > 0) nextMax then Repeat pattern nextCount else Empty
        deriv = group result.deriv repeatTail
      in
        result { deriv = deriv }
  where
    countDown :: Int -> Int
    countDown i = if i > 0 then i - 1 else 0

-- | nullable Pattern matches empty input (f.ex. in array content validation,
-- | the array content is valid if derivative of last array element is nullable)
nullable :: Pattern -> Boolean
nullable Empty = true
nullable (Group p1 p2) = nullable p1 && nullable p2
nullable (Choice p1 p2) = nullable p1 || nullable p2
nullable (Repeat p (RepeatCount { min })) = nullable p || min <= 0
nullable _ = false

describeType :: Json -> String
describeType = foldJson
  (const "null")
  (const "Boolean")
  (const "Number")
  (const "String")
  (const "Array")
  (const "Object")
