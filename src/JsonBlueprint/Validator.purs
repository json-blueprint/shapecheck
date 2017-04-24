module JsonBlueprint.Validator (
  ValidationError(..),
  ValidationErrors,
  validate
) where

import Prelude
import Data.Argonaut.Core as Json
import Data.Int as Int
import Data.Sequence as Seq
import Data.Set as Set
import Data.StrMap as StrMap
import Data.String as Str
import Data.String.Regex as Regex
import Data.Argonaut.Core (Json, foldJson, foldJsonArray, foldJsonBoolean, foldJsonNull, foldJsonNumber, foldJsonObject, foldJsonString)
import Data.Array (mapWithIndex)
import Data.Either (Either(..), fromRight, isRight)
import Data.Foldable (foldl, intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Sequence (Seq, empty)
import Data.Set (Set)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import JsonBlueprint.JsonPath (JsonPath, JsonPathNode(..), (\))
import JsonBlueprint.Pattern (Bound(..), GenRegex(..), Pattern(..), PropertyNamePattern(..), RepeatCount(..), group, walk)
import JsonBlueprint.Schema (Schema, lookupPattern)
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

-- | when matching Pattern against part of complex JSON value (an array item or
-- | a property of JSON object), it doesn't have to match fully - we can be left
-- | with some remainder of the pattern which for subsequent items / properties -
-- | this is called a "derivative" of the Pattern with respect to the given
-- | JSON node
type Derivative = { deriv :: Pattern, errors :: ValidationErrors }

isValid :: Derivative -> Boolean
isValid { errors } = Seq.null errors

deriv2Either :: Derivative -> Either ValidationErrors Unit
deriv2Either der =
  if isValid der then pure unit
  else Left der.errors

validate :: Schema -> Json -> Pattern -> Either ValidationErrors Unit
validate schema = validateValue schema mempty

-- | validates given JSON value (null / Boolean / String / Number / Array / Object)
-- | against the given pattern; this does not produce a derivative each of these
-- | values have to fully match given pattern (unlike validation of array / object
-- | content, where the pattern usually matches each item / property only partially)
validateValue :: Schema -> JsonPath -> Json -> Pattern -> Either ValidationErrors Unit
validateValue schema path json pattern = case pattern of
    Any -> pure unit

    Null -> const unit <$> expectX "null" foldJsonNull json

    BooleanDataType -> const unit <$> expectBoolean json

    BooleanLiteral bool -> validateLiteral bool (expectBoolean json)

    StringLiteral str -> validateLiteral str (expectString json)

    -- show for Number prints even integers as decimals, so we keep it wrapped in NumberLiteral which has better show
    num@NumberLiteral _ -> validateLiteral num (NumberLiteral <$> expectNumber json)

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

    ArrayPattern contentP -> do
      arr <- expectX "Array" foldJsonArray json
      validateArray schema path arr contentP

    Object contentP -> do
      obj <- expectX "Object" foldJsonObject json
      validateObject schema path obj contentP

    Choice _ _ ->
      let
        result = simpleChoiceDeriv aggError (validateValueDeriv schema path json) pattern
      in
        if isValid result then pure unit
        else Left result.errors

    NamedPattern name -> case lookupPattern name schema of
      Just p -> validateValue schema path json p
      Nothing -> fail $ "Pattern with name `" <> name <> "` not found in schema."

    _ -> fail $ "Unexpected pattern type found in JSON value position (this is most likely a bug in the validator): " <> show pattern
  where
    fail :: forall a. String -> Either ValidationErrors a
    fail msg = Left $ pure $ error msg

    error :: String -> ValidationError
    error = (flip aggError) Seq.empty

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

validateValueDeriv :: Schema -> JsonPath -> Json -> Pattern -> Derivative
validateValueDeriv schema path json = result2Deriv <<< (validateValue schema path json) where
  result2Deriv :: Either ValidationErrors Unit -> Derivative
  result2Deriv (Right _) = { deriv: Empty, errors: empty }
  result2Deriv (Left es) = { deriv: Empty, errors: es }

validateArray :: Schema -> JsonPath -> Array Json -> Pattern -> Either ValidationErrors Unit
validateArray schema basePath is pattern =
    let
      zippedWithPath = mapWithIndex (\idx item -> { json: item, path: basePath \ IdxNode idx }) is
      result = foldl validateItem { deriv: pattern, errors: empty } zippedWithPath
    in
      if nullable result.deriv then deriv2Either result
      else
        let
          message = "Unexpected end of array. Pattern for remaining items: " <> show result.deriv
          err = ValidationError { path: basePath, pattern: result.deriv, message, children: Seq.empty }
        in Left $ Seq.snoc result.errors err
  where
    validateItem :: Derivative -> { json :: Json, path :: JsonPath } -> Derivative
    validateItem { deriv, errors } { json, path } =
      let result = validateArrayItem schema path json deriv
      in { deriv: result.deriv, errors: errors <> result.errors }

validateArrayItem :: Schema -> JsonPath -> Json -> Pattern -> Derivative
validateArrayItem schema path json pattern = case pattern of
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

    ch@(Choice _ _) -> simpleChoiceDeriv aggError recur ch

    Repeat p count -> simpleRepeatDeriv error recur p count

    Empty -> fail "Expected end of array"

    other -> validateValueDeriv schema path json other
  where
    recur :: Pattern -> Derivative
    recur = validateArrayItem schema path json

    error :: String -> ValidationError
    error = (flip aggError) Seq.empty

    aggError :: String -> ValidationErrors -> ValidationError
    aggError message children = ValidationError { path, pattern, message, children }

    fail :: String -> Derivative
    fail message = { deriv: Empty, errors: Seq.singleton $ error message }

validateObject :: Schema -> JsonPath -> StrMap Json -> Pattern -> Either ValidationErrors Unit
validateObject schema basePath obj pattern =
  let
    props :: Array { name :: String, value :: Json, path :: JsonPath }
    props  = (\(Tuple name value) -> { name, value, path: basePath \ KeyNode name }) <$> (StrMap.toUnfoldable obj)
    result = foldl (validateObjectProperty schema) { deriv: pattern, errors: Seq.empty } props
  in
    if nullable result.deriv then deriv2Either result
    else
      let message = "Object is missing required properties: " <> (intercalate ", " $ propertyNames result.deriv)
      in Left $ pure $ ValidationError { path: basePath, pattern: result.deriv, message, children: Seq.empty }

validateObjectProperty :: Schema -> Derivative -> { name :: String, value :: Json, path :: JsonPath } -> Derivative
validateObjectProperty schema { deriv, errors } prop =
    case doValidate matchLiteralProps of
      Just res -> { deriv: res.deriv, errors: errors <> res.errors }
      Nothing ->
        case doValidate matchWildcardProps of
          Just res2 -> { deriv: res2.deriv, errors: errors <> res2.errors }
          Nothing ->
            let
              validPropNames = propertyNames deriv
              err = error $ "Unexpected property " <> show prop.name <> ". Possible property names: " <> (intercalate ", " $ show <$> validPropNames)
            in
              { deriv, errors: Seq.snoc errors err }
  where
    doValidate :: (String -> PropertyNamePattern -> Boolean) -> Maybe Derivative
    doValidate propNameMatcher = validateObjectProperty' schema (propNameMatcher prop.name) prop deriv

    error :: String -> ValidationError
    error message = ValidationError { path: prop.path, pattern: deriv, message, children: Seq.empty }

    matchLiteralProps :: String -> PropertyNamePattern -> Boolean
    matchLiteralProps actualName = case _ of
      LiteralName expectedName -> actualName == expectedName
      _ -> false

    matchWildcardProps :: String -> PropertyNamePattern -> Boolean
    matchWildcardProps actualName = case _ of
      WildcardName props -> isRight $ validateValue schema mempty (Json.fromString actualName) (StringDataType props)
      _ -> false

validateObjectProperty' :: forall a. Schema -> (PropertyNamePattern -> Boolean) -> { value :: Json, path :: JsonPath | a } -> Pattern -> Maybe Derivative
validateObjectProperty' schema matchesPropName prop pattern = case pattern of
    Property { name, value } ->
      if matchesPropName name then pure $ validateValueDeriv schema prop.path prop.value value
      else Nothing

    Group p1 p2 ->
      case recur p1 of
        Just p1Res -> Just { deriv: group p1Res.deriv p2, errors: p1Res.errors }
        Nothing ->
          case recur p2 of
            Just p2Res -> Just { deriv: group p1 p2Res.deriv, errors: p2Res.errors }
            Nothing -> Nothing

    ch@Choice _ _ -> choiceDeriv aggError recur ch

    r@Repeat p c -> repeatDeriv error recur p c

    Empty -> fail $ "Unexpected property. Schema doesn't allow any additional properties."

    other -> fail $ "Unexpected pattern type found when validating object content (this is most likely a bug in the validator): " <> show pattern
  where
    error :: String -> ValidationError
    error = (flip aggError) Seq.empty

    aggError :: String -> ValidationErrors -> ValidationError
    aggError message children = ValidationError { path: prop.path, pattern, message, children }

    fail :: String -> Maybe Derivative
    fail message = pure { deriv: Empty, errors: Seq.singleton $ error message }

    recur :: Pattern -> Maybe Derivative
    recur = validateObjectProperty' schema matchesPropName prop

propertyNames :: Pattern -> Seq String
propertyNames pattern = Seq.sort $ show <$> (Seq.fromFoldable $ propertyNamePatterns pattern)
  where
    propertyNamePatterns :: Pattern -> Set PropertyNamePattern
    propertyNamePatterns = walk shouldOpen transform
      where
        shouldOpen (Property _) = false
        shouldOpen _            = true

        transform (Property { name }) = Set.singleton name
        transform _                   = mempty

simpleChoiceDeriv :: (String -> ValidationErrors -> ValidationError) -> (Pattern -> Derivative) -> Pattern -> Derivative
simpleChoiceDeriv createError doValidate pattern =
  case choiceDeriv createError (Just <<< doValidate) pattern of
    Just deriv -> deriv
    Nothing -> { deriv: Empty, errors: empty }

choiceDeriv :: (String -> ValidationErrors -> ValidationError) -> (Pattern -> Maybe Derivative) -> Pattern -> Maybe Derivative
choiceDeriv createError doValidate pattern =
    let result = choiceDeriv' doValidate pattern
    in wrapErrors <$> result
  where
    isLiteral :: Pattern -> Boolean
    isLiteral (BooleanLiteral _) = true
    isLiteral (StringLiteral _)  = true
    isLiteral (NumberLiteral _)  = true
    isLiteral Null               = true
    isLiteral _                  = false

    wrapErrors :: Derivative -> Derivative
    wrapErrors res =
      if isValid res || Seq.length res.errors <= 1 then res
      else
        let
          errPs = (\(ValidationError props) -> props.pattern) <$> res.errors
          err = if foldl (&&) true (isLiteral <$> errPs) then
                  createError ("Expected one of: " <> intercalate ", " (show <$> errPs)) empty
                else
                  createError "None of allowed patterns matches JSON value" res.errors
        in
          { deriv: Empty, errors: pure err }

choiceDeriv' :: (Pattern -> Maybe Derivative) -> Pattern -> Maybe Derivative
choiceDeriv' doValidate = case _ of
    Choice p1 p2 -> appendDeriv (choiceDeriv' doValidate p1) (choiceDeriv' doValidate p2)
    other -> doValidate other
  where
    appendDeriv :: Maybe Derivative -> Maybe Derivative -> Maybe Derivative
    appendDeriv Nothing    Nothing    = Nothing
    appendDeriv j@(Just _) Nothing    = j
    appendDeriv Nothing    j@(Just _) = j
    appendDeriv (Just d1)  (Just d2)  = Just
      case Tuple (isValid d1) (isValid d2) of
        Tuple true  true  -> { deriv: Choice d1.deriv d2.deriv, errors: (empty :: ValidationErrors) }
        Tuple true  false -> d1
        Tuple false true  -> d2
        Tuple false false -> { deriv: Empty, errors: d1.errors <> d2.errors }

simpleRepeatDeriv :: (String -> ValidationError) -> (Pattern -> Derivative) -> Pattern -> RepeatCount -> Derivative
simpleRepeatDeriv createError doValidate pattern repeatCount =
  case repeatDeriv createError (Just <<< doValidate) pattern repeatCount of
    Just d -> d
    Nothing -> { deriv: Empty, errors: empty }

repeatDeriv :: (String -> ValidationError) -> (Pattern -> Maybe Derivative) -> Pattern -> RepeatCount -> Maybe Derivative
repeatDeriv createError doValidate pattern (RepeatCount { min, max }) =
  (doValidate pattern) <#> \result ->
    if maybe false (\m -> m <= 0) max then
      { deriv: Empty, errors: Seq.singleton $ createError "Schema doesn't allow another repetition of this item or property." }
    else
      let
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
