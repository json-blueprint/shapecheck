module JsonBlueprint.Parser (
  schemaParser,
  valuePatternParser,
  jsonPathParser
) where

import Prelude
import Data.Argonaut.Core as Json
import Data.Eulalie.Char as C
import Data.Eulalie.String as S
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Sequence as Seq
import JsonBlueprint.Schema as Schema
import Control.Alt ((<|>))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (fromFoldable)
import Data.Array.Partial (head, tail)
import Data.Char (fromCharCode, toLower)
import Data.Char.Unicode (isHexDigit)
import Data.Either (Either(..))
import Data.Eulalie.Parser (Parser, cut, either, expected, fail, many, maybe, sat, sepBy, sepBy1)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Int (fromNumber, fromString, fromStringAs, hexadecimal)
import Data.Lazy (Lazy, defer, force)
import Data.List (List, (:))
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.String (fromCharArray, singleton)
import Data.String.Regex (regex)
import Data.String.Regex.Flags (RegexFlags, ignoreCase, noFlags)
import JsonBlueprint.JsonPath (JsonPath(..), JsonPathNode(..))
import JsonBlueprint.Pattern (Bound(..), GenRegex(..), NumericDtProps, ObjectRefinement(..), Pattern(..), PropertyNamePattern(..), RepeatCount(..), StringDtProps, emptyNumericDtProps, emptyStringDtProps, group)
import JsonBlueprint.Schema (Schema)
import Partial.Unsafe (unsafePartial)

lazyParser :: forall a. Lazy (Parser Char a) -> Parser Char a
lazyParser lp = do
  pure unit
  force lp

stringChar :: Parser Char Char
stringChar =  sat \c -> c /= '\\' && c /= '"'

standardEscape :: Parser Char Char
standardEscape = expected standardEscape' "standard escape sequence" where
  standardEscape' = do
    C.char '\\'
    cut do
      c <- C.oneOf "\\/\"bfnrt"
      pure $ case c of
        'b' -> '\b'
        'f' -> '\f'
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        other -> other

hexDigit :: Parser Char Char
hexDigit = expected (sat isHexDigit) "hex digit"

unicodeEscape :: Parser Char Char
unicodeEscape = do
    expected (S.string "\\u") "unicode escape sequence"
    ds <- cut $ expected (replicateM 4 hexDigit) "invalid unicode escape sequence"
    decodeChar ds
  where
    decodeChar :: forall f. (Foldable f) => f Char -> Parser Char Char
    decodeChar cs =
      let optCharCode = (fromFoldable >>> fromCharArray >>> (fromStringAs hexadecimal)) cs in
        case optCharCode of
         Just charCode -> pure $ fromCharCode charCode
         Nothing -> fail

zeroP :: Parser Char Int
zeroP = const 0 <$> C.char '0'

nonZeroDigit :: Parser Char Char
nonZeroDigit = sat $ \c -> c >= '1' && c <= '9'

nonNegativeInt :: Parser Char Int
nonNegativeInt = expected nonNegativeInt' "non-negative integer" where
  nonNegativeInt' = zeroP <|> do
    first <- nonZeroDigit
    rest <- C.many C.digit
    case fromString (singleton first <> rest) of
      Just n -> pure n
      Nothing -> fail

number :: Parser Char Number
number = do
    sign <- maybe $ S.string "-"
    intPart <- show <$> nonNegativeInt
    cut do
      theRest <- maybe (exp <|> fraction)
      case jsonParser (sign <> intPart <> theRest) of
        Left err -> expected fail err
        Right json -> pure $ unsafePartial $ fromJust $ Json.toNumber json
  where
    exp :: Parser Char String
    exp = do
      e <- S.oneOf ["e", "E"]
      cut do
        sign <- maybe $ S.string "-"
        ds <- C.many1 C.digit
        pure $ e <> sign <> ds

    fraction ::  Parser Char String
    fraction = do
      C.char '.'
      cut do
        ds <- C.many1 C.digit
        e <- maybe exp
        pure $ "." <> ds <> e

integer :: Parser Char Int
integer = do
  num <- number
  case fromNumber num of
    Just i -> pure i
    Nothing -> expected fail (show num <> " is not an integer")

dtProp :: forall v d. String -> Parser Char v -> (v -> d -> d) -> Parser Char (d -> d)
dtProp name valP f = do
  S.spaces
  S.string name
  S.spaces
  C.char '='
  cut do
    S.spaces
    val <- valP
    S.spaces
    pure $ f val

commaSeparator :: Parser Char Unit
commaSeparator = do
  S.spaces
  C.char ','
  S.spaces
  pure unit

commaSeparated :: forall i o. Char -> Parser Char i -> Char -> (List i -> o) -> Parser Char o
commaSeparated open itemParser close transform = do
  C.char open
  cut do
    S.spaces
    is <- sepBy commaSeparator itemParser
    S.spaces
    C.char close
    pure $ transform is

-- TODO: improve error reporting for misspelled property names
dtProps :: forall d. d -> Array (Parser Char (d -> d)) -> Parser Char d
dtProps a b = (propList a b) <|> pure a
  where
    propList :: d -> Array (Parser Char (d -> d)) -> Parser Char d
    propList dt propParsers = do
      S.spaces
      commaSeparated '(' (reduce propParsers) ')' (foldl (#) dt)

    reduce :: Array (Parser Char (d -> d)) -> Parser Char (d -> d)
    reduce [] = pure id
    reduce ps = unsafePartial $ foldl either (head ps) (tail ps)

booleanDataType :: Parser Char Pattern
booleanDataType = S.string "Boolean" <#> (\_ -> BooleanDataType)

booleanLiteral :: Parser Char Pattern
booleanLiteral = (const (BooleanLiteral true)  <$> S.string "true")
             <|> (const (BooleanLiteral false) <$> S.string "false")

charList2String :: List Char -> String
charList2String = fromCharArray <<< fromFoldable

stringLiteral :: Parser Char Pattern
stringLiteral = StringLiteral <$> stringLiteral'

stringLiteral' :: Parser Char String
stringLiteral' = do
  expected (C.char '"') "string literal"
  cut do
    cs <- many $ stringChar <|> unicodeEscape <|> standardEscape
    expected (C.char '"') "unterminated string literal"
    pure $ charList2String cs

numberLiteral :: Parser Char Pattern
numberLiteral = NumberLiteral <$> number

numericDtProps :: forall n. NumericDtProps n -> Parser Char n -> Parser Char (NumericDtProps n)
numericDtProps emptyDt numParser = dtProps emptyDt [
    dtProp "min"          numParser (\i ps -> ps { min = Just (Bound { value: i, inclusive: true }) }),
    dtProp "minExclusive" numParser (\i ps -> ps { min = Just (Bound { value: i, inclusive: false }) }),
    dtProp "max"          numParser (\i ps -> ps { max = Just (Bound { value: i, inclusive: true }) }),
    dtProp "maxExclusive" numParser (\i ps -> ps { max = Just (Bound { value: i, inclusive: false }) }),
    dtProp "multipleOf"   numParser (\i ps -> ps { multipleOf = Just i })]

intDataType :: Parser Char Pattern
intDataType = do
  S.string "Int"
  ps <- numericDtProps emptyNumericDtProps integer
  pure $ IntDataType ps

numberDataType :: Parser Char Pattern
numberDataType = do
  S.string "Number"
  ps <- numericDtProps emptyNumericDtProps number
  pure $ NumberDataType ps

regexLiteral :: Parser Char GenRegex
regexLiteral = do
    C.char '/'
    cut do
      cs <- many $ escapedSlash <|> C.notChar '/'
      C.char '/'
      fs <- maybe $ const ignoreCase <$> C.char 'i'
      makeRegex (charList2String cs) fs
  where
    escapedSlash :: Parser Char Char
    escapedSlash = const '/' <$> S.string "\\/"

makeRegex :: String -> RegexFlags -> Parser Char GenRegex
makeRegex pattern flags = case regex pattern flags of
  Right re  -> pure $ GenRegex re
  Left err -> expected fail err

stringDataTypeProps :: Parser Char StringDtProps
stringDataTypeProps = do
    S.string "String"
    ps <- dtProps emptyStringDtProps [
      dtProp "minLength" nonNegativeInt (\i ps -> ps { minLength = Just i }),
      dtProp "maxLength" nonNegativeInt (\i ps -> ps { maxLength = Just i }),
      dtProp "pattern"   regexProp      (\r ps -> ps { pattern = Just r })]
    pure ps
  where
    regexFromStringPattern :: Parser Char GenRegex
    regexFromStringPattern = do
      pattern <- stringLiteral'
      makeRegex pattern noFlags

    regexProp :: Parser Char GenRegex
    regexProp = regexFromStringPattern <|> regexLiteral

stringDataType :: Parser Char Pattern
stringDataType = StringDataType <$> stringDataTypeProps

groupParser :: Parser Char Pattern -> Parser Char Pattern
groupParser itemParser = commaSeparated '(' itemParser ')' (foldl group Empty)

withChoice :: Parser Char Pattern -> Parser Char Pattern
withChoice contentP = do
    first <- contentP
    cut do
      S.spaces
      (parseChoice first) <|> pure first
  where
    parseChoice :: Pattern -> Parser Char Pattern
    parseChoice first = do
      C.char '|'
      cut do
        S.spaces
        second <- lazyParser (defer \_ -> withChoice contentP)
        pure $ Choice first second

repeatable :: Parser Char Pattern -> Parser Char Pattern
repeatable valueParser = do
  value <- valueParser
  cut do
    S.spaces
    count <- (Just <$> repeatCount) <|> pure Nothing
    pure $ case count of
      Just c  -> Repeat value c
      Nothing -> value

repeatCount :: Parser Char RepeatCount
repeatCount =
    (const (RepeatCount { min: 0, max: Just 1 }) <$> C.char '?') <|>
    (const (RepeatCount { min: 0, max: Nothing }) <$> C.char '*') <|>
    (const (RepeatCount { min: 1, max: Nothing }) <$> C.char '+') <|>
    parseBounds
  where
    parseUpperBound :: Parser Char (Maybe Int)
    parseUpperBound = do
      C.char ','
      cut do
        S.spaces
        max <- (Just <$> nonNegativeInt) <|> pure Nothing
        S.spaces
        C.char '}'
        pure max

    parseBounds :: Parser Char RepeatCount
    parseBounds = do
      C.char '{'
      cut do
        S.spaces
        min <- nonNegativeInt
        S.spaces
        max <- (const (Just min) <$> C.char '}') <|> parseUpperBound
        pure $ RepeatCount { min, max }

list2Group :: List Pattern -> Pattern
list2Group ps = foldr group Empty ps -- used `foldr` to parse `(1, 2, 3)` as `Group 1 (Group 2 3)` and not `Group (Group 1, 2) 3`

arrayPattern :: Parser Char Pattern
arrayPattern = commaSeparated '[' arrayContent ']' (ArrayPattern <<< list2Group)  where
  arrayGroup :: Parser Char Pattern
  arrayGroup = lazyParser (defer \_ -> groupParser arrayContent)

  nonChoiceArrayContent :: Parser Char Pattern
  nonChoiceArrayContent =
    lazyParser (defer \_ -> nonChoiceValuePattern) <|>
    lazyParser (defer \_ -> arrayGroup)

  arrayContent :: Parser Char Pattern
  arrayContent = withChoice <<< repeatable $ lazyParser (defer \u -> nonChoiceArrayContent)

identifier :: Parser Char String
identifier = do
  c <- sat \c -> c == '_' || ((toLower c) >= 'a' && (toLower c) <= 'z')
  cut do
    cs <- many C.letter
    pure $ fromCharArray <<< fromFoldable $ c : cs

property :: Parser Char Pattern
property = do
    name <- propName
    S.spaces
    C.char ':'
    cut do
      S.spaces
      value <- valuePatternParser
      pure $ Property { name, value }
  where
    quotedProp :: Parser Char PropertyNamePattern
    quotedProp = LiteralName <$> stringLiteral'

    wildcardProp :: Parser Char PropertyNamePattern
    wildcardProp = WildcardName <$> stringDataTypeProps

    wildcardRegexShorthand :: Parser Char PropertyNamePattern
    wildcardRegexShorthand = (\re -> WildcardName emptyStringDtProps { pattern = Just re }) <$> regexLiteral

    propName :: Parser Char PropertyNamePattern
    propName =  wildcardProp <|> wildcardRegexShorthand <|> (LiteralName <$> identifier) <|> quotedProp

objectContent :: Parser Char Pattern
objectContent = commaSeparated '{' contentParser '}' list2Group where
  objectGroup :: Parser Char Pattern
  objectGroup = lazyParser (defer \_ -> groupParser contentParser)

  nonChoiceObjectContent :: Parser Char Pattern
  nonChoiceObjectContent =
    lazyParser (defer \_ -> property) <|>
    lazyParser (defer \_ -> objectGroup)

  contentParser :: Parser Char Pattern
  contentParser = withChoice <<< repeatable $ lazyParser (defer \u -> nonChoiceObjectContent)

objectOrNamedPattern :: Parser Char Pattern
objectOrNamedPattern = do
    base <- refinement
    (refinedObject base) <|> (case base of
                                 ObjectRefinement p -> pure $ Object p
                                 NamedRefinement n -> pure $ NamedPattern n)
  where
    namedRefinement :: Parser Char ObjectRefinement
    namedRefinement = do
      C.char '$'
      cut $ NamedRefinement <$> identifier

    objectRefinement :: Parser Char ObjectRefinement
    objectRefinement = ObjectRefinement <$> (lazyParser $ defer (\_ -> objectContent))

    refinement :: Parser Char ObjectRefinement
    refinement = namedRefinement <|> objectRefinement

    refinementSep :: Parser Char Unit
    refinementSep = do
      S.spaces
      S.string "with"
      S.spaces
      pure unit

    refinedObject :: ObjectRefinement -> Parser Char Pattern
    refinedObject base = do
      refinementSep
      cut $ do
        refs <- sepBy1 refinementSep refinement
        (let allRefs = base : refs
             allRefsNe = unsafePartial $ fromJust $ NonEmptyList.fromList $ List.reverse allRefs
         in pure $ RefinedObject allRefsNe)

nonChoiceValuePattern :: Parser Char Pattern
nonChoiceValuePattern =
    anyValue <|>
    null <|>
    booleanLiteral <|>
    booleanDataType <|>
    stringLiteral <|>
    stringDataType <|>
    numberLiteral <|>
    intDataType <|>
    numberDataType <|>
    regexStringShorthand <|>
    lazyParser (defer \_ -> arrayPattern) <|>
    lazyParser (defer \_ -> objectOrNamedPattern)
  where
    null :: Parser Char Pattern
    null = const Null <$> S.string "null"

    anyValue :: Parser Char Pattern
    anyValue = const Any <$> S.string "Any"

    -- allows using literal pattenr (f.ex. /[a-z]/i) instead of full String(pattern = /[a-z]/i)
    regexStringShorthand :: Parser Char Pattern
    regexStringShorthand = (\re -> StringDataType emptyStringDtProps { pattern = Just re }) <$> regexLiteral

valuePatternParser :: Parser Char Pattern
valuePatternParser = withChoice $ lazyParser (defer \_ -> nonChoiceValuePattern)

namedPatternDefinition :: Parser Char { name :: String, pattern :: Pattern }
namedPatternDefinition = do
  name <- identifier
  S.spaces
  C.char '='
  cut do
    S.spaces
    pattern <- valuePatternParser
    pure { name, pattern }

schemaParser :: Parser Char Schema
schemaParser = (sepBy S.spaces namedPatternDefinition) >>= toSchema where
  appendPattern :: Parser Char Schema -> { name :: String, pattern :: Pattern } -> Parser Char Schema
  appendPattern schemaP { name, pattern } = schemaP >>= \schema ->
    if isJust $ Schema.lookupPattern name schema then expected fail $ "Multiple definitions of pattern `" <> name <> "` found in schema."
    else pure $ Schema.add name pattern schema

  toSchema :: List { name :: String, pattern :: Pattern } -> Parser Char Schema
  toSchema = foldl appendPattern (pure Schema.empty)

jsonPathParser :: Parser Char JsonPath
jsonPathParser = do
    C.char '.'
    ns <- sepBy (C.char '.') $ idxNode <|> (KeyNode <$> identifier) <|> keyNode
    pure $ JsonPath $ Seq.fromFoldable ns
  where
    idxNode :: Parser Char JsonPathNode
    idxNode = do
      C.char '['
      idx <- nonNegativeInt
      C.char ']'
      pure $ IdxNode idx

    keyNode :: Parser Char JsonPathNode
    keyNode = do
      C.char '['
      key <- stringLiteral'
      C.char ']'
      pure $ KeyNode key
