module JsonBlueprint.Parser where

import Prelude
import Data.Eulalie.Char as C
import Data.Eulalie.String as S
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Array.Partial (head, tail)
import Data.Char (fromCharCode)
import Data.Char.Unicode (isHexDigit)
import Data.Eulalie.Parser (Parser, cut, either, expected, fail, many, sat, sepBy)
import Data.Foldable (class Foldable, foldl)
import Data.Int (fromString, fromStringAs, hexadecimal)
import Data.Lazy (Lazy, defer, force)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray, singleton)
import JsonBlueprint.Pattern (Pattern(..), RepeatCount(..), group)
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

nonNegativeInt :: Parser Char Int
nonNegativeInt = expected nonNegativeInt' "non-negative integer" where
  nonNegativeInt' = zeroP <|> do
    first <- C.oneOf "123456789"
    rest <- C.many C.digit
    case fromString (singleton first <> rest) of
      Just n -> pure n
      Nothing -> fail

prop :: forall v d. String -> Parser Char v -> (v -> d -> d) -> Parser Char (d -> d)
prop name valP f = do
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

-- TODO: improve error reporting for misspelled property names
props :: forall d. d -> Array (Parser Char (d -> d)) -> Parser Char d
props a b = (propList a b) <|> pure a
  where
    propList :: d -> Array (Parser Char (d -> d)) -> Parser Char d
    propList dt propParsers = do
      S.spaces
      C.char '('
      cut do
        S.spaces
        ps <- sepBy commaSeparator (reduce propParsers)
        S.spaces
        C.char ')'
        S.spaces
        pure $ foldl (#) dt ps

    reduce :: Array (Parser Char (d -> d)) -> Parser Char (d -> d)
    reduce [] = pure id
    reduce ps = unsafePartial $ foldl either (head ps) (tail ps)

booleanDataType :: Parser Char Pattern
booleanDataType = S.string "Boolean" <#> (\_ -> BooleanDataType)

booleanLiteral :: Parser Char Pattern
booleanLiteral = (const (BooleanLiteral true)  <$> S.string "true")
             <|> (const (BooleanLiteral false) <$> S.string "false")

stringLiteral :: Parser Char Pattern
stringLiteral = do
  expected (C.char '"') "string literal"
  cut do
    cs <- many $ stringChar <|> unicodeEscape <|> standardEscape
    expected (C.char '"') "unterminated string literal"
    pure $ StringLiteral $ fromFoldable >>> fromCharArray $ cs

stringDataType :: Parser Char Pattern
stringDataType = do
  S.string "String"
  ps <- props { minLength: Nothing, maxLength: Nothing } [
    prop "minLength" nonNegativeInt (\i ps -> ps { minLength = Just i }),
    prop "maxLength" nonNegativeInt (\i ps -> ps { maxLength = Just i })]
  pure $ StringDataType ps

groupParser :: Parser Char Pattern -> Parser Char Pattern
groupParser itemParser = do
  C.char '('
  cut do
    S.spaces
    items <- sepBy commaSeparator itemParser
    S.spaces
    C.char ')'
    pure $ foldl group Empty items

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

arrayPattern :: Parser Char Pattern
arrayPattern = do
    C.char '['
    cut do
      S.spaces
      vs <- sepBy commaSeparator arrayContent
      S.spaces
      C.char ']'
      pure $ ArrayPattern vs
  where
    arrayGroup :: Parser Char Pattern
    arrayGroup = lazyParser (defer \_ -> groupParser arrayContent)

    nonChoiceArrayContent :: Parser Char Pattern
    nonChoiceArrayContent =
      lazyParser (defer \u -> nonChoiceValuePattern) <|>
      lazyParser (defer \u -> arrayGroup)

    arrayContent :: Parser Char Pattern
    arrayContent = withChoice <<< repeatable $ lazyParser (defer \u -> nonChoiceArrayContent)

nonChoiceValuePattern :: Parser Char Pattern
nonChoiceValuePattern =
  booleanLiteral <|>
  booleanDataType <|>
  stringLiteral <|>
  stringDataType <|>
  lazyParser (defer \u -> arrayPattern)

valuePattern :: Parser Char Pattern
valuePattern = withChoice <<< repeatable $ nonChoiceValuePattern
