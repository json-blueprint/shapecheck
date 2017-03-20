module JsonBlueprint.Parser where

import Prelude
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Char (fromCharCode)
import Data.Char.Unicode (isHexDigit)
-- import Data.Eulalie.Parser (Parser(..))
import Data.Foldable (class Foldable)
import Data.Int (fromStringAs, hexadecimal)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import JsonBlueprint.Pattern (Pattern(..))
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.Combinators ((<?>), many)
import Text.Parsing.StringParser.String (char, noneOf, oneOf, satisfy, string)

stringChar :: Parser Char
stringChar = noneOf ['\\', '"']

standardEscape :: Parser Char
standardEscape = do
  try $ char '\\'
  c <- try $ oneOf ['\\', '/', '"', 'b', 'f', 'n', 'r', 't']
  pure $ case c of
    'b' -> '\b'
    'f' -> '\f'
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    other -> other

hexDigit :: Parser Char
hexDigit = satisfy isHexDigit <?> "hex digit"

unicodeEscape :: Parser Char
unicodeEscape = do
    try $ string "\\u"
    ds <- replicateM 4 hexDigit
    decodeChar ds
  where
    decodeChar :: forall f. (Foldable f) => f Char -> Parser Char
    decodeChar cs =
      let optCharCode = (fromFoldable >>> fromCharArray >>> (fromStringAs hexadecimal)) cs in
        case optCharCode of
          Just charCode -> pure $ fromCharCode charCode
          Nothing -> fail "invalid unicode escape sequence"

stringLiteral :: Parser String
stringLiteral = do
  char '"'
  cs <- many $ stringChar <|> standardEscape <|> unicodeEscape
  char '"'
  pure $ fromFoldable >>> fromCharArray $ cs

booleanDataType :: Parser Pattern
booleanDataType = try $ string "Boolean" <#> (\_ -> BooleanDataType)

booleanLiteral :: Parser Pattern
booleanLiteral = (string "true" <#> \_ -> BooleanLiteral true)
             <|> (string "false" <#> \_ -> BooleanLiteral false)

stringDataType :: Parser Pattern
stringDataType = try $ string  "String" <#> (\_ -> StringDataType { minLength: Nothing, maxLength: Nothing })
