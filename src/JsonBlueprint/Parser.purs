module JsonBlueprint.Parser where

import Prelude
import Data.Eulalie.Char as C
import Data.Eulalie.String as S
import Control.Alt ((<|>))
import Data.Array (fromFoldable)
import Data.Char (fromCharCode)
import Data.Char.Unicode (isHexDigit)
import Data.Eulalie.Parser (Parser, cut, expected, fail, many, sat)
import Data.Foldable (class Foldable)
import Data.Int (fromStringAs, hexadecimal)
import Data.List.Lazy (replicateM)
import Data.Maybe (Maybe(..))
import Data.String (fromCharArray)
import JsonBlueprint.Pattern (Pattern(..))

stringChar :: Parser Char Char
stringChar =  sat \c -> c /= '\\' && c /= '"'

standardEscape :: Parser Char Char
standardEscape = expected standardEscape' "standard escape sequence" where
  standardEscape' = do
    C.char '\\'
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
unicodeEscape = expected unicodeEscape' "unicode escape"

unicodeEscape' :: Parser Char Char
unicodeEscape' = do
    S.string "\\u"
    ds <- replicateM 4 hexDigit
    decodeChar ds
  where
    decodeChar :: forall f. (Foldable f) => f Char -> Parser Char Char
    decodeChar cs =
      let optCharCode = (fromFoldable >>> fromCharArray >>> (fromStringAs hexadecimal)) cs in
        case optCharCode of
          Just charCode -> pure $ fromCharCode charCode
          Nothing -> fail

booleanDataType :: Parser Char Pattern
booleanDataType = expected (S.string "Boolean" <#> (\_ -> BooleanDataType)) "boolean data type"

booleanLiteral :: Parser Char Pattern
booleanLiteral = expected booleanLiteral' "boolean literal" where
  booleanLiteral' = (S.string "true" <#> \_ -> BooleanLiteral true)
                <|> (S.string "false" <#> \_ -> BooleanLiteral false)

stringLiteral :: Parser Char Pattern
stringLiteral = expected stringLiteral' "string literal" where
  stringLiteral' = do
    C.char '"'
    cs <- cut $ many $ stringChar <|> unicodeEscape <|> standardEscape
    C.char '"'
    pure $ StringLiteral $ fromFoldable >>> fromCharArray $ cs

stringDataType :: Parser Char Pattern
stringDataType = expected stringDataType' "string data type" where
  stringDataType' = S.string  "String" <#> (\_ -> StringDataType { minLength: Nothing, maxLength: Nothing })

valuePattern :: Parser Char Pattern
valuePattern =
  booleanLiteral <|>
  booleanDataType <|>
  stringLiteral <|>
  stringDataType
