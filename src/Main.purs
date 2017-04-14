module Main (
  createValidator
) where

import Prelude
import Data.Array as Arr
import Data.String as Str
import JsonBlueprint.Validator as Validator
import Control.Monad.Eff.Exception (error, throwException)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Eulalie.Parser (eof, parse, Parser)
import Data.Eulalie.Result (ParseResult(..))
import Data.Eulalie.Stream (stream, Stream)
import Data.Foldable (intercalate)
import Data.Validation.Semigroup (unV)
import JsonBlueprint.Parser (valuePatternParser)
import JsonBlueprint.Pattern (Pattern)
import JsonBlueprint.Validator (Errors, ValidationResult, Error)

type Schema = String

type JsValidationError = { message :: String, path :: String, pattern :: String }
type JsValidationResult = { valid :: Boolean, errors :: Array JsValidationError }

type JsValidator = { validate :: Json -> JsValidationResult }

createValidator :: Schema -> JsValidator
createValidator schema = case createValidator' schema of
  Right validator -> validator
  Left errMsg -> unsafePerformEff $ throwException $ error errMsg

createValidator' :: Schema -> Either String JsValidator
createValidator' schema = do
  pattern <- parseSchema schema
  pure { validate: validate pattern }

validate :: Pattern -> Json -> JsValidationResult
validate schema json =
    res2Js $ Validator.validate json schema
  where
    res2Js :: ValidationResult -> JsValidationResult
    res2Js res =
      unV failure2Js (const valid) res

    failure2Js :: Errors -> JsValidationResult
    failure2Js es = { valid: false, errors: Arr.fromFoldable $ error2Js <$> es }

    error2Js :: Error -> JsValidationError
    error2Js { path, pattern, message } = { message, path: show path, pattern: show pattern }

    valid :: JsValidationResult
    valid = { valid: true, errors: [] }

parseSchema :: Schema -> Either String Pattern
parseSchema inputStr =
    case parse consumeInputParser input of
      Error({expected}) -> Left $ "failed to parse schema: " <> intercalate "; " expected
      Success({value}) -> Right $ value
  where
    input :: Stream Char
    input = stream $ Str.toCharArray inputStr

    consumeInputParser :: Parser Char Pattern
    consumeInputParser = do
      val <- valuePatternParser
      eof
      pure val
