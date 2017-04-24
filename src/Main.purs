module Main (
  createValidator,
  JsValidationError
) where

import Prelude
import Data.Array as Arr
import Data.String as Str
import JsonBlueprint.Validator as Validator
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Eulalie.Parser (eof, parse, Parser)
import Data.Eulalie.Result (ParseResult(..))
import Data.Eulalie.Stream (stream, Stream)
import Data.Foldable (intercalate)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import JsonBlueprint.Parser (schemaParser)
import JsonBlueprint.Schema (PatternDefName, Schema, lookupPattern, showProblems, validateAndSimplify)
import JsonBlueprint.Validator (ValidationError(..), ValidationErrors)

type SchemaString = String

newtype JsValidationError = JsValidationError { message :: String, path :: String, pattern :: String, children :: Array JsValidationError }
type JsValidationResult = { valid :: Boolean, errors :: Array JsValidationError }

type JsValidator = { validate :: Fn2 PatternDefName Json JsValidationResult }

createValidator :: SchemaString -> JsValidator
createValidator schema = case createValidator' schema of
  Right validator -> validator
  Left errMsg -> unsafePerformEff $ throwException $ error errMsg

createValidator' :: SchemaString -> Either String JsValidator
createValidator' schemaStr = do
    schema <- parseSchema schemaStr
    simplified <- checkSchema schema
    pure { validate: mkFn2 (unsafeValidate simplified) }
  where
    checkSchema :: Schema -> Either String Schema
    checkSchema schema = case validateAndSimplify schema of
      Right s -> Right s
      Left problems -> Left $ "Supplied schema is invalid: \n" <> showProblems problems

unsafeValidate :: Schema -> PatternDefName -> Json -> JsValidationResult
unsafeValidate schema patternName json = unsafePerformEff $ validate schema patternName json

validate :: forall eff. Schema -> PatternDefName -> Json -> Eff (err :: EXCEPTION | eff) JsValidationResult
validate schema patternName json =
    case lookupPattern patternName schema of
      Just pattern -> pure $ res2Js $ Validator.validate schema json pattern
      Nothing -> throwException $ error $ "No pattern with name `" <> patternName <> "` found in the schema."
  where
    res2Js :: Either ValidationErrors Unit -> JsValidationResult
    res2Js (Right _) = valid
    res2Js (Left es) = failure2Js es

    failure2Js :: ValidationErrors -> JsValidationResult
    failure2Js es = { valid: false, errors: Arr.fromFoldable $ error2Js <$> es }

    error2Js :: ValidationError -> JsValidationError
    error2Js (ValidationError { path, pattern, message, children }) =
      JsValidationError { message, path: show path, pattern: show pattern, children: Arr.fromFoldable $ error2Js <$> children }

    valid :: JsValidationResult
    valid = { valid: true, errors: [] }

parseSchema :: SchemaString -> Either String Schema
parseSchema inputStr =
    case parse consumeInputParser input of
      Error({expected}) -> Left $ "failed to parse schema: " <> intercalate "; " expected
      Success({value}) -> Right $ value
  where
    input :: Stream Char
    input = stream $ Str.toCharArray inputStr

    consumeInputParser :: Parser Char Schema
    consumeInputParser = do
      val <- schemaParser
      eof
      pure val
