module Main (
  createValidator,
  validateSchema,
  JsValidationError
) where

import Prelude
import Data.Argonaut.Core as Json
import Data.Array as Arr
import Data.Int as Int
import Data.String as Str
import JsonBlueprint.Validator as Validator
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION, error, throwException)
import Control.Monad.Eff.Unsafe (unsafePerformEff)
import Data.Argonaut.Core (Json)
import Data.Array (intercalate)
import Data.Either (Either(..))
import Data.Eulalie.Error (ParseError)
import Data.Eulalie.Parser (eof, parse, Parser)
import Data.Eulalie.Result (ParseResult(..))
import Data.Eulalie.Stream (Stream(..), atEnd, stream)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (fromCharArray)
import JsonBlueprint.JsonPath (JsonPath(..), JsonPathNode(..))
import JsonBlueprint.Parser (schemaParser)
import JsonBlueprint.Schema (PatternDefName, Schema, lookupPattern, patternNames, showProblems, validateAndSimplify)
import JsonBlueprint.Validator (ValidationError(..))

type SchemaString = String

newtype JsValidationError = JsValidationError {
  message :: String,
  path :: Array Json,
  pathString :: String,
  patternString :: String,
  children :: Array JsValidationError
}

type JsValidationResult = { valid :: Boolean, errors :: Array JsValidationError }

type JsValidator = {
  validate :: Fn2 PatternDefName Json JsValidationResult,
  patternNames :: Array String
}

createValidator :: SchemaString -> JsValidator
createValidator schema = case createValidator' schema of
  Right validator -> validator
  Left errMsg -> unsafePerformEff $ throwException $ error errMsg

createValidator' :: SchemaString -> Either String JsValidator
createValidator' schemaStr = do
    schema <- parseSchema schemaStr
    simplified <- checkSchema schema
    pure {
      validate: mkFn2 (unsafeValidate simplified),
      patternNames: patternNames schema
    }
  where
    checkSchema :: Schema -> Either String Schema
    checkSchema schema = case validateAndSimplify schema of
      Right s -> Right s
      Left problems -> Left $ "Supplied schema is invalid: \n" <> showProblems problems

unsafeValidate :: Schema -> PatternDefName -> Json -> JsValidationResult
unsafeValidate schema patternName json = unsafePerformEff $ validate schema patternName json

validate :: forall eff. Schema -> PatternDefName -> Json -> Eff (exception :: EXCEPTION | eff) JsValidationResult
validate schema patternName json =
    case lookupPattern patternName schema of
      Just pattern -> pure $ res2Js $ Validator.validate schema json pattern
      Nothing -> throwException $ error $ "No pattern with name `" <> patternName <> "` found in the schema."
  where
    res2Js :: Either (Array ValidationError) Unit -> JsValidationResult
    res2Js (Right _) = valid
    res2Js (Left es) = failure2Js es

    failure2Js :: (Array ValidationError) -> JsValidationResult
    failure2Js es = { valid: false, errors: Arr.fromFoldable $ error2Js <$> es }

    pathNode2Js :: JsonPathNode -> Json
    pathNode2Js (IdxNode i) = Json.fromNumber $ Int.toNumber i
    pathNode2Js (KeyNode n) = Json.fromString n

    path2Js :: JsonPath -> Array Json
    path2Js (JsonPath xs) = pathNode2Js <$> Arr.fromFoldable xs

    error2Js :: ValidationError -> JsValidationError
    error2Js (ValidationError { path, pattern, message, children }) =
      JsValidationError {
        message,
        path: path2Js path,
        pathString: show path,
        patternString: show pattern,
        children: Arr.fromFoldable $ error2Js <$> children
      }

    valid :: JsValidationResult
    valid = { valid: true, errors: [] }

type SchemaError = { message :: String, index :: Int }

validateSchema :: SchemaString -> { valid :: Boolean, errors :: Array SchemaError }
validateSchema inputStr = case parseSchema' inputStr of
  Right _  -> { valid: true, errors: mempty }
  Left err -> { valid: false, errors: pure err }

parseSchema :: SchemaString -> Either String Schema
parseSchema inputStr = case parseSchema' inputStr of
  Right s  -> pure s
  Left err -> Left err.message

parseSchema' :: SchemaString -> Either SchemaError Schema
parseSchema' inputStr =
    case parse consumeInputParser input of
      Error(err@{ input: Stream { cursor } }) -> Left $ { message: printErr err, index: cursor }
      Success({value}) -> Right $ value
  where
    input :: Stream Char
    input = stream $ Str.toCharArray inputStr

    consumeInputParser :: Parser Char Schema
    consumeInputParser = do
      val <- schemaParser
      eof
      pure val

printErr :: ParseError Char -> String
printErr { input: input@Stream { buffer, cursor }, expected } =
  "Expected " <> exp expected <> ", saw "
  <> (if atEnd input then "EOF" else quote $ Str.take 6 $ Str.drop cursor buf)
  where buf = fromCharArray buffer
        quote s = "\"" <> s <> "\""
        exp xs = intercalate " or " $ Arr.fromFoldable xs
