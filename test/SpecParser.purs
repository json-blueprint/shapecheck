module Test.SpecParser where

import Prelude
import Data.Array as Array
import Data.Eulalie.Error as Error
import Data.String as Str
import JsonBlueprint.Schema as Schema
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.CatList (CatList, cons, null, uncons)
import Data.Either (Either(..))
import Data.Eulalie.Parser (eof, parse, Parser)
import Data.Eulalie.Result (ParseResult(..))
import Data.Eulalie.Stream (stream, Stream)
import Data.Foldable (foldMap, foldl, intercalate)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.String (split, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import JsonBlueprint.JsonPath (JsonPath)
import JsonBlueprint.Parser (jsonPathParser, schemaParser, valuePatternParser)
import JsonBlueprint.Pattern (Pattern)
import JsonBlueprint.Schema (Schema)
import Text.Markdown.SlamDown (Block(..), CodeBlockType(..), Inline(..), SlamDown, SlamDownP(SlamDown))
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Markdown.SlamDown.Traverse (everything)

type Markdown = String

type Spec = { name :: String, pattern :: Pattern, schema :: Schema, docs :: Array SampleDoc }
type SampleDoc = { name :: String, json :: Json, expectedErrors :: Array JsonPath, tags :: Array String }

parseSpec :: Markdown -> Either String Spec
parseSpec mdSource = do
    SlamDown blocks <- (parseMd mdSource) :: Either String SlamDown
    parsed <- foldl parseBlock (Right initialState) blocks
    completeSpec parsed
  where
    initialState :: Spec'
    initialState = { name: Nothing, pattern: Nothing, schema: Nothing, docs: mempty }

doParse :: forall o. String -> Parser Char o -> String -> Either String o
doParse parserDesc parser inputStr =
    case parse consumeInputParser input of
      Error(err) -> Left $ "failed to parse " <> parserDesc <> ": " <> Error.print err
      Success({value}) -> Right $ value
  where
    input :: Stream Char
    input = stream $ toCharArray inputStr

    consumeInputParser :: Parser Char o
    consumeInputParser = do
      val <- parser
      eof
      pure val

parseBlock :: Either String Spec' -> Block String -> Either String Spec'
parseBlock (Right s) (Header 1 is) = Right $ s { name = Just (renderText is) }
parseBlock (Right s) (Header 2 is) = Right $ s { docs = cons doc s.docs } where
  doc :: SampleDoc'
  doc = { name: renderText is, json: Nothing, expectedErrors: mempty, tags: [] }
parseBlock (Right s) (CodeBlock (Fenced _ "jsbp") ls) = (\p -> s { pattern = Just p, schema = Just $ Schema.singleton "Root" p }) <$> doParse "pattern" valuePatternParser (intercalate "\n" ls)
parseBlock (Right s) (CodeBlock (Fenced _ "jsbp-schema") ls) = do
  schema <- doParse "schema" schemaParser (intercalate "\n" ls)
  case Schema.validateAndSimplify schema of
    Right simplified ->
      case Schema.lookupPattern "Root" simplified of
        Just p -> pure $ s { pattern = Just p, schema = Just simplified }
        Nothing -> Left "Schema must declare a 'Root' pattern which will be the one JSON documents are validated against."
    Left problems -> Left $ intercalate ", " $ (\problem -> problem. message) <$> problems

parseBlock (Right s) (CodeBlock (Fenced _ "json") ls) = do
  json <- jsonParser $ intercalate "\n" ls
  case uncons s.docs of
    Just (Tuple doc docs) -> Right $ s { docs = cons (doc { json = Just json }) docs }
    Nothing -> Left "found sample document (fenced `json` code block) with no name (preceding 2nd-level heading)"
parseBlock (Right s) (Blockquote ( (Paragraph is) : Nil )) =
  let
    tagString = renderText is
    tags = split (Str.Pattern " ") tagString
  in
    case uncons s.docs of
      Just (Tuple doc docs) -> Right $ s { docs = cons (doc { tags = tags }) docs }
      Nothing -> Left "found tags for sample document with no name (preceding 2nd-level heading)"
parseBlock (Right s) (Lst _ ((b : bs) : _)) = case b of
    Paragraph is -> case renderText is of
      "Valid" -> Right s
      "Invalid" ->
        case uncons s.docs of
          Just (Tuple doc docs) ->
            (\ee -> s { docs = cons (doc { expectedErrors = ee }) docs }) <$> parseExpectedErrors bs
          Nothing -> Left "found validation outcome spec with no preceding document name header (2nd-level heading)"
      other -> Left $ unexpectedShape <> " '" <> other <> "'"
    other -> Left $ unexpectedShape <> " " <> show other
  where
    unexpectedShape = "unexpected validation outcome shape: expected bullet item 'Valid' | 'Invalid' but found"
parseBlock s _ = s

parseExpectedErrors :: List (Block String) -> Either String (CatList JsonPath)
parseExpectedErrors bs =
    if null inlineCode then
      Left "sample document marked as invalid but no expected errors provided"
    else
      traverse (doParse "JSON path" jsonPathParser) inlineCode
  where
    inlineCode :: CatList String
    inlineCode = everything (\_ -> mempty) inlineCode' (SlamDown bs)

    inlineCode' :: Inline String -> CatList String
    inlineCode' (Code _ codeStr) = pure codeStr
    inlineCode' _ = mempty

renderText :: List (Inline String) -> String
renderText is = foldMap renderText' is

renderText' :: Inline String -> String
renderText' (Str str)   = str
renderText' (Entity e)  = e
renderText' Space       = " "
renderText' LineBreak   = "\n"
renderText' SoftBreak   = " "
renderText' (Emph is)   = renderText is
renderText' (Strong is) = renderText is
renderText' (Code _ s)  = s
renderText' (Link is _) = renderText is
renderText' _           = ""

type Spec' = { name :: Maybe String, pattern :: Maybe Pattern, schema :: Maybe Schema, docs :: CatList SampleDoc' }
type SampleDoc' = { name :: String, json :: Maybe Json, expectedErrors :: CatList JsonPath, tags :: Array String }

completeDoc :: SampleDoc' -> Either String SampleDoc
completeDoc { name, json, expectedErrors, tags } = do
  js <- maybe (Left $ "JSON code block not found for sample document '" <> name <> "'") Right json
  pure $ { name, json: js, expectedErrors: Array.fromFoldable expectedErrors, tags }

completeSpec :: Spec' -> Either String Spec
completeSpec { name, pattern, schema, docs } = do
  n <- maybe (Left "No name defined for spec. Each spec should start with a 1-st level header that specifies it's name.") Right name
  p <- maybe (Left $ "No pattern defined in spec '" <> n <> "'. It should be provided in a `jsbp` or `jsbp-schema` fenced code block.") Right pattern
  s <- maybe (Left $ "No schema defined in spec '" <> n <> "'. It should be provided in a `jsbp` or `jsbp-schema` fenced code block.") Right schema
  ds <- traverse completeDoc docs
  pure $ { name: n, pattern: p, schema: s, docs: Array.reverse $ Array.fromFoldable ds }
