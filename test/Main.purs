module Test.Main where

import Prelude
import Data.Sequence as Seq
import Node.FS as F
import Node.FS.Aff as FS
import Test.Unit.Assert as Assert
import Control.Monad.Aff (Aff, Canceler, launchAff, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (intersect, null, uncons)
import Data.Either (Either(..))
import Data.Foldable (elem, foldl, intercalate)
import Data.Maybe (Maybe(..))
import Data.Sequence (Seq)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import JsonBlueprint.JsonPath (JsonPath)
import JsonBlueprint.Pattern (Pattern)
import JsonBlueprint.Schema (Schema)
import JsonBlueprint.Validator (ValidationError(..), validate)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readdir)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath, basename, concat)
import Test.SpecParser (Markdown, parseSpec, SampleDoc)
import Test.Unit (failure, success, suite, test, TestSuite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

skipSpecsTagged :: Array String
skipSpecsTagged = ["StrictInt"]

main :: forall e. Eff (err :: EXCEPTION, console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, fs :: F.FS | e)
                        (Canceler (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR, fs :: F.FS | e))
main = launchAff do
  -- specs <- loadSpecs "./specs"
  specs <- loadSpecs "./specs"
  liftEff' $ runTest do
    specs

loadSpecs :: forall e1 e2. FilePath -> Aff (fs :: F.FS | e1) (TestSuite e2)
loadSpecs path =
    FS.stat path >>= \stats ->
      if isDirectory stats then
        do
          children <- readdir path
          chSpecs <- childSpecs $ (\c -> concat [path, c]) <$> children
          pure $ suite (basename path) chSpecs
      else
        do
          specMd <- FS.readTextFile UTF8 path
          pure $ spec2Test (basename path) specMd
  where
    childSpecs :: forall ep1 ep2. Array FilePath -> Aff (fs :: F.FS | ep1) (TestSuite ep2)
    childSpecs paths = do
      chs <- traverse loadSpecs paths
      pure $ case uncons chs of
        Just { head, tail } -> foldl bind head (const <$> tail)
        Nothing -> test "no specs in this directory" $ failure "add some specs to avoid failure"

    spec2Test :: forall ep. String -> Markdown -> TestSuite ep
    spec2Test testFileName md =
      case parseSpec md of
        Left err -> test testFileName $ Assert.assert err false
        Right { name, pattern, schema, docs } -> test name $ assertions pattern schema (filterDocs docs)

    filterDocs :: Seq SampleDoc -> Seq SampleDoc
    filterDocs = Seq.filter (\d -> null $ intersect d.tags skipSpecsTagged)

    assertions :: forall e. Pattern -> Schema -> Seq SampleDoc -> Aff e Unit
    assertions pattern schema docs = case Seq.uncons $ (assertion pattern schema) <$> docs of
      Just (Tuple a as) -> foldl bind a (const <$> as)
      Nothing -> failure "no sample documents in this spec"

    assertion :: forall e. Pattern -> Schema -> SampleDoc -> Aff e Unit
    assertion pattern schema doc =
        if doc.expectedErrors == actualErrorPaths then
          success
        else
          failure ("incorrect validation of sample document '" <> doc.name <> "':\n" <> intercalate "\n" problems)
      where
        actualErrors :: Seq ValidationError
        actualErrors = case validate schema doc.json pattern of
          Left errs -> errs
          Right _ -> Seq.empty

        actualErrorPaths :: Seq JsonPath
        actualErrorPaths = (\(ValidationError err) -> err.path) <$> actualErrors

        unexpected :: Seq ValidationError
        unexpected = Seq.filter (\(ValidationError err) -> not $ elem err.path doc.expectedErrors) actualErrors

        missing :: Seq JsonPath
        missing = Seq.filter (\i -> not $ elem i actualErrorPaths) doc.expectedErrors

        problems :: Seq String
        problems =
          ((\(ValidationError err) -> "    - unexpected error at `" <> show err.path <> "`: " <> err.message) <$> unexpected) <>
          ((\jp -> "    - missing error at `" <> show jp <> "`") <$> missing)
