module Test.PatternProps where

import Prelude
import Data.Array as A
import Data.Eulalie.Parser (parse)
import Data.Eulalie.Result (ParseResult(..))
import Data.Eulalie.Stream (stream)
import Data.String (toCharArray)
import Jack (Property, counterexample, forAll, property, (===))
import JsonBlueprint.Parser (valuePattern)
import Test.PatternGens (genValuePattern)

prop_parsing_shown_pattern_should_yield_equal_pattern :: Property
prop_parsing_shown_pattern_should_yield_equal_pattern =
  forAll genValuePattern \p ->
    let
      serialized = show p
      parsed = parse valuePattern (stream $ toCharArray serialized)
      result = case parsed of
        Error({expected}) -> counterexample ("parsing failure: " <> (show $ A.fromFoldable expected)) $ property false
        Success({value}) -> value === p
    in
      result
