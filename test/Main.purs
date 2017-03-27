module Test.Main where

import Prelude
import Data.Array as A
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.Eulalie.Parser (parse)
import Data.Eulalie.Result (ParseResult(..))
import Data.Eulalie.Stream (stream)
import Data.String (toCharArray)
import JsonBlueprint.Parser (valuePattern)
import JsonBlueprint.Pattern (Pattern)
import Test.QuickCheck (Result, quickCheck, (<?>), (===))

patternSerializationRoundtrip :: Pattern -> Result
patternSerializationRoundtrip p =
  let
    serialized = show p
    parsed = parse valuePattern (stream $ toCharArray serialized)
    result = case parsed of
      Error({expected}) -> false <?> (show $ A.fromFoldable expected)
      Success({value}) -> value === p
  in
    result

main :: forall e. Eff (err :: EXCEPTION, random :: RANDOM, console :: CONSOLE | e) Unit
main = quickCheck patternSerializationRoundtrip
