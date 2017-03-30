module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Random (RANDOM)
import Jack (jackMain)

-- patternSerializationRoundtrip :: Pattern -> Result
-- patternSerializationRoundtrip p =
--   let
--     serialized = show p
--     parsed = parse valuePattern (stream $ toCharArray serialized)
--     result = case parsed of
--       Error({expected}) -> false <?> (show $ A.fromFoldable expected)
--       Success({value}) -> value === p
--   in
--     result

main :: forall e. Eff (random :: RANDOM, console :: CONSOLE | e) Unit
main =
  jackMain [
    "Test.PatternProps"
  ]
