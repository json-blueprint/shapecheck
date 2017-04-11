module JsonBlueprint.Validator where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Foldable (intercalate)
import Data.Monoid (class Monoid, mempty)
import Data.Sequence (Seq, snoc)
import JsonBlueprint.Pattern (Pattern(..), propNameRequiresQuoting, RepeatCount(..))

-- | simple jq-like path pointing to a locaiton in a JSON document
newtype JsonPath = JsonPath (Seq JsonPathNode)

instance semigroupJsonPath :: Semigroup JsonPath where
  append (JsonPath ns) (JsonPath ms) = JsonPath $ ns <> ms

instance monoidJsonPath :: Monoid JsonPath where
  mempty = JsonPath (mempty :: Seq JsonPathNode)

instance showJsonPath :: Show JsonPath where
  show (JsonPath ns) = "." <> intercalate "." (show <$> ns)

instance eqJsonPath :: Eq JsonPath where
  eq (JsonPath n1s) (JsonPath n2s) = n1s == n2s

appendNode :: JsonPath -> JsonPathNode -> JsonPath
appendNode (JsonPath ns) n = JsonPath $ snoc ns n

infixr 5 appendNode as \

data JsonPathNode = KeyNode String
                  | IdxNode Int

instance eqJsonPathNode :: Eq JsonPathNode where
  eq (KeyNode k1) (KeyNode k2) = k1 == k2
  eq (IdxNode i1) (IdxNode i2) = i2 == i2
  eq _ _ = false

instance showJsonPathNode :: Show JsonPathNode where
  show (KeyNode key) | propNameRequiresQuoting key = "[" <> show key <> "]"
                     | otherwise = key
  show (IdxNode idx) = "[" <> show idx <> "]"

-- | type of validation errors
type Error = { path :: JsonPath, pattern :: Pattern, message :: String }

-- | nullable Pattern matches empty input (f.ex. in array content validation,
-- | the array content is valid if derivative of last array element is nullable)
nullable :: Pattern -> Boolean
nullable Empty = true
nullable (Group p1 p2) = nullable p1 && nullable p2
nullable (Choice p1 p2) = nullable p1 || nullable p2
nullable (Repeat p (RepeatCount { min })) = nullable p || min <= 0
nullable _ = false

validate :: Json -> Pattern -> Seq Error
validate json pattern = mempty
