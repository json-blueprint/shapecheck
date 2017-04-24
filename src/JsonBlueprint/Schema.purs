module JsonBlueprint.Schema (
  PatternDefName,
  Schema,
  add,
  empty,
  lookupPattern,
  validateAndSimplify,
  showProblems) where

import Prelude
import Data.StrMap as StrMap
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.List (List)
import Data.Maybe (Maybe(..), isJust)
import Data.Sequence (Seq)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import JsonBlueprint.Pattern (Pattern(..))

newtype Schema = Schema (StrMap Pattern)

instance showSchema :: Show Schema where
  show (Schema ps) =
    let
      list = (StrMap.toUnfoldable ps) :: List (Tuple String Pattern)
      defs = (\(Tuple name pattern) -> name <> " = " <> show pattern) <$> list
    in
      intercalate "\n\n" defs

empty :: Schema
empty = Schema StrMap.empty

add :: String -> Pattern -> Schema -> Schema
add name pattern (Schema ps) = Schema $ StrMap.insert name pattern ps

lookupPattern :: PatternDefName -> Schema -> Maybe Pattern
lookupPattern name (Schema ps) = StrMap.lookup name ps

type PatternDefName = String
type SchemaProblem = { message :: String, inDef :: Maybe PatternDefName, pattern :: Pattern }
type SchemaProblems = Seq SchemaProblem

showProblems :: SchemaProblems -> String
showProblems ps = intercalate "\n" $ showProblem <$> ps

showProblem :: SchemaProblem -> String
showProblem { message, inDef: (Just patternDef) } = patternDef <> ": " <> message
showProblem { message } = message

validateAndSimplify :: Schema -> Either SchemaProblems Schema
validateAndSimplify schema@(Schema ps) = Schema <$> StrMap.fold foldF (pure StrMap.empty) ps where
  foldF :: Either SchemaProblems (StrMap Pattern) -> PatternDefName -> Pattern -> Either SchemaProblems (StrMap Pattern)
  foldF accu patternDef pattern =
    let simplified = (StrMap.singleton patternDef) <$> simplifyPattern schema patternDef pattern
    in combineProblems StrMap.union accu simplified

simplifyPattern :: Schema -> PatternDefName -> Pattern -> Either SchemaProblems Pattern
simplifyPattern schema patternDef  = case _ of
    Group p1 p2 -> recurBinary Group p1 p2

    Choice p1 p2 -> recurBinary Choice p1 p2

    Repeat p count -> do
      sp <- recur p
      pure $ Repeat sp count

    Property { name, value } -> Property <<< { name, value: _ } <$> recur value

    ArrayPattern p -> ArrayPattern <$> recur p

    Object p -> Object <$> recur p

    named@NamedPattern name ->
      if isJust $ lookupPattern name schema then Right named
      else Left $ pure { message: "reference to an undefined pattern " <> show named, inDef: Just patternDef , pattern: named }

    other -> Right other
  where
    recur :: Pattern -> Either SchemaProblems Pattern
    recur pattern = simplifyPattern schema patternDef pattern

    recurBinary :: (Pattern -> Pattern -> Pattern) -> Pattern -> Pattern -> Either SchemaProblems Pattern
    recurBinary combinePs p1 p2 = combineProblems combinePs (recur p1) (recur p2)

combineProblems :: forall o. (o -> o -> o) -> Either SchemaProblems o -> Either SchemaProblems o -> Either SchemaProblems o
combineProblems combinePs a b = case Tuple a b of
  Tuple (Right ap) (Right bp)  -> Right $ combinePs ap bp
  Tuple (Left l1)  (Left l2)   -> Left $ l1 <> l2
  Tuple _          l@(Left  _) -> l
  Tuple l@(Left _) _           -> l
