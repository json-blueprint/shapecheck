module JsonBlueprint.Schema (
  PatternDefName,
  Schema,
  add,
  empty,
  lookupPattern,
  validateAndSimplify,
  showProblems,
  singleton) where

import Prelude
import Data.List.NonEmpty as NonEmptyList
import Data.Set as Set
import Data.StrMap as StrMap
import Data.CatList (CatList)
import Data.Either (Either(..))
import Data.Foldable (find, foldl, intercalate)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.Maybe (Maybe(..), isJust)
import Data.NonEmpty ((:|))
import Data.Set (Set)
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import JsonBlueprint.Pattern (ObjectRefinement(..), Pattern(..), PropertyNamePattern, group, propertyNamePatterns)

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

singleton :: String -> Pattern -> Schema
singleton name pattern = Schema $ StrMap.insert name pattern StrMap.empty

lookupPattern :: PatternDefName -> Schema -> Maybe Pattern
lookupPattern name (Schema ps) = StrMap.lookup name ps

type PatternDefName = String
type SchemaProblem = { message :: String, inDef :: Maybe PatternDefName, pattern :: Pattern }
type SchemaProblems = CatList SchemaProblem

showProblems :: SchemaProblems -> String
showProblems ps = intercalate "\n" $ showProblem <$> ps

showProblem :: SchemaProblem -> String
showProblem { message, inDef: (Just patternDef) } = patternDef <> ": " <> message
showProblem { message } = message

validateAndSimplify :: Schema -> Either SchemaProblems Schema
validateAndSimplify schema@(Schema ps) = Schema <$> StrMap.fold foldF (pure StrMap.empty) ps where
  foldF :: Either SchemaProblems (StrMap Pattern) -> PatternDefName -> Pattern -> Either SchemaProblems (StrMap Pattern)
  foldF accu patternDef pattern =
    let simplified = (StrMap.singleton patternDef) <$> simplifyPattern schema (NonEmptyList.singleton patternDef) pattern
    in combineProblems StrMap.union accu simplified

simplifyPattern :: Schema -> NonEmptyList PatternDefName -> Pattern -> Either SchemaProblems Pattern
simplifyPattern schema patternDefs@(NonEmptyList (patternDef :| _) ) pattern  = case pattern of
    Group p1 p2 -> recurBinary Group p1 p2

    Choice p1 p2 -> recurBinary Choice p1 p2

    Repeat p count -> do
      sp <- recur p
      pure $ Repeat sp count

    Property { name, value } -> Property <<< { name, value: _ } <$> recur value

    ArrayPattern p -> ArrayPattern <$> recur p

    Object p -> Object <$> recur p

    RefinedObject refinements ->
      let
        resolved :: Either SchemaProblems (NonEmptyList Pattern)
        resolved = traverse resolveRefinement refinements
      in do
        contentPs <- resolved
        case NonEmptyList.uncons contentPs of
          { head, tail } ->
            let merged = foldl foldContentPatterns { propNames: propertyNamePatterns head, content: head } tail
            in pure $ Object merged.content

    p@NamedPattern name -> const p <$> (resolveNamedPatternRef name)

    other -> Right other
  where
    recur :: Pattern -> Either SchemaProblems Pattern
    recur p = simplifyPattern schema patternDefs p

    recurBinary :: (Pattern -> Pattern -> Pattern) -> Pattern -> Pattern -> Either SchemaProblems Pattern
    recurBinary combinePs p1 p2 = combineProblems combinePs (recur p1) (recur p2)

    recurWithDef :: PatternDefName -> Pattern -> Either SchemaProblems Pattern
    recurWithDef newDefName p = simplifyPattern schema (NonEmptyList.singleton newDefName <> patternDefs) p

    fail :: forall a. String -> Either SchemaProblems a
    fail message = Left $ pure { message, inDef: Just patternDef, pattern: pattern }

    resolveNamedPatternRef :: String -> Either SchemaProblems Pattern
    resolveNamedPatternRef name =
      case lookupPattern name schema of
        Just p -> pure p
        Nothing -> fail $ "reference to an undefined pattern " <> show (NamedPattern name)

    -- Takes an object refinement declaration and returns simplified object content pattern
    resolveRefinement :: ObjectRefinement -> Either SchemaProblems Pattern
    resolveRefinement (NamedRefinement name) =
      if isJust $ find ((==) name) patternDefs then
        fail $ "object refinements contain cycle: " <> intercalate ", " (name :| patternDefs)
      else do
        resolved <- resolveNamedPatternRef name
        case resolved of
          r@RefinedObject _ ->
            case recurWithDef name r of
              Right (Object p) -> pure p
              Right _ -> fail $ "simplification of refined object pattern did not yield an object pattern, this is a bug in the simplifier"
              left -> left
          Object p -> recurWithDef name p
          _ -> fail $ "definition of " <> show (NamedPattern patternDef) <> " is mixing in " <>
                      show (NamedPattern name) <> " which is not an object pattern; only object patterns can be used as mixins"
    resolveRefinement (ObjectRefinement contentP) = recur contentP

    foldContentPatterns :: { propNames :: Set PropertyNamePattern, content :: Pattern } -> Pattern -> { propNames :: Set PropertyNamePattern, content :: Pattern }
    foldContentPatterns { propNames, content } p =
      let
        noOverrides = dropOverriddenProps propNames p
        nextPropNames = (propertyNamePatterns noOverrides) <> propNames
      in
        { propNames: nextPropNames, content: group content noOverrides }

    dropOverriddenProps :: Set PropertyNamePattern -> Pattern -> Pattern
    dropOverriddenProps overridenProps = case _ of
        Group p1 p2 -> group (recurDrop p1) (recurDrop p2)
        other ->
          if containsOverridenProp other then Empty
          else other
      where
        recurDrop :: Pattern -> Pattern
        recurDrop p = dropOverriddenProps overridenProps p

        containsOverridenProp :: Pattern -> Boolean
        containsOverridenProp p = not Set.isEmpty $ Set.intersection overridenProps (propertyNamePatterns p)

combineProblems :: forall o. (o -> o -> o) -> Either SchemaProblems o -> Either SchemaProblems o -> Either SchemaProblems o
combineProblems combinePs a b = case Tuple a b of
  Tuple (Right ap) (Right bp)  -> Right $ combinePs ap bp
  Tuple (Left l1)  (Left l2)   -> Left $ l1 <> l2
  Tuple _          l@(Left  _) -> l
  Tuple l@(Left _) _           -> l
