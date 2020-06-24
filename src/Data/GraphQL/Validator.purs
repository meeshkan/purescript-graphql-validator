module Data.GraphQL.Gen where

import Prelude
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.GraphQL.AST (NamedType(..))
import Data.GraphQL.AST as AST
import Data.List (List(..))
import Data.List.Types (NonEmptyList(..))
import Data.Map as M
import Data.Map as Map
import Data.NonEmpty (NonEmpty(..))
import Data.Nullable (Nullable, null)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect.Exception (error, Error)
import Foreign (isNull, readArray, readBoolean, readNumber, readString)
import Foreign.Object as FO
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype JMap b
  = JMap (M.Map String b)

derive newtype instance eqJMap ∷ (Eq a) ⇒ Eq (JMap a)

instance readForeignJMap ∷ (ReadForeign a) ⇒ ReadForeign (JMap a) where
  readImpl f = do
    v <- (readImpl f)
    pure (JMap $ M.fromFoldable ((FO.toUnfoldable $ v) ∷ (Array (Tuple String a))))

jMapToObject ∷ forall a. (WriteForeign a) ⇒ JMap a -> FO.Object a
jMapToObject (JMap f) = FO.fromFoldable ((M.toUnfoldable f) ∷ (Array (Tuple String a)))

instance writeForeignJMap ∷ (WriteForeign a) ⇒ WriteForeign (JMap a) where
  writeImpl f = writeImpl (jMapToObject f)

data JSON
  = JObject (JMap JSON)
  | JArray (Array JSON)
  | JString String
  | JBoolean Boolean
  | JNumber Number
  | JNull

derive instance genericJSON :: Generic JSON _

instance readForeignJSON :: ReadForeign JSON where
  readImpl f =
    if (isNull f) then
      pure JNull
    else
      (JNumber <$> readNumber f)
        <|> (JBoolean <$> readBoolean f)
        <|> (JString <$> readString f)
        <|> (JArray <$> (readArray f >>= sequence <<< map readImpl))
        <|> (JObject <$> readImpl f)

instance writeForeignJSON :: WriteForeign JSON where
  writeImpl (JObject j) = writeImpl $ jMapToObject j
  writeImpl (JArray j) = writeImpl j
  writeImpl (JBoolean j) = writeImpl j
  writeImpl (JNumber j) = writeImpl j
  writeImpl (JString j) = writeImpl j
  writeImpl JNull = writeImpl (null :: Nullable Int) -- chose Int at random

instance eqJSON :: Eq JSON where
  eq a b = genericEq a b

validateAsEnum ∷ String → String → Either (NonEmptyList Error) Unit
validateAsEnum js nt = ?hole

validateAsScalar ∷ String → String → Either (NonEmptyList Error) Unit
validateAsScalar js nt = ?hole

validateValueAgainstType ∷ JSON → AST.Type → Either (NonEmptyList Error) Unit
validateValueAgainstType JNull (AST.Type_ListType _) = Right unit

validateValueAgainstType JNull (AST.Type_NamedType _) = Right unit

validateValueAgainstType JNull (AST.Type_NonNullType _) = Left (pure (error "Type is non-null in definition but null in JSON"))

validateValueAgainstType (JNumber _) (AST.Type_NamedType (NamedType "Int")) = Right unit

validateValueAgainstType (JNumber _) (AST.Type_NamedType (NamedType "Float")) = Right unit

validateValueAgainstType (JNumber _) (AST.Type_NamedType (NamedType _)) = Left (pure (error "Value in JSON must be number"))

validateValueAgainstType (JString _) (AST.Type_NamedType (NamedType "String")) = Right unit

validateValueAgainstType (JString s) (AST.Type_NamedType (NamedType nt)) = validateAsEnum s nt <|> validateAsScalar s nt

validateKVPairAgainstFieldDefinition ∷ Tuple String JSON → AST.T_FieldDefinition → Either (NonEmptyList Error) Unit
validateKVPairAgainstFieldDefinition (Tuple k v) fd =
  if (k /= fd.name) then
    (Left $ pure (error $ "Incorrect field name: expecting " <> fd.name <> " but received " <> k))
  else
    validateValueAgainstType v fd.type

validateFieldDefinitionsAgainstJSONObject ∷ JSON → List AST.T_FieldDefinition → Either (NonEmptyList Error) Unit
validateFieldDefinitionsAgainstJSONObject (JObject (JMap j)) fd = (sequence <<< fold) map (\js → map (validateKVPairAgainstFieldDefinition js) fd) (Map.toUnfoldable j)

validateFieldDefinitionsAgainstJSONObject _ fd = Left (pure (error "Cannot validate field definitions against anything other than an object"))

--validateJSONResposneAgainstSchema ∷ String → String → Either (NonEmptyList Error) Unit
