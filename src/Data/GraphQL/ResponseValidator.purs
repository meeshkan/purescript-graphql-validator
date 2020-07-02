module Data.GraphQL.ResponseValidator
  ( validateJSONAgainstSchema'
  , validateJSONAgainstSchema
  , validateJSONAsStringAgainstSchema
  , validate_x_againstSchemaAsString
  , validateJSONAgainstSchemaAsString
  , validateJSONStringAgainstSchemaAsString
  , validateJSONStringAndOperationDefStringAgainstSchemaAsString'
  , JSON(..)
  , JMap(..)
  ) where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (execWriterT)
import Data.Array as A
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.GraphQL.AST as AST
import Data.GraphQL.Lens (getAllMutationDefinitions, getAllQueryDefinitions, lensToFragmentDefinitions, lensToTypeDefinitions)
import Data.GraphQL.Parser (document, ignoreMe, operationDefinition)
import Data.GraphQL.Validator.Util (GraphQLResEnv, ValStackRes, altalt', dive, oooook, plusplus', taddle, topLevelError, validateAsEnum, validateAsScalar, validationDoubleLoop)
import Data.Lens as L
import Data.List (List(..), fromFoldable, singleton, (:))
import Data.List.NonEmpty (NonEmptyList, fromList)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, null)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (error)
import Foreign (ForeignError, isNull, readArray, readBoolean, readNumber, readString)
import Foreign.Object as FO
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, readJSON, writeImpl)
import Text.Parsing.Parser (runParser)

uw ∷ ValStackRes → GraphQLResEnv → (List String) → List (Tuple (List String) String)
uw a env s = unwrap (runReaderT (execWriterT (evalStateT a s)) env)

altalt ∷ ValStackRes → ValStackRes → ValStackRes
altalt = altalt' uw

plusplus ∷ ValStackRes → ValStackRes → ValStackRes
plusplus = plusplus' uw

newtype JMap
  = JMap (Map.Map String JSON)

instance eqJMap ∷ Eq JMap where
  eq a b = genericEq a b

derive instance genericJMap ∷ Generic JMap _

instance jmapShow ∷ Show JMap where
  show s = genericShow s

instance readForeignJMap ∷ ReadForeign JMap where
  readImpl f = do
    v ← (readImpl f)
    pure (JMap $ Map.fromFoldable ((FO.toUnfoldable $ v) ∷ (Array (Tuple String JSON))))

jMapToObject ∷ JMap → FO.Object JSON
jMapToObject (JMap f) = FO.fromFoldable ((Map.toUnfoldable f) ∷ (Array (Tuple String JSON)))

instance writeForeignJMap ∷ WriteForeign JMap where
  writeImpl f = writeImpl (jMapToObject f)

data JSON
  = JObject JMap
  | JArray (List JSON)
  | JString String
  | JBoolean Boolean
  | JNumber Number
  | JNull

derive instance genericJSON ∷ Generic JSON _

instance jsonShow ∷ Show JSON where
  show s = genericShow s

instance readForeignJSON ∷ ReadForeign JSON where
  readImpl f =
    if (isNull f) then
      pure JNull
    else
      (JNumber <$> readNumber f)
        <|> (JBoolean <$> readBoolean f)
        <|> (JString <$> readString f)
        <|> (JArray <$> (readArray f >>= sequence <<< map readImpl <<< fromFoldable))
        <|> (JObject <$> readImpl f)

instance writeForeignJSON ∷ WriteForeign JSON where
  writeImpl (JObject j) = writeImpl $ jMapToObject j
  writeImpl (JArray j) = writeImpl (A.fromFoldable j)
  writeImpl (JBoolean j) = writeImpl j
  writeImpl (JNumber j) = writeImpl j
  writeImpl (JString j) = writeImpl j
  writeImpl JNull = writeImpl (null ∷ Nullable Int) -- chose Int at random

instance eqJSON ∷ Eq JSON where
  eq a b = genericEq a b

validateArrayAsListType ∷ List JSON → AST.Type → ValStackRes
validateArrayAsListType l t = do
  dive "*"
  void $ sequence (map (flip validateValueAgainstType t) l)

validateAsObjectAgainstUnionDefinition ∷ Map.Map String JSON → List AST.NamedType → ValStackRes
validateAsObjectAgainstUnionDefinition m Nil = taddle $ "Validated " <> show m <> " against a union definition, but couldn't find a valid type to match against"

validateAsObjectAgainstUnionDefinition m (x : xs) =
  validateValueAgainstType
    (JObject (JMap m))
    (AST.Type_NamedType x)
    `altalt`
      validateAsObjectAgainstUnionDefinition m xs

validateAsObjectAgainstInterfaceOrObjectDefinition ∷ Map.Map String JSON → List AST.FieldDefinition → ValStackRes
validateAsObjectAgainstInterfaceOrObjectDefinition m id = validateFieldDefinitionsAgainstJSONObject (JObject (JMap m)) (map unwrap id)

validateAsObjectFromTDs ∷ Map.Map String JSON → List AST.TypeDefinition → ValStackRes
validateAsObjectFromTDs m Nil = taddle "Incoming type is an object, but no type corresponds to it"

validateAsObjectFromTDs m ((AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition ({ unionMemberTypes: Just (AST.UnionMemberTypes x) }))) : xs) =
  validateAsObjectAgainstUnionDefinition m x
    `altalt`
      validateAsObjectFromTDs m xs

validateAsObjectFromTDs m ((AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition ({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) =
  validateAsObjectAgainstInterfaceOrObjectDefinition m x
    `altalt`
      validateAsObjectFromTDs m xs

validateAsObjectFromTDs m ((AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition ({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) =
  validateAsObjectAgainstInterfaceOrObjectDefinition m x
    `altalt`
      validateAsObjectFromTDs m xs

validateAsObjectFromTDs m (_ : xs) = validateAsObjectFromTDs m xs

validateAsObject ∷ Map.Map String JSON → String → ValStackRes
validateAsObject o nt = do
  v ← ask
  validateAsObjectFromTDs o v.typeDefinitions

validateValueAgainstType ∷ JSON → AST.Type → ValStackRes
validateValueAgainstType JNull (AST.Type_ListType _) = oooook

validateValueAgainstType JNull (AST.Type_NamedType _) = oooook

validateValueAgainstType JNull (AST.Type_NonNullType _) = taddle "Type is non-null in definition but null in JSON"

validateValueAgainstType (JNumber _) (AST.Type_NamedType (AST.NamedType "Int")) = oooook

validateValueAgainstType (JNumber _) (AST.Type_NamedType (AST.NamedType "Float")) = oooook

validateValueAgainstType (JNumber _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "Int"))) = oooook

validateValueAgainstType (JNumber _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "Float"))) = oooook

validateValueAgainstType (JNumber _) t = taddle $ "Got a number but type is " <> show t

validateValueAgainstType (JString _) (AST.Type_NamedType (AST.NamedType "String")) = oooook

validateValueAgainstType (JString _) (AST.Type_NamedType (AST.NamedType "ID")) = oooook

validateValueAgainstType (JString s) (AST.Type_NamedType (AST.NamedType nt)) =
  validateAsEnum s nt
    `altalt`
      validateAsScalar s nt

validateValueAgainstType (JString _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "String"))) = oooook

validateValueAgainstType (JString _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "ID"))) = oooook

validateValueAgainstType (JString s) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) =
  validateAsEnum s nt
    `altalt`
      validateAsScalar s nt

validateValueAgainstType (JString _) t = taddle $ "Got a string but type is " <> show t

validateValueAgainstType (JBoolean _) (AST.Type_NamedType (AST.NamedType "Boolean")) = oooook

validateValueAgainstType (JBoolean _) t = taddle $ "Got a boolean but type is " <> show t

validateValueAgainstType (JArray a) (AST.Type_ListType (AST.ListType t)) = validateArrayAsListType a t

validateValueAgainstType (JArray a) (AST.Type_NonNullType (AST.NonNullType_ListType (AST.ListType t))) = validateArrayAsListType a t

validateValueAgainstType (JArray _) t = taddle $ "Got an array but type is " <> show t

validateValueAgainstType (JObject (JMap o)) (AST.Type_NamedType (AST.NamedType nt)) = validateAsObject o nt

validateValueAgainstType (JObject (JMap o)) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) = validateAsObject o nt

validateValueAgainstType (JObject _) t = taddle $ "Got an object but type is " <> show t

validateKVPairAgainstFieldDefinition ∷ Tuple String JSON → AST.T_FieldDefinition → ValStackRes
validateKVPairAgainstFieldDefinition (Tuple k v) fd =
  if (k /= fd.name) then
    taddle $ "Incorrect field name: expecting " <> fd.name <> " but received " <> k
  else do
    dive fd.name
    validateValueAgainstType v fd.type

validateFieldDefinitionsAgainstJSONObject ∷ JSON → List AST.T_FieldDefinition → ValStackRes
validateFieldDefinitionsAgainstJSONObject (JObject (JMap j)) fd =
  validationDoubleLoop
    plusplus
    altalt
    validateKVPairAgainstFieldDefinition
    ((Map.toUnfoldable j) ∷ (List (Tuple String JSON)))
    fd

validateFieldDefinitionsAgainstJSONObject _ fd = taddle "Cannot validate field definitions against anything other than an object"

decodeToJSON ∷ String → Either (NonEmptyList ForeignError) JSON
decodeToJSON = readJSON

validateJSONAgainstSchema' ∷ JSON → AST.OperationDefinition → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAgainstSchema' json opdef doc =
  maybe (pure unit) throwError
    $ fromList
        ( uw
            ( validateFieldDefinitionsAgainstJSONObject
                json
                (getAllQueryDefinitions doc)
                `altalt`
                  validateFieldDefinitionsAgainstJSONObject json (getAllMutationDefinitions doc)
            )
            { typeDefinitions: (L.toListOf lensToTypeDefinitions doc), fragmentDefinitions: (L.toListOf lensToFragmentDefinitions doc) }
            Nil
        )

validateJSONAgainstSchema ∷ JSON → AST.OperationDefinition → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAgainstSchema (JObject (JMap m)) o d =
  maybe
    (throwError $ pure topLevelError)
    (\j → validateJSONAgainstSchema' j o d)
    (Map.lookup "data" m)

validateJSONAgainstSchema _ _ _ = throwError $ pure topLevelError

validateJSONAsStringAgainstSchema ∷ String → AST.OperationDefinition → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAsStringAgainstSchema s o d =
  either
    (throwError <<< pure <<< Tuple (singleton "[JSON parser]") <<< show)
    (\j → validateJSONAgainstSchema j o d)
    (decodeToJSON s)

validate_x_againstSchemaAsString ∷ ∀ x. (x → AST.OperationDefinition → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit) → x → String → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validate_x_againstSchemaAsString x s o d =
  either
    (throwError <<< pure <<< Tuple (singleton "[Schema parser]") <<< show)
    ( \doc →
        either
          (throwError <<< pure <<< Tuple (singleton "[Opdef parser]") <<< show)
          (\opdef → x s opdef doc)
          (runParser o (ignoreMe *> operationDefinition))
    )
    (runParser d document)

validateJSONAgainstSchemaAsString ∷ JSON → String → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAgainstSchemaAsString = validate_x_againstSchemaAsString validateJSONAgainstSchema

validateJSONStringAgainstSchemaAsString ∷ String → String → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONStringAgainstSchemaAsString = validate_x_againstSchemaAsString validateJSONAsStringAgainstSchema

validateJSONStringAndOperationDefStringAgainstSchemaAsString' ∷ String → String → String → Effect Unit
validateJSONStringAndOperationDefStringAgainstSchemaAsString' json opdef schema =
  either
    (throwError <<< error <<< show)
    pure
    (unwrap $ runExceptT (validateJSONStringAgainstSchemaAsString json opdef schema))
