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
import Data.Array as A
import Data.Either (Either, either)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.GraphQL.AST as AST
import Data.GraphQL.Lens (getAllMutationDefinitions, getAllQueryDefinitions, getAllSubscriptionDefinitions, lensToFragmentDefinitions, lensToTypeDefinitions)
import Data.GraphQL.Parser (document, ignoreMe, operationDefinition)
import Data.GraphQL.Validator.Util (GraphQLResEnv, ValStack'', ValStackRes, altalt, dive, oooook, taddle, topLevelError, validateAsEnum, validateAsScalar, validationDoubleLoop)
import Data.Lens as L
import Data.List (List(..), filter, fromFoldable, head, singleton, (:))
import Data.List.NonEmpty (NonEmptyList, fromList)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
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
uw a env s = unwrap (runReaderT (evalStateT a s) env)

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

validateArrayAsListType ∷ Maybe (AST.SelectionSet) → List JSON → AST.Type → ValStackRes
validateArrayAsListType mss l t = do
  dive "*"
  fold <$> (sequence $ map (\j → validateValueAgainstType mss j t) l)

validateAsObjectAgainstUnionDefinition ∷ Maybe (AST.SelectionSet) → Map.Map String JSON → List AST.NamedType → ValStackRes
validateAsObjectAgainstUnionDefinition mss m Nil = taddle $ "Validated " <> show m <> " against a union definition, but couldn't find a valid type to match against"

validateAsObjectAgainstUnionDefinition mss m (x : xs) =
  validateValueAgainstType
    mss
    (JObject (JMap m))
    (AST.Type_NamedType x)
    `altalt`
      validateAsObjectAgainstUnionDefinition mss m xs

validateAsObjectAgainstInterfaceOrObjectDefinition ∷ Maybe (AST.SelectionSet) → Map.Map String JSON → List AST.FieldDefinition → ValStackRes
validateAsObjectAgainstInterfaceOrObjectDefinition mss m id = validateFieldDefinitionsAgainstJSONObject (JObject (JMap m)) (maybe Nil (\(AST.SelectionSet ls) → ls) mss) (map unwrap id)

validateAsObjectFromTDs ∷ Maybe (AST.SelectionSet) → Map.Map String JSON → List AST.TypeDefinition → ValStackRes
validateAsObjectFromTDs mss m Nil = taddle "Incoming type is an object, but no type corresponds to it"

validateAsObjectFromTDs mss m ((AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition ({ unionMemberTypes: Just (AST.UnionMemberTypes x) }))) : xs) =
  validateAsObjectAgainstUnionDefinition mss m x
    `altalt`
      validateAsObjectFromTDs mss m xs

validateAsObjectFromTDs mss m ((AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition ({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) =
  validateAsObjectAgainstInterfaceOrObjectDefinition mss m x
    `altalt`
      validateAsObjectFromTDs mss m xs

validateAsObjectFromTDs mss m ((AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition ({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) =
  validateAsObjectAgainstInterfaceOrObjectDefinition mss m x
    `altalt`
      validateAsObjectFromTDs mss m xs

validateAsObjectFromTDs mss m (_ : xs) = validateAsObjectFromTDs mss m xs

validateAsObject ∷ Maybe AST.SelectionSet → Map.Map String JSON → String → ValStackRes
validateAsObject mss o nt = do
  v ← ask
  validateAsObjectFromTDs mss o v.typeDefinitions

validateValueAgainstType ∷ Maybe AST.SelectionSet → JSON → AST.Type → ValStackRes
validateValueAgainstType _ JNull (AST.Type_ListType _) = oooook

validateValueAgainstType _ JNull (AST.Type_NamedType _) = oooook

validateValueAgainstType _ JNull (AST.Type_NonNullType _) = taddle "Type is non-null in definition but null in JSON"

validateValueAgainstType _ (JNumber _) (AST.Type_NamedType (AST.NamedType "Int")) = oooook

validateValueAgainstType _ (JNumber _) (AST.Type_NamedType (AST.NamedType "Float")) = oooook

validateValueAgainstType _ (JNumber _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "Int"))) = oooook

validateValueAgainstType _ (JNumber _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "Float"))) = oooook

validateValueAgainstType _ (JNumber _) t = taddle $ "Got a number but type is " <> show t

validateValueAgainstType _ (JString _) (AST.Type_NamedType (AST.NamedType "String")) = oooook

validateValueAgainstType _ (JString _) (AST.Type_NamedType (AST.NamedType "ID")) = oooook

validateValueAgainstType _ (JString s) (AST.Type_NamedType (AST.NamedType nt)) =
  validateAsEnum s nt
    `altalt`
      validateAsScalar s nt

validateValueAgainstType _ (JString _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "String"))) = oooook

validateValueAgainstType _ (JString _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "ID"))) = oooook

validateValueAgainstType _ (JString s) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) =
  validateAsEnum s nt
    `altalt`
      validateAsScalar s nt

validateValueAgainstType _ (JString _) t = taddle $ "Got a string but type is " <> show t

validateValueAgainstType _ (JBoolean _) (AST.Type_NamedType (AST.NamedType "Boolean")) = oooook

validateValueAgainstType _ (JBoolean _) t = taddle $ "Got a boolean but type is " <> show t

validateValueAgainstType mss (JArray a) (AST.Type_ListType (AST.ListType t)) = validateArrayAsListType mss a t

validateValueAgainstType mss (JArray a) (AST.Type_NonNullType (AST.NonNullType_ListType (AST.ListType t))) = validateArrayAsListType mss a t

validateValueAgainstType _ (JArray _) t = taddle $ "Got an array but type is " <> show t

validateValueAgainstType mss (JObject (JMap o)) (AST.Type_NamedType (AST.NamedType nt)) = validateAsObject mss o nt

validateValueAgainstType mss (JObject (JMap o)) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) = validateAsObject mss o nt

validateValueAgainstType _ (JObject _) t = taddle $ "Got an object but type is " <> show t

validateKVPairAgainstFieldDefinition ∷ List AST.Selection → Tuple String JSON → AST.T_FieldDefinition → ValStackRes
validateKVPairAgainstFieldDefinition ss (Tuple k v) fd = do
  fds ← selectionsToFields ss
  let
    field = head (filter (\(AST.Field f) → (==) k (fromMaybe f.name f.alias)) fds)
  maybe (taddle $ "Could not find matching field for name " <> k <> " on selection names " <> show (map (\(AST.Field f) -> f.name) fds))
    ( \(AST.Field realField) →
        if (realField.name /= fd.name) then
          taddle $ "Incorrect field name: expecting " <> fd.name <> " but received " <> k
        else do
          dive fd.name
          validateValueAgainstType realField.selectionSet v fd.type
    )
    field

validateFieldDefinitionsAgainstJSONObject ∷ JSON → List AST.Selection → List AST.T_FieldDefinition → ValStackRes
validateFieldDefinitionsAgainstJSONObject (JObject (JMap j)) ss fd =
  validationDoubleLoop
    (validateKVPairAgainstFieldDefinition ss)
    ((Map.toUnfoldable j) ∷ (List (Tuple String JSON)))
    fd

validateFieldDefinitionsAgainstJSONObject _ _ _ = taddle "Cannot validate field definitions against anything other than an object"

selectionToFields ∷ AST.Selection → ValStack'' GraphQLResEnv (List AST.Field)
selectionToFields (AST.Selection_Field f) = pure $ singleton f

selectionToFields (AST.Selection_InlineFragment (AST.InlineFragment { selectionSet: (AST.SelectionSet ss) })) = do
  o ← sequence $ map selectionToFields ss
  pure $ fold o

selectionToFields (AST.Selection_FragmentSpread (AST.FragmentSpread fs)) = do
  v ← ask
  let
    def = head (filter ((==) fs.fragmentName <<< _.fragmentName <<< unwrap) v.fragmentDefinitions)
  maybe
    (pure Nil) -- todo, add ::: taddle $ "Could not find fragment with name " <> fs.fragmentName
    ( \foundDef → do
        o ← (sequence <<< map selectionToFields <<< unwrap <<< _.selectionSet <<< unwrap) foundDef
        pure $ fold o
    )
    def

selectionsToFields ∷ List AST.Selection → ValStack'' GraphQLResEnv (List AST.Field)
selectionsToFields i = fold <$> (sequence $ map selectionToFields i)

decodeToJSON ∷ String → Either (NonEmptyList ForeignError) JSON
decodeToJSON = readJSON

validateJSONAgainstSchema'' ∷ JSON → List AST.Selection → List AST.T_FieldDefinition → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAgainstSchema'' json ss fdlist doc =
  maybe (pure unit) throwError
    $ fromList
        ( uw
            ( validateFieldDefinitionsAgainstJSONObject
                json
                ss
                fdlist
            )
            { typeDefinitions: (L.toListOf lensToTypeDefinitions doc), fragmentDefinitions: (L.toListOf lensToFragmentDefinitions doc) }
            Nil
        )

validateJSONAgainstSchema' ∷ JSON → AST.OperationDefinition → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAgainstSchema' json (AST.OperationDefinition_SelectionSet (AST.SelectionSet ss)) doc = validateJSONAgainstSchema'' json ss (getAllQueryDefinitions doc) doc

validateJSONAgainstSchema' json (AST.OperationDefinition_OperationType { operationType: AST.Query, selectionSet: (AST.SelectionSet ss) }) doc = validateJSONAgainstSchema'' json ss (getAllQueryDefinitions doc) doc

validateJSONAgainstSchema' json (AST.OperationDefinition_OperationType { operationType: AST.Mutation, selectionSet: (AST.SelectionSet ss) }) doc = validateJSONAgainstSchema'' json ss (getAllMutationDefinitions doc) doc

validateJSONAgainstSchema' json (AST.OperationDefinition_OperationType { operationType: AST.Subscription, selectionSet: (AST.SelectionSet ss) }) doc = validateJSONAgainstSchema'' json ss (getAllSubscriptionDefinitions doc) doc

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
