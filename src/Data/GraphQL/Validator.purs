module Data.GraphQL.Validator where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Writer (WriterT, execWriterT, tell, censor)
import Data.Array as A
import Data.Either (Either, either)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.GraphQL.AST (Document)
import Data.GraphQL.AST as AST
import Data.GraphQL.Lens (getAllMutationDefinitions, getAllQueryDefinitions, lensToTypeDefinitions)
import Data.GraphQL.Parser (document)
import Data.Identity (Identity)
import Data.Lazy (Lazy, defer, force)
import Data.Lens as L
import Data.List (List(..), singleton, (:), fromFoldable, length)
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

type GraphQLEnv
  = { typeDefinitions ∷ List AST.TypeDefinition }

type ValStack' a
  = StateT
      -- where we are in the path
      (List String)
      ( WriterT
          -- errors
          (List (Tuple (List String) String))
          ( ReaderT
              -- info about the document
              GraphQLEnv
              Identity
          )
      )
      a

type ValStack
  = ValStack' Unit

oooook ∷ ValStack
oooook = lift $ pure unit

taddle ∷ String → ValStack
taddle msg = do
  whereWeAre ← get
  tell $ singleton (Tuple whereWeAre msg)
  oooook

dive ∷ String → ValStack
dive segment = do
  whereWeAre ← get
  put $ whereWeAre <> (singleton segment)
  oooook

-- if both go to shit, we want to have the errors for both
altalt :: Lazy ValStack -> Lazy ValStack -> ValStack
altalt a b = do
  whereWeAre ← get
  env ← ask
  let
    a' = force a
  let
    _a = unwrap (runReaderT (execWriterT (evalStateT a' whereWeAre)) env)
  if length _a == 0 then
    oooook
  else
    ( let
        b' = force b
      in
        let
          _b = unwrap (runReaderT (execWriterT (evalStateT b' whereWeAre)) env)
        in
          if length _b == 0 then b' else censor (\w -> _a <> _b) oooook
    )

infixl 3 altalt as <:>

plusplus :: ValStack -> ValStack -> ValStack
plusplus a b = do
  whereWeAre ← get
  env ← ask
  let
    _a = unwrap (runReaderT (execWriterT (evalStateT a whereWeAre)) env)
  let
    _b = unwrap (runReaderT (execWriterT (evalStateT b whereWeAre)) env)
  if length _b == 0 && length _a == 0 then b else censor (\_ -> _a <> _b) oooook

infixl 3 plusplus as <+>

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

isInEnumValuesDefinition ∷ String → List AST.EnumValueDefinition → Boolean
isInEnumValuesDefinition s Nil = false

isInEnumValuesDefinition s ( AST.EnumValueDefinition
    ({ enumValue: (AST.EnumValue ev) })
    :
    xs
) = ev == s || isInEnumValuesDefinition s xs

isEnum ∷ String → List AST.TypeDefinition → Boolean
isEnum s Nil = false

isEnum s ( ( AST.TypeDefinition_EnumTypeDefinition
      ( AST.EnumTypeDefinition
        ({ enumValuesDefinition: Just (AST.EnumValuesDefinition evd) })
    )
  )
    :
    xs
) = isInEnumValuesDefinition s evd || isEnum s xs

isEnum s (_ : xs) = isEnum s xs

validateAsEnum ∷ String → String → ValStack
validateAsEnum js nt = do
  v ← ask
  if isEnum js v.typeDefinitions then
    oooook
  else
    taddle "Value is not a valid enum type in the document"

isScalar ∷ String → List AST.TypeDefinition → Boolean
isScalar s Nil = false

isScalar s ((AST.TypeDefinition_ScalarTypeDefinition x) : xs) = true

isScalar s (_ : xs) = isScalar s xs

validateAsScalar ∷ String → String → ValStack
validateAsScalar js nt = do
  v ← ask
  if (isScalar js v.typeDefinitions) then
    oooook
  else
    taddle "Value is not a valid scalar type in the document"

validateArrayAsListType ∷ List JSON → AST.Type → ValStack
validateArrayAsListType l t = do
  dive "*"
  void $ sequence (map (flip validateValueAgainstType t) l)

validateAsObjectAgainstUnionDefinition ∷ Map.Map String JSON → List AST.NamedType → ValStack
validateAsObjectAgainstUnionDefinition m Nil = taddle $ "Validated " <> show m <> " against a union definition, but couldn't find a valid type to match against"

validateAsObjectAgainstUnionDefinition m (x : xs) =
  ( defer \_ →
      validateValueAgainstType
        (JObject (JMap m))
        (AST.Type_NamedType x)
  )
    <:> (defer \_ → validateAsObjectAgainstUnionDefinition m xs)

validateAsObjectAgainstInterfaceOrObjectDefinition ∷ Map.Map String JSON → List AST.FieldDefinition → ValStack
validateAsObjectAgainstInterfaceOrObjectDefinition m id = validateFieldDefinitionsAgainstJSONObject (JObject (JMap m)) (map unwrap id)

validateAsObjectFromTDs ∷ Map.Map String JSON → List AST.TypeDefinition → ValStack
validateAsObjectFromTDs m Nil = taddle "Incoming type is an object, but no type corresponds to it"

validateAsObjectFromTDs m ((AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition ({ unionMemberTypes: Just (AST.UnionMemberTypes x) }))) : xs) = (defer \_ → validateAsObjectAgainstUnionDefinition m x) <:> (defer \_ → validateAsObjectFromTDs m xs)

validateAsObjectFromTDs m ((AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition ({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) = (defer \_ → validateAsObjectAgainstInterfaceOrObjectDefinition m x) <:> (defer \_ → validateAsObjectFromTDs m xs)

validateAsObjectFromTDs m ((AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition ({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) = (defer \_ → validateAsObjectAgainstInterfaceOrObjectDefinition m x) <:> (defer \_ → validateAsObjectFromTDs m xs)

validateAsObjectFromTDs m (_ : xs) = validateAsObjectFromTDs m xs

validateAsObject ∷ Map.Map String JSON → String → ValStack
validateAsObject o nt = do
  v ← ask
  validateAsObjectFromTDs o v.typeDefinitions

validateValueAgainstType ∷ JSON → AST.Type → ValStack
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

validateValueAgainstType (JString s) (AST.Type_NamedType (AST.NamedType nt)) = (defer \_ → validateAsEnum s nt) <:> (defer \_ → validateAsScalar s nt)

validateValueAgainstType (JString _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "String"))) = oooook

validateValueAgainstType (JString _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "ID"))) = oooook

validateValueAgainstType (JString s) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) = (defer \_ → validateAsEnum s nt) <:> (defer \_ → validateAsScalar s nt)

validateValueAgainstType (JString _) t = taddle $ "Got a string but type is " <> show t

validateValueAgainstType (JBoolean _) (AST.Type_NamedType (AST.NamedType "Boolean")) = oooook

validateValueAgainstType (JBoolean _) t = taddle $ "Got a boolean but type is " <> show t

validateValueAgainstType (JArray a) (AST.Type_ListType (AST.ListType t)) = validateArrayAsListType a t

validateValueAgainstType (JArray a) (AST.Type_NonNullType (AST.NonNullType_ListType (AST.ListType t))) = validateArrayAsListType a t

validateValueAgainstType (JArray _) t = taddle $ "Got an array but type is " <> show t

validateValueAgainstType (JObject (JMap o)) (AST.Type_NamedType (AST.NamedType nt)) = validateAsObject o nt

validateValueAgainstType (JObject (JMap o)) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) = validateAsObject o nt

validateValueAgainstType (JObject _) t = taddle $ "Got an object but type is " <> show t

validateKVPairAgainstFieldDefinition ∷ Tuple String JSON → AST.T_FieldDefinition → ValStack
validateKVPairAgainstFieldDefinition (Tuple k v) fd =
  if (k /= fd.name) then
    taddle $ "Incorrect field name: expecting " <> fd.name <> " but received " <> k
  else
    ( do
        dive fd.name
        validateValueAgainstType v fd.type
    )

validateKVPairAgainstFieldDefinitions ∷ Tuple String JSON → List AST.T_FieldDefinition → ValStack
validateKVPairAgainstFieldDefinitions kv Nil = (taddle $ "Could not find a match for kv pair: " <> show kv <> "\n")

validateKVPairAgainstFieldDefinitions kv (x : xs) = (defer \_ → validateKVPairAgainstFieldDefinition kv x) <:> (defer \_ → validateKVPairAgainstFieldDefinitions kv xs)

validateFieldDefinitionsAgainstJSONObject ∷ JSON → List AST.T_FieldDefinition → ValStack
validateFieldDefinitionsAgainstJSONObject (JObject (JMap j)) fd =
  foldl
    (<+>)
    oooook
    ( map (flip validateKVPairAgainstFieldDefinitions fd)
        ((Map.toUnfoldable j) :: (List (Tuple String JSON)))
    )

validateFieldDefinitionsAgainstJSONObject _ fd = taddle "Cannot validate field definitions against anything other than an object"

decodeToJSON ∷ String → Either (NonEmptyList ForeignError) JSON
decodeToJSON = readJSON

validateJSONAgainstSchema' ∷ JSON → Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAgainstSchema' json doc =
  maybe (pure unit) throwError
    $ fromList
        ( unwrap
            ( runReaderT
                ( execWriterT
                    ( evalStateT
                        ( ( defer \_ →
                              validateFieldDefinitionsAgainstJSONObject
                                json
                                (getAllQueryDefinitions doc)
                          )
                            <:> (defer \_ → validateFieldDefinitionsAgainstJSONObject json (getAllMutationDefinitions doc))
                        )
                        Nil
                    )
                )
                { typeDefinitions: (L.toListOf lensToTypeDefinitions doc) }
            )
        )

topLevelError =
  Tuple
    (singleton "[Root level]")
    "A graphql response must have a field called 'data' at the top level" ∷
    Tuple (List String) String

validateJSONAgainstSchema ∷ JSON → Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAgainstSchema (JObject (JMap m)) d =
  maybe
    (throwError $ pure topLevelError)
    (flip validateJSONAgainstSchema' d)
    (Map.lookup "data" m)

validateJSONAgainstSchema _ _ = throwError $ pure topLevelError

validateJSONAsStringAgainstSchema ∷ String → Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAsStringAgainstSchema s d =
  either
    (throwError <<< pure <<< Tuple (singleton "[JSON parser]") <<< show)
    (flip validateJSONAgainstSchema d)
    (decodeToJSON s)

validate_x_againstSchemaAsString ∷ ∀ x. (x → Document → Except (NonEmptyList (Tuple (List String) String)) Unit) → x → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validate_x_againstSchemaAsString x s d =
  either
    (throwError <<< pure <<< Tuple (singleton "[Schema parser]") <<< show)
    (x s)
    (runParser d document)

validateJSONAgainstSchemaAsString ∷ JSON → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONAgainstSchemaAsString = validate_x_againstSchemaAsString validateJSONAgainstSchema

validateJSONStringAgainstSchemaAsString ∷ String → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validateJSONStringAgainstSchemaAsString = validate_x_againstSchemaAsString validateJSONAsStringAgainstSchema

validateJSONStringAgainstSchemaAsString' ∷ String → String → Effect Unit
validateJSONStringAgainstSchemaAsString' a b =
  either
    (throwError <<< error <<< show)
    pure
    (unwrap $ runExceptT (validateJSONStringAgainstSchemaAsString a b))
