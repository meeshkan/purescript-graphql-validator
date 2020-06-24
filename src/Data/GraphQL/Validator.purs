module Data.GraphQL.Gen where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Writer (WriterT, execWriterT, tell, censor)
import Data.Array as A
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.GraphQL.AST as AST
import Data.Identity (Identity)
import Data.List (List(..), singleton, (:), fromFoldable, length)
import Data.List.Types (NonEmptyList)
import Data.Map as M
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, null)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Foreign (ForeignError, isNull, readArray, readBoolean, readNumber, readString)
import Foreign.Object as FO
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, readJSON, writeImpl)

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
altalt :: forall a. ValStack' a -> ValStack' a -> ValStack' a
altalt a b = do
  whereWeAre ← get
  env ← ask
  let
    _a = unwrap (runReaderT (execWriterT (evalStateT a whereWeAre)) env)
  if length _a == 0 then
    a
  else
    ( let
        _b = unwrap (runReaderT (execWriterT (evalStateT a whereWeAre)) env)
      in
        if length _b == 0 then b else censor (\w -> _a <> w) b
    )

infixl 3 altalt as <:>

newtype JMap
  = JMap (M.Map String JSON)

instance eqJMap ∷ Eq JMap where
  eq a b = genericEq a b

derive instance genericJMap ∷ Generic JMap _

instance jmapShow ∷ Show JMap where
  show s = genericShow s

instance readForeignJMap ∷ ReadForeign JMap where
  readImpl f = do
    v ← (readImpl f)
    pure (JMap $ M.fromFoldable ((FO.toUnfoldable $ v) ∷ (Array (Tuple String JSON))))

jMapToObject ∷ JMap → FO.Object JSON
jMapToObject (JMap f) = FO.fromFoldable ((M.toUnfoldable f) ∷ (Array (Tuple String JSON)))

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
  validateValueAgainstType
    (JObject (JMap m))
    (AST.Type_NamedType x)
    <:> validateAsObjectAgainstUnionDefinition m xs

validateAsObjectAgainstInterfaceOrObjectDefinition ∷ Map.Map String JSON → List AST.FieldDefinition → ValStack
validateAsObjectAgainstInterfaceOrObjectDefinition m id = validateFieldDefinitionsAgainstJSONObject (JObject (JMap m)) (map unwrap id)

validateAsObjectFromTDs ∷ Map.Map String JSON → List AST.TypeDefinition → ValStack
validateAsObjectFromTDs m Nil = taddle "Incoming type is an object, but no type corresponds to it"

validateAsObjectFromTDs m ((AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition ({ unionMemberTypes: Just (AST.UnionMemberTypes x) }))) : xs) = validateAsObjectAgainstUnionDefinition m x <:> validateAsObjectFromTDs m xs

validateAsObjectFromTDs m ((AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition ({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) = validateAsObjectAgainstInterfaceOrObjectDefinition m x <:> validateAsObjectFromTDs m xs

validateAsObjectFromTDs m ((AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition ({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) = validateAsObjectAgainstInterfaceOrObjectDefinition m x <:> validateAsObjectFromTDs m xs

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

validateValueAgainstType (JString s) (AST.Type_NamedType (AST.NamedType nt)) = validateAsEnum s nt <:> validateAsScalar s nt

validateValueAgainstType (JString _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "String"))) = oooook

validateValueAgainstType (JString s) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) = validateAsEnum s nt <:> validateAsScalar s nt

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

validateAllFieldDefs ∷ Map.Map String JSON → List AST.T_FieldDefinition → List (List ValStack)
validateAllFieldDefs j fd = (map (\js → map (validateKVPairAgainstFieldDefinition js) fd) (Map.toUnfoldable j))

validateFieldDefinitionsAgainstJSONObject ∷ JSON → List AST.T_FieldDefinition → ValStack
validateFieldDefinitionsAgainstJSONObject (JObject (JMap j)) fd = void (sequence $ fold (validateAllFieldDefs j fd))

validateFieldDefinitionsAgainstJSONObject _ fd = taddle "Cannot validate field definitions against anything other than an object"

decodeToMyJSON ∷ String → Either (NonEmptyList ForeignError) JSON
decodeToMyJSON = readJSON

--validateJSONResposneAgainstSchema ∷ String → String → ValStack
--validateJSONResposneAgainstSchema = do
--  either
