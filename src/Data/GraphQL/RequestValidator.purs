module Data.GraphQL.RequestValidator
  ( validateOperationDefinitionAgainstSchema
  , validateOperationDefinitionAsStringAgainstSchema
  , validate_x_againstSchemaAsString
  , validateOperationDefinitionAgainstSchemaAsString
  , validateOperationDefinitionStringAgainstSchemaAsString
  , validateOperationDefinitionStringAgainstSchemaAsString'
  ) where

import Prelude
import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.State (evalStateT)
import Data.Either (Either, either)
import Data.GraphQL.AST as AST
import Data.GraphQL.Lens (getAllMutationDefinitions, getAllQueryDefinitions, getAllSubscriptionDefinitions, lensToFragmentDefinitions, lensToTypeDefinitions)
import Data.GraphQL.Parser as GP
import Data.GraphQL.Validator.Util (GraphQLReqEnv, ValStackReq, altalt, dive, oooook, taddle, validateAsEnum, validateAsScalar, validationDoubleLoop, validationOuterLoop)
import Data.Lens as L
import Data.List (List(..), singleton, length, filter, (:), head)
import Data.List.NonEmpty (fromList)
import Data.List.Types (NonEmptyList)
import Data.Maybe (maybe, Maybe(..))
import Data.Newtype (unwrap)
import Data.Set (fromFoldable, empty, difference)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (error)
import Text.Parsing.Parser (ParseError, runParser)

uw ∷ ValStackReq → GraphQLReqEnv → (List String) → List (Tuple (List String) String)
uw a env s = unwrap (runReaderT (evalStateT a s) env)

validateListValueAsListType ∷ List AST.Value → AST.Type → ValStackReq
validateListValueAsListType l t = do
  dive "*"
  validationOuterLoop (flip validateValueAgainstType t) l

validateAsObjectAgainstInputObjectDefinition ∷ List AST.Argument → List AST.InputValueDefinition → ValStackReq
validateAsObjectAgainstInputObjectDefinition a ad = validateArgumentsAgainstArgumentsDefinition (Just (AST.Arguments a)) (Just (AST.ArgumentsDefinition ad))

validateAsInputObjectFromTDs ∷ List AST.Argument → List AST.TypeDefinition → ValStackReq
validateAsInputObjectFromTDs m Nil = taddle "Incoming type is an object, but no type corresponds to it"

validateAsInputObjectFromTDs m ((AST.TypeDefinition_InputObjectTypeDefinition (AST.InputObjectTypeDefinition ({ inputFieldsDefinition: Just (AST.InputFieldsDefinition x) }))) : xs) = validateAsObjectAgainstInputObjectDefinition m x

validateAsInputObjectFromTDs m (_ : xs) = validateAsInputObjectFromTDs m xs

validateAsInputObject ∷ List AST.Argument → String → ValStackReq
validateAsInputObject o nt = do
  v ← ask
  validateAsInputObjectFromTDs o v.typeDefinitions

validateValueAgainstType ∷ AST.Value → AST.Type → ValStackReq
validateValueAgainstType (AST.Value_Variable _) _ = oooook -- as a variable could be anything

validateValueAgainstType (AST.Value_NullValue _) (AST.Type_ListType _) = oooook

validateValueAgainstType (AST.Value_NullValue _) (AST.Type_NamedType _) = oooook

validateValueAgainstType (AST.Value_NullValue _) (AST.Type_NonNullType _) = taddle "Type is non-null in definition but null in field"

validateValueAgainstType (AST.Value_IntValue _) (AST.Type_NamedType (AST.NamedType "Int")) = oooook

validateValueAgainstType (AST.Value_FloatValue _) (AST.Type_NamedType (AST.NamedType "Float")) = oooook

validateValueAgainstType (AST.Value_IntValue _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "Int"))) = oooook

validateValueAgainstType (AST.Value_FloatValue _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "Float"))) = oooook

validateValueAgainstType (AST.Value_IntValue _) t = taddle $ "Got an int but type is " <> show t

validateValueAgainstType (AST.Value_FloatValue _) t = taddle $ "Got a float but type is " <> show t

validateValueAgainstType (AST.Value_StringValue _) (AST.Type_NamedType (AST.NamedType "String")) = oooook

validateValueAgainstType (AST.Value_StringValue _) (AST.Type_NamedType (AST.NamedType "ID")) = oooook

validateValueAgainstType (AST.Value_EnumValue (AST.EnumValue s)) (AST.Type_NamedType (AST.NamedType nt)) =
  validateAsEnum s nt
    `altalt`
      validateAsScalar s nt

validateValueAgainstType (AST.Value_StringValue _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "String"))) = oooook

validateValueAgainstType (AST.Value_StringValue _) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType "ID"))) = oooook

validateValueAgainstType (AST.Value_EnumValue (AST.EnumValue s)) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) =
  validateAsEnum s nt
    `altalt`
      validateAsScalar s nt

validateValueAgainstType (AST.Value_StringValue _) t = taddle $ "Got a string but type is " <> show t

validateValueAgainstType (AST.Value_EnumValue _) t = taddle $ "Got a enum but type is " <> show t

validateValueAgainstType (AST.Value_BooleanValue _) (AST.Type_NamedType (AST.NamedType "Boolean")) = oooook

validateValueAgainstType (AST.Value_BooleanValue _) t = taddle $ "Got a boolean but type is " <> show t

validateValueAgainstType (AST.Value_ListValue (AST.ListValue lv)) (AST.Type_ListType (AST.ListType t)) = validateListValueAsListType lv t

validateValueAgainstType (AST.Value_ListValue (AST.ListValue lv)) (AST.Type_NonNullType (AST.NonNullType_ListType (AST.ListType t))) = validateListValueAsListType lv t

validateValueAgainstType (AST.Value_ListValue _) t = taddle $ "Got an array but type is " <> show t

validateValueAgainstType (AST.Value_ObjectValue (AST.ObjectValue ov)) (AST.Type_NamedType (AST.NamedType nt)) = validateAsInputObject ov nt

validateValueAgainstType (AST.Value_ObjectValue (AST.ObjectValue ov)) (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) = validateAsInputObject ov nt

validateValueAgainstType (AST.Value_ObjectValue _) t = taddle $ "Got an object but type is " <> show t

validateNameCorrespondsToSimpleType ∷ String → ValStackReq
validateNameCorrespondsToSimpleType t = case t of
  "Int" → oooook
  "Float" → oooook
  "ID" → oooook
  "String" → oooook
  "Boolean" → oooook
  s → validateAsEnum s t `altalt` validateAsScalar s t

validateUnderlyingTypeIsNotObject ∷ AST.Type → ValStackReq
validateUnderlyingTypeIsNotObject (AST.Type_NamedType (AST.NamedType t)) = validateNameCorrespondsToSimpleType t

validateUnderlyingTypeIsNotObject (AST.Type_ListType (AST.ListType t)) = validateUnderlyingTypeIsNotObject t

validateUnderlyingTypeIsNotObject (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType t))) = validateNameCorrespondsToSimpleType t

validateUnderlyingTypeIsNotObject (AST.Type_NonNullType (AST.NonNullType_ListType (AST.ListType t))) = validateUnderlyingTypeIsNotObject t

validateAsObjectAgainstUnionDefinition ∷ List AST.Selection → List AST.NamedType → ValStackReq
validateAsObjectAgainstUnionDefinition ss Nil = taddle $ "Validated " <> show ss <> " against a union definition, but couldn't find a valid type to match against"

validateAsObjectAgainstUnionDefinition ss ((AST.NamedType nt) : xs) =
  validateSelectionSetAgainstStringType
    (AST.SelectionSet ss)
    nt
    `altalt`
      validateAsObjectAgainstUnionDefinition ss xs

validateAsObjectAgainstInterfaceOrObjectDefinition ∷ List AST.Selection → List AST.FieldDefinition → ValStackReq
validateAsObjectAgainstInterfaceOrObjectDefinition ss fd = validateSelectionSetAgainstFieldDefinitions (AST.SelectionSet ss) (map unwrap fd)

validateAsObjectFromTDs ∷ String → List AST.Selection → List AST.TypeDefinition → ValStackReq
validateAsObjectFromTDs n m Nil = taddle "Incoming type is an object, but no type corresponds to it"

validateAsObjectFromTDs n m ((AST.TypeDefinition_UnionTypeDefinition (AST.UnionTypeDefinition td@({ unionMemberTypes: Just (AST.UnionMemberTypes x) }))) : xs) =
  if (n == td.name) then
    (validateAsObjectAgainstUnionDefinition m x)
  else
    (taddle $ "Name does not match")
      `altalt`
        validateAsObjectFromTDs n m xs

validateAsObjectFromTDs n m ((AST.TypeDefinition_InterfaceTypeDefinition (AST.InterfaceTypeDefinition td@({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) =
  if (n == td.name) then
    (validateAsObjectAgainstInterfaceOrObjectDefinition m x)
  else
    (taddle $ "Name does not match")
      `altalt`
        validateAsObjectFromTDs n m xs

validateAsObjectFromTDs n m ((AST.TypeDefinition_ObjectTypeDefinition (AST.ObjectTypeDefinition td@({ fieldsDefinition: Just (AST.FieldsDefinition x) }))) : xs) =
  if (n == td.name) then
    (validateAsObjectAgainstInterfaceOrObjectDefinition m x)
  else
    (taddle $ "Name does not match")
      `altalt`
        validateAsObjectFromTDs n m xs

validateAsObjectFromTDs n m (_ : xs) = validateAsObjectFromTDs n m xs

validateSelectionSetAgainstStringType ∷ AST.SelectionSet → String → ValStackReq
validateSelectionSetAgainstStringType (AST.SelectionSet ss) tn = do
  v ← ask
  validateAsObjectFromTDs tn ss v.typeDefinitions

validateFieldAgainstType ∷ AST.T_Field → AST.Type → ValStackReq
validateFieldAgainstType { selectionSet: Nothing } t = validateUnderlyingTypeIsNotObject t

validateFieldAgainstType { selectionSet: (Just s) } (AST.Type_NamedType (AST.NamedType nt)) = validateSelectionSetAgainstStringType s nt

validateFieldAgainstType f@{ selectionSet: (Just s) } (AST.Type_ListType (AST.ListType t)) = validateFieldAgainstType f t

validateFieldAgainstType { selectionSet: (Just s) } (AST.Type_NonNullType (AST.NonNullType_NamedType (AST.NamedType nt))) = validateSelectionSetAgainstStringType s nt

validateFieldAgainstType f@{ selectionSet: (Just s) } (AST.Type_NonNullType (AST.NonNullType_ListType (AST.ListType t))) = validateFieldAgainstType f t

isArgumentNonNull ∷ AST.InputValueDefinition → Boolean
isArgumentNonNull (AST.InputValueDefinition { type: (AST.Type_NonNullType _) }) = true

isArgumentNonNull _ = false

getAllNonNullArguments ∷ List AST.InputValueDefinition → List AST.InputValueDefinition
getAllNonNullArguments = filter isArgumentNonNull

validateArgumentAgainstArgumentDefinition ∷ AST.Argument → AST.InputValueDefinition → ValStackReq
validateArgumentAgainstArgumentDefinition (AST.Argument a) (AST.InputValueDefinition ivd) =
  if a.name /= ivd.name then
    taddle $ "Argument names do not match"
  else do
    dive a.name
    validateValueAgainstType a.value ivd.type

validateArgumentsAgainstArgumentsDefinition ∷ Maybe AST.Arguments → Maybe AST.ArgumentsDefinition → ValStackReq
validateArgumentsAgainstArgumentsDefinition Nothing Nothing = oooook

validateArgumentsAgainstArgumentsDefinition (Just _) Nothing = taddle $ "Providing arguments to a field that takes no arguments"

validateArgumentsAgainstArgumentsDefinition Nothing (Just (AST.ArgumentsDefinition ad)) =
  if (length (getAllNonNullArguments ad) == 0) then
    oooook
  else
    (taddle $ "Did not supply arguments, but there are non-null arguments in the field definition.")

validateArgumentsAgainstArgumentsDefinition (Just (AST.Arguments a)) (Just (AST.ArgumentsDefinition ad)) =
  let
    diff =
      difference
        (fromFoldable $ map (_.name <<< unwrap) (getAllNonNullArguments ad))
        (fromFoldable $ map (_.name <<< unwrap) a)
  in
    if diff /= empty then
      taddle ("Missing required arguments: " <> show diff)
    else
      validationDoubleLoop
        validateArgumentAgainstArgumentDefinition
        a
        ad

validateSelectionAgainstFieldDefinition ∷ List AST.T_FieldDefinition → AST.Selection → AST.T_FieldDefinition → ValStackReq
validateSelectionAgainstFieldDefinition incomingFD (AST.Selection_Field (AST.Field f)) fd =
  if (f.name /= fd.name) then
    taddle $ "Incorrect field name: expecting " <> fd.name <> " but received " <> f.name
  else
    ( do
        dive fd.name
        ( (<>) <$> validateArgumentsAgainstArgumentsDefinition f.arguments fd.argumentsDefinition
            <*> validateFieldAgainstType f fd.type
        )
    )

validateSelectionAgainstFieldDefinition incomingFD (AST.Selection_FragmentSpread (AST.FragmentSpread fs)) fd = do
  v <- ask
  maybe
    (taddle $ "Could not find fragment with name " <> fs.fragmentName)
    ((flip validateSelectionSetAgainstFieldDefinitions incomingFD) <<< _.selectionSet <<< unwrap)
    (head $ filter ((==) fs.fragmentName <<< _.fragmentName <<< unwrap) v.fragmentDefinitions)

validateSelectionAgainstFieldDefinition incomingFD (AST.Selection_InlineFragment (AST.InlineFragment ilf)) fd = validateSelectionSetAgainstFieldDefinitions ilf.selectionSet incomingFD

validateSelectionSetAgainstFieldDefinitions ∷ AST.SelectionSet → List AST.T_FieldDefinition → ValStackReq
validateSelectionSetAgainstFieldDefinitions (AST.SelectionSet ss) fd =
  validationDoubleLoop
    (validateSelectionAgainstFieldDefinition fd)
    ss
    fd

validateOperationDefinitionAgainstSchema ∷ AST.OperationDefinition → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateOperationDefinitionAgainstSchema (AST.OperationDefinition_SelectionSet ss) doc = validateOperationDefinitionAgainstSchema (AST.OperationDefinition_OperationType { operationType: AST.Query, name: Nothing, variableDefinitions: Nothing, directives: Nothing, selectionSet: ss }) doc

validateOperationDefinitionAgainstSchema (AST.OperationDefinition_OperationType ot) doc =
  maybe (pure unit) throwError
    $ fromList
        ( uw
            ( validateSelectionSetAgainstFieldDefinitions ot.selectionSet
                ( case ot.operationType of
                    AST.Query → getAllQueryDefinitions doc
                    AST.Mutation → getAllMutationDefinitions doc
                    AST.Subscription → getAllSubscriptionDefinitions doc
                )
            )
            { typeDefinitions: (L.toListOf lensToTypeDefinitions doc), fragmentDefinitions: (L.toListOf lensToFragmentDefinitions doc) }
            Nil
        )

decodeToOperationDefinition ∷ String → Either ParseError AST.OperationDefinition
decodeToOperationDefinition s = runParser s GP.operationDefinition

validateOperationDefinitionAsStringAgainstSchema ∷ String → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateOperationDefinitionAsStringAgainstSchema s d =
  either
    (throwError <<< pure <<< Tuple (singleton "[OperationDefinition parser]") <<< show)
    (flip validateOperationDefinitionAgainstSchema d)
    (decodeToOperationDefinition s)

validate_x_againstSchemaAsString ∷ ∀ x. (x → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit) → x → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validate_x_againstSchemaAsString x s d =
  either
    (throwError <<< pure <<< Tuple (singleton "[Schema parser]") <<< show)
    (x s)
    (runParser d GP.document)

validateOperationDefinitionAgainstSchemaAsString ∷ AST.OperationDefinition → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validateOperationDefinitionAgainstSchemaAsString = validate_x_againstSchemaAsString validateOperationDefinitionAgainstSchema

validateOperationDefinitionStringAgainstSchemaAsString ∷ String → String → Except (NonEmptyList (Tuple (List String) String)) Unit
validateOperationDefinitionStringAgainstSchemaAsString = validate_x_againstSchemaAsString validateOperationDefinitionAsStringAgainstSchema

validateOperationDefinitionStringAgainstSchemaAsString' ∷ String → String → Effect Unit
validateOperationDefinitionStringAgainstSchemaAsString' a b =
  either
    (throwError <<< error <<< show)
    pure
    (unwrap $ runExceptT (validateOperationDefinitionStringAgainstSchemaAsString a b))
