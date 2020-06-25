module Data.GraphQL.Lens where

import Prelude
import Data.GraphQL.AST as AST
import Data.Lens (class Wander)
import Data.Lens as L
import Data.Lens.Record as LR
import Data.List (List, length, concat)
import Data.Maybe (Maybe)
import Data.Profunctor.Choice (class Choice)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple, uncurry)

down ∷ ∀ a b o. Tuple (a → b) (b → Maybe a) → (Choice o ⇒ o a a → o b b)
down = uncurry L.prism'

lensToTypeSystemDefinitions ∷ ∀ o. Choice o ⇒ Wander o ⇒ o AST.TypeSystemDefinition AST.TypeSystemDefinition -> o AST.Document AST.Document
lensToTypeSystemDefinitions =
  down AST._Document
    <<< L.traversed
    <<< down AST._Definition_TypeSystemDefinition

lensToFragmentDefinitions ∷ ∀ o. Choice o ⇒ Wander o ⇒ o AST.FragmentDefinition AST.FragmentDefinition -> o AST.Document AST.Document
lensToFragmentDefinitions =
  down AST._Document
    <<< L.traversed
    <<< down AST._Definition_ExecutableDefinition
    <<< down AST._ExecutableDefinition_FragmentDefinition

lensToTypeDefinitions ∷ ∀ o. Choice o ⇒ Wander o ⇒ o AST.TypeDefinition AST.TypeDefinition → o AST.Document AST.Document
lensToTypeDefinitions =
  lensToTypeSystemDefinitions
    <<< down AST._TypeSystemDefinition_TypeDefinition

lensToObjectTypeDefinitions ∷ ∀ o. Choice o ⇒ Wander o ⇒ o AST.T_ObjectTypeDefinition AST.T_ObjectTypeDefinition → o AST.Document AST.Document
lensToObjectTypeDefinitions =
  lensToTypeDefinitions
    <<< down AST._TypeDefinition_ObjectTypeDefinition
    <<< down AST._ObjectTypeDefinition

lensToNamedFieldDefinitions ∷ ∀ o. Choice o ⇒ Wander o ⇒ String → o AST.T_FieldDefinition AST.T_FieldDefinition → o AST.Document AST.Document
lensToNamedFieldDefinitions name =
  lensToObjectTypeDefinitions
    <<< L.filtered (eq name <<< _.name)
    <<< LR.prop (SProxy ∷ SProxy "fieldsDefinition")
    <<< L._Just
    <<< down AST._FieldsDefinition
    <<< L.traversed
    <<< down AST._FieldDefinition

lensToSchemaDefinitions ∷ ∀ o. Choice o ⇒ Wander o ⇒ o AST.T_SchemaDefinition AST.T_SchemaDefinition → o AST.Document AST.Document
lensToSchemaDefinitions =
  lensToTypeSystemDefinitions
    <<< down AST._TypeSystemDefinition_SchemaDefinition
    <<< down AST._SchemaDefinition

lensToNamedRootOperationTypeDefinitions ∷ ∀ o. Choice o ⇒ Wander o ⇒ AST.OperationType → o AST.T_RootOperationTypeDefinition AST.T_RootOperationTypeDefinition → o AST.Document AST.Document
lensToNamedRootOperationTypeDefinitions operationType =
  lensToSchemaDefinitions
    <<< LR.prop (SProxy ∷ SProxy "rootOperationTypeDefinition")
    <<< L.traversed
    <<< down AST._RootOperationTypeDefinition
    <<< L.filtered (eq operationType <<< _.operationType)

getOperationTypeDefinitionType ∷ AST.T_OperationTypeDefinition -> String
getOperationTypeDefinitionType = L.view (LR.prop (SProxy ∷ SProxy "namedType") <<< L.iso (\(AST.NamedType s) -> s) \s -> AST.NamedType s)

getAllDefinitions ∷ String → AST.OperationType → AST.Document → List AST.T_FieldDefinition
getAllDefinitions s o d =
  let
    f = L.toListOf (lensToNamedRootOperationTypeDefinitions o) d
  in
    if (length f > 0) then
      concat $ map (flip L.toListOf d <<< lensToNamedFieldDefinitions <<< getOperationTypeDefinitionType) f
    else
      L.toListOf (lensToNamedFieldDefinitions s) d

getAllQueryDefinitions ∷ AST.Document → List AST.T_FieldDefinition
getAllQueryDefinitions = getAllDefinitions "Query" AST.Query

getAllMutationDefinitions ∷ AST.Document → List AST.T_FieldDefinition
getAllMutationDefinitions = getAllDefinitions "Mutation" AST.Mutation

getAllSubscriptionDefinitions ∷ AST.Document → List AST.T_FieldDefinition
getAllSubscriptionDefinitions = getAllDefinitions "Subscription" AST.Subscription
