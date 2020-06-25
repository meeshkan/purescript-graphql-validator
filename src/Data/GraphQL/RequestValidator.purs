module Data.GraphQL.RequestValidator where

import Prelude

import Control.Monad.Except (Except, throwError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (execWriterT)
import Data.GraphQL.AST as AST
import Data.Foldable(foldl)
import Data.GraphQL.Validator.Util (ValStack(..), (<:>), (<+>), oooook, taddle)
import Data.List (List(..))
import Data.List.Lazy.NonEmpty (fromList)
import Data.List.Types (NonEmptyList)
import Data.Maybe (maybe, Maybe(..))
import Data.Lens as L
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)
import Data.GraphQL.Lens (getAllMutationDefinitions, getAllQueryDefinitions, getAllSubscriptionDefinitions, lensToTypeDefinitions)


{-
OperationDefinition_SelectionSet
{
  foo
  bar
}

OperationDefinition_OperationType
query (id:"a") {
  foo
  bar
}

data OperationDefinition
  = OperationDefinition_SelectionSet SelectionSet
  | OperationDefinition_OperationType T_OperationDefinition_OperationType
-}
-- loop 1: loop over selection sets
-- loop 2 (inner loop): loop over field definitions
-- n^2

{-
Selection
    -> { argumentsDefinition :: Maybe ArgumentsDefinition
       , description :: Maybe String
       , directives :: Maybe Directives
       , name :: String
       , type :: Type
       }
       -> StateT (List String)
            (WriterT (List (Tuple (List String) String))
               (ReaderT
                  { typeDefinitions :: List TypeDefinition
                  }
                  Identity
               )
            )
            Unit-}

{-
data Selection
  = Selection_Field Field
  | Selection_FragmentSpread FragmentSpread
  | Selection_InlineFragment InlineFragment
-}

validateSelectionAgainstFieldDefinition ∷ AST.Selection → AST.T_FieldDefinition → ValStack
validateSelectionAgainstFieldDefinition (Selection_Field f) fd - ?hole
validateSelectionAgainstFieldDefinition (Selection_FragmentSpread fs) fd - ?hole
validateSelectionAgainstFieldDefinition (Selection_InlineFragment ilf) fd - ?hole
{-
validateSelectionAgainstFieldDefinition s fd = ?hole
  if ( /= fd.name) then
    taddle $ "Incorrect field name: expecting " <> fd.name <> " but received " <> k
  else
    ( do
        dive fd.name
        validateValueAgainstType v fd.type
    )-}
    
validateSelectionSetAgainstFieldDefinitions ∷ AST.SelectionSet → List AST.T_FieldDefinition → ValStack
validateSelectionSetAgainstFieldDefinitions (AST.SelectionSet ss) fd =
  foldl
    (<+>)
    oooook
    ( map
        ( \kv →
            foldl
              (<:>)
              (taddle $ "Could not find a match for kv pair: " <> show kv <> "\n")
              ( map (validateSelectionAgainstFieldDefinition kv) fd
              )
        )
        ss
    )
{-
mutation foobar {} -- actually a query

  = { operationType ∷ OperationType, name ∷ (Maybe String), variableDefinitions ∷ (Maybe VariableDefinitions), directives ∷ (Maybe Directives), selectionSet ∷ SelectionSet }

data OperationDefinition = OperationDefinition_SelectionSet x  |  OperationDefinition_SelectionSet x y z

data FooBar = Foo String
  | Bar Int String

doSomethingOnFoobar :: FooBar -> Boolean
doSomethingOnFoobar (Foo s) = doSomethingOnFoobar (Bar 42 s)
doSomethingOnFoobar (Bar i s) = s == "hello"

data Maybe a = Nothing | Just a

isJust :: forall a. Maybe a -> Boolean
isJust (Just _) = true
isJust Nothing = false

doSomethingOnFoobar (Foo "hello")
-- true
-}

validateExecutableDefinitionAgainstSchema ∷ AST.OperationDefinition → AST.Document → Except (NonEmptyList (Tuple (List String) String)) Unit
validateExecutableDefinitionAgainstSchema (AST.OperationDefinition_SelectionSet ss) doc = validateExecutableDefinitionAgainstSchema (AST.OperationDefinition_OperationType { operationType: AST.Query, name: Nothing, variableDefinitions: Nothing, directives: Nothing, selectionSet: ss })
validateExecutableDefinitionAgainstSchema (AST.OperationDefinition_OperationType ot) doc =
  maybe (pure unit) throwError
    $ fromList
        ( unwrap
            ( runReaderT
                ( execWriterT
                    ( evalStateT
                        ( validateSelectionSetAgainstFieldDefinitions ot.selectionSet (case ot.operationType of
                          AST.Query → getAllQueryDefinitions doc
                          AST.Mutation → getAllMutationDefinitions doc
                          AST.Subscription → getAllSubscriptionDefinitions doc
                          )
                        )
                        Nil
                    )
                )
                { typeDefinitions: (L.toListOf lensToTypeDefinitions doc) }
            )
        )
    