module Data.GraphQL.Validator.Util where

import Prelude
import Data.Foldable (foldl)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask)
import Control.Monad.State (class MonadState, StateT, get, put)
import Control.Monad.Writer (class MonadTell, WriterT, tell)
import Data.GraphQL.AST as AST
import Data.Identity (Identity)
import Data.List (List(..), (:), length, singleton)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type GraphQLReqEnv
  = { fragmentDefinitions ∷ List AST.FragmentDefinition, typeDefinitions ∷ List AST.TypeDefinition }

type GraphQLResEnv
  = GraphQLReqEnv

type ValStack' r
  = StateT
      -- where we are in the path
      (List String)
      ( WriterT
          -- errors
          (List (Tuple (List String) String))
          ( ReaderT
              -- info about the document
              r
              Identity
          )
      )
      Unit

type ValStackRes
  = ValStack' GraphQLResEnv

type ValStackReq
  = ValStack' GraphQLReqEnv

oooook ∷ ∀ m. Monad m ⇒ m Unit
oooook = pure unit

not'oooook ∷ ∀ a m. MonadTell (List (Tuple a String)) m ⇒ List (Tuple a String) → m Unit
not'oooook l = do
  tell l
  oooook

taddle ∷ ∀ a m. MonadTell (List (Tuple a String)) m ⇒ MonadState a m ⇒ String → m Unit
taddle msg = do
  whereWeAre ← get
  tell $ singleton (Tuple whereWeAre msg)
  oooook

dive ∷ ∀ m. MonadState (List String) m ⇒ String → m Unit
dive segment = do
  whereWeAre ← get
  put $ whereWeAre <> (singleton segment)
  oooook

--     _a = unwrap (runReaderT (execWriterT (evalStateT a whereWeAre)) env)
-- if both go to shit, we want to have the errors for both
altalt' ∷
  ∀ a r m.
  MonadTell (List (Tuple a String)) m ⇒
  MonadState a m ⇒
  MonadAsk r m ⇒
  (m Unit → r → a → (List (Tuple a String))) →
  m Unit →
  m Unit →
  m Unit
altalt' uw a b = do
  whereWeAre ← get
  env ← ask
  let
    _a = uw a env whereWeAre
  if length _a == 0 then
    oooook
  else
    ( let
        _b = uw b env whereWeAre
      in
        if length _b == 0 then oooook else not'oooook $ _a <> _b
    )

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

validateAsEnum ∷
  ∀ a r m.
  MonadAsk { typeDefinitions ∷ List AST.TypeDefinition | r } m ⇒
  MonadState a m ⇒
  MonadTell (List (Tuple a String)) m ⇒
  String →
  String →
  m Unit
validateAsEnum js nt = do
  v ← ask
  if isEnum js v.typeDefinitions then
    oooook
  else
    taddle $ "Value is not a valid enum type in the document"

isScalar ∷ String → List AST.TypeDefinition → Boolean
isScalar s Nil = false

isScalar s ((AST.TypeDefinition_ScalarTypeDefinition x) : xs) = true

isScalar s (_ : xs) = isScalar s xs

validateAsScalar ∷
  ∀ a r m.
  MonadAsk { typeDefinitions ∷ List AST.TypeDefinition | r } m ⇒
  MonadState a m ⇒
  MonadTell (List (Tuple a String)) m ⇒
  String →
  String →
  m Unit
validateAsScalar js nt = do
  v ← ask
  if (isScalar js v.typeDefinitions) then
    oooook
  else
    taddle "Value is not a valid scalar type in the document"

plusplus' ∷
  ∀ a r m.
  MonadTell (List (Tuple a String)) m ⇒
  MonadState a m ⇒
  MonadAsk r m ⇒
  (m Unit → r → a → (List (Tuple a String))) →
  m Unit →
  m Unit →
  m Unit
plusplus' uw a b = do
  whereWeAre ← get
  env ← ask
  let
    _a = uw a env whereWeAre
  let
    _b = uw b env whereWeAre
  if length _b == 0 && length _a == 0 then oooook else not'oooook $ _a <> _b

validationDoubleLoop ∷
  ∀ x y a r m.
  Show x ⇒
  MonadTell (List (Tuple a String)) m ⇒
  MonadState a m ⇒
  MonadAsk r m ⇒
  (m Unit → m Unit → m Unit) →
  (m Unit → m Unit → m Unit) →
  (x → y → m Unit) →
  List x → List y → m Unit
validationDoubleLoop pp aa f l0 l1 =
  foldl
    pp
    oooook
    ( map
        ( \kv →
            foldl
              aa
              (taddle $ "Could not find a match for kv pair: " <> show kv <> "\n")
              ( map (f kv) l1
              )
        )
        l0
    )

topLevelError =
  Tuple
    (singleton "[Root level]")
    "A graphql response must have a field called 'data' at the top level" ∷
    Tuple (List String) String
