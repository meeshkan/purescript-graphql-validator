module Data.GraphQL.Validator.Util where

import Prelude
import Control.Monad.Reader (class MonadAsk, ReaderT, ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState, StateT, get, put)
import Data.GraphQL.AST as AST
import Data.Identity (Identity)
import Data.List (List(..), (:), singleton, length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type GraphQLReqEnv
  = { fragmentDefinitions ∷ List AST.FragmentDefinition, typeDefinitions ∷ List AST.TypeDefinition }

type GraphQLResEnv
  = GraphQLReqEnv

type ValStack'' r a
  = StateT
      -- where we are in the path
      (List String)
      ( ReaderT
          -- info about the document
          r
          Identity
      )
      a

type ValStack' r
  = ValStack'' r (List (Tuple (List String) String))

type ValStackRes
  = ValStack' GraphQLResEnv

type ValStackReq
  = ValStack' GraphQLReqEnv

oooook ∷ ∀ a m. Monad m ⇒ m (List a)
oooook = pure Nil

taddle ∷ ∀ a m. MonadState a m ⇒ String → m (List (Tuple a String))
taddle msg = do
  whereWeAre ← get
  pure $ singleton (Tuple whereWeAre msg)

dive ∷ ∀ m. MonadState (List String) m ⇒ String → m Unit
dive segment = do
  whereWeAre ← get
  put $ whereWeAre <> (singleton segment)

--     _a = unwrap (runReaderT (execWriterT (evalStateT a whereWeAre)) env)
-- if both go to shit, we want to have the errors for both
altalt ∷
  ∀ a m.
  Monad m ⇒
  m (List (Tuple a String)) →
  m (List (Tuple a String)) →
  m (List (Tuple a String))
altalt a b = do
  _a ← a
  if length _a == 0 then
    pure Nil
  else
    ( do
        _b ← b
        if length _b == 0 then pure Nil else pure $ _a <> _b
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
  String →
  String →
  m (List (Tuple a String))
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
  String →
  String →
  m (List (Tuple a String))
validateAsScalar js nt = do
  v ← ask
  if (isScalar js v.typeDefinitions) then
    oooook
  else
    taddle "Value is not a valid scalar type in the document"

type ValStep y a m
  = m (Step { acc ∷ List (Tuple a String), l ∷ List y } (List (Tuple a String)))

validationInnerLoop ∷
  ∀ y a m.
  MonadState a m ⇒
  MonadRec m ⇒
  List (Tuple a String) →
  (y → m (List (Tuple a String))) →
  List y →
  m (List (Tuple a String))
validationInnerLoop i f l1 = tailRecM go { acc: i, l: l1 }
  where
  go ∷ { acc ∷ List (Tuple a String), l ∷ List y } → ValStep y a m
  go { acc, l: Nil } = pure $ Done acc

  go { acc, l: (x : xs) } = do
    r ← f x
    proc r acc xs

  proc ∷
    List (Tuple a String) →
    List (Tuple a String) →
    List y →
    ValStep y a m
  proc Nil _ _ = pure $ Done Nil

  proc _ Nil _ = pure $ Done Nil

  proc a b l = pure $ Loop { acc: a <> b, l }

validationDoubleLoop ∷
  ∀ x y a m.
  Show x ⇒
  MonadState a m ⇒
  MonadRec m ⇒
  (x → y → m (List (Tuple a String))) →
  List x →
  List y →
  m (List (Tuple a String))
validationDoubleLoop f l0 l1 = tailRecM go { acc: Nil, l: l0 }
  where
  go ∷ { acc ∷ List (Tuple a String), l ∷ List x } → ValStep x a m
  go { acc, l: Nil } = pure $ Done acc

  go { acc, l: (x : xs) } = do
    whereWeAre ← get
    vl ←
      validationInnerLoop
        (singleton (Tuple whereWeAre $ "Could not find a match for kv pair: " <> show x <> "\n"))
        (f x)
        l1
    pure $ Loop { acc: vl <> acc, l: xs }

validationOuterLoop ∷
  ∀ x a m.
  Show x ⇒
  MonadState a m ⇒
  MonadRec m ⇒
  (x → m (List (Tuple a String))) →
  List x →
  m (List (Tuple a String))
validationOuterLoop f l0 = tailRecM go { acc: Nil, l: l0 }
  where
  go ∷ { acc ∷ List (Tuple a String), l ∷ List x } → ValStep x a m
  go { acc, l: Nil } = pure $ Done acc

  go { acc, l: (x : xs) } = do
    vl ← f x
    pure $ Loop { acc: vl <> acc, l: xs }

topLevelError =
  Tuple
    (singleton "[Root level]")
    "A graphql response must have a field called 'data' at the top level" ∷
    Tuple (List String) String
