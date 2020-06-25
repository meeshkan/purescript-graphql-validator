module Data.GraphQL.Validator.Util where


import Prelude
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.GraphQL.AST (Document)
import Data.GraphQL.AST as AST
import Data.Tuple(Tuple(..))
import Data.GraphQL.Lens (getAllMutationDefinitions, getAllQueryDefinitions, lensToTypeDefinitions)
import Data.GraphQL.Parser (document)
import Data.Identity (Identity)
import Data.Lens as L
import Data.List (List(..), singleton, (:), fromFoldable, length)
import Data.List.NonEmpty (NonEmptyList, fromList)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, null)
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

not'oooook ∷ List (Tuple (List String) String) → ValStack
not'oooook l = do
  tell l
  lift $ pure unit

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
altalt ∷ ValStack → ValStack → ValStack
altalt a b = do
  whereWeAre ← get
  env ← ask
  let
    _a = unwrap (runReaderT (execWriterT (evalStateT a whereWeAre)) env)
  if length _a == 0 then
    oooook
  else
    ( let
        _b = unwrap (runReaderT (execWriterT (evalStateT b whereWeAre)) env)
      in
        if length _b == 0 then oooook else not'oooook $ _a <> _b
    )

infixl 3 altalt as <:>

plusplus ∷ ValStack → ValStack → ValStack
plusplus a b = do
  whereWeAre ← get
  env ← ask
  let
    _a = unwrap (runReaderT (execWriterT (evalStateT a whereWeAre)) env)
  let
    _b = unwrap (runReaderT (execWriterT (evalStateT b whereWeAre)) env)
  if length _b == 0 && length _a == 0 then oooook else not'oooook $ _a <> _b

infixl 3 plusplus as <+>