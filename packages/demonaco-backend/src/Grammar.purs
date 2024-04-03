module Grammar where

import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either as Either
import Data.Either (Either(..))
import Data.Enum (class Enum)
import Data.Expr (class IsExprLabel, MetaVar(..), freshMetaVar, (%), (%*))
import Data.Expr as Expr
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (class Foldable, class Traversable)
import Data.Traversable (sequence)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (trace, traceM)
import Unification (Sub, unifyFImpl, flattenSub)
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Pretty, pretty)
import Util as Util
import Data.Foldable as Foldable
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.State (State)
import Control.Monad.State as State

--------------------------------------------------------------------------------
-- RuleLabel
--------------------------------------------------------------------------------

--getSortFromSub :: forall l r. IsRuleLabel l r => r -> Expr.MetaVarSub (Sort l) -> Sort l
--getSortFromSub r sub =
--    let (Rule _vars _kidSorts parentSort) = TotalMap.lookup r language in
--    Expr.subMetaExprPartially sub parentSort


--------------------------------------------------------------------------------
-- Sorts
--------------------------------------------------------------------------------

type Sort l = Expr.MetaExpr l

freshMetaVarSort :: forall l. String -> Sort l
freshMetaVarSort name = (Expr.MV (freshMetaVar name)) % []

--------------------------------------------------------------------------------
-- Rule
--------------------------------------------------------------------------------

-- | A `Rule` has the form
-- | ```
-- |   [∀ «MetaVar»]*
-- |   [«Sort»] -- kids
-- |   --------------------
-- |   «Sort» -- parent
-- | ```
data Rule l =
  Rule
--    (Set.Set Expr.MetaVar)
    (Array (Sort l))
    (Sort l)

makeRule' :: forall l.
  Array String -> -- metavariables
  (Array (Sort l) -> Array (Sort l) /\ Sort l) ->
  Rule l
makeRule' = \strs f -> do
  let mxs = Expr.freshMetaVar <$> strs
  let es = Expr.fromMetaVar <$> mxs
  let hyps /\ con = f es
  Rule hyps con

makeRule :: forall l.
  Array String -> -- Regular metavariables
  (Partial => Array (Sort l) -> Array (Sort l) /\ Sort l) ->
  Rule l
makeRule = \strs f -> makeRule' strs (unsafePartial f)

--------------------------------------------------------------------------------
------- Faster versions of infer* ----------------------------------------------
--------------------------------------------------------------------------------

type SortSub l = Sub l

{-
inferFImpl :: forall l r. Expr.IsExprLabel l => IsRuleLabel l r =>
    DerivTerm l r -> ExceptT Unit (State (SortSub l)) Unit
inferFImpl (l % kids) = do
    _ <- sequence (map inferFImpl kids)
    _ <- sequence $ Array.zipWith (\parentBottom kidTop -> unifyFImpl parentBottom (derivTermSort kidTop)) (kidSorts l) kids
    pure unit


inferF :: forall l r. IsRuleLabel l r => DerivTerm l r -> Maybe (SortSub l)
inferF term =
    fst <$> runUnifyMonad do
        _ <- inferFImpl term
        pure unit
-}
