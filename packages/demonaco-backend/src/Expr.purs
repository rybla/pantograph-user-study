module Data.Expr where

import Data.Either
import Data.Either.Nested
import Prelude

import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.Eq (class Eq1, eq1)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, intercalate, sequence_)
import Data.Map as Map
import Data.Foldable as Foldable
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List (List(..), Pattern(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype as Newtype
import Data.Ord (class Ord1, compare1)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.Traversable (class Traversable, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (trace, traceM)
import Debug as Debug
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafePartial)
import Prim.Row (class Cons)
import Text.Pretty (class Pretty, pretty)
import Text.Pretty as P
import Text.Pretty as Pretty
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Util as Util

--------------------------------------------------------------------------------
-- Expr
--------------------------------------------------------------------------------

data Expr l = Expr l (Array (Expr l))
type ExprF l kid = l /\ Array kid

infixl 7 Expr as %

toExprF :: forall l. Expr l -> ExprF l (Expr l)
toExprF (Expr l es) = l /\ es

fromExprF :: forall l. ExprF l (Expr l) -> Expr l
fromExprF (l /\ es) = Expr l es

derive instance Generic (Expr l) _
instance Show l => Show (Expr l) where show x = genericShow x
instance Eq l => Eq (Expr l) where eq x y = genericEq x y
instance Ord l => Ord (Expr l) where compare x y = genericCompare x y
derive instance Functor Expr
derive instance Foldable Expr
derive instance Traversable Expr

--------------------------------------------------------------------------------
-- IsExprLabel
--------------------------------------------------------------------------------

class (Eq l, Ord l, Show l) <= IsExprLabel l -- where
  -- !TODO rename to prettyExprF_unsafe
--  prettyExprF'_unsafe :: ExprF l String -> String
--  expectedKidsCount :: l -> Int

--prettyExprF :: forall l. IsExprLabel l => ExprF l String -> String
--prettyExprF (l /\ es) = prettyExprF'_unsafe (l /\ (pretty <$> es))

--instance IsExprLabel l => Pretty (Expr l) where
--  pretty (Expr l es) = prettyExprF (l /\ (pretty <$> es))

--------------------------------------------------------------------------------
-- MetaVar
--------------------------------------------------------------------------------

data MetaVar 
  = MetaVar (Maybe String) Int
  | RuleMetaVar String

metaVarName :: MetaVar -> String
metaVarName (RuleMetaVar s) = s
metaVarName (MetaVar (Just s) _) = s
metaVarName _ = "no_name"

derive instance Generic MetaVar _
instance Show MetaVar where show x = genericShow x
instance Eq MetaVar where eq x y = genericEq x y
instance Ord MetaVar where compare x y = genericCompare x y
instance Pretty MetaVar where
  pretty (MetaVar Nothing uuid) = "?" <> show uuid
  pretty (MetaVar (Just str) uuid) = "?" <> str <> "#" <> show uuid
  pretty (RuleMetaVar str) = "??" <> str

varMaker :: Util.Stateful Int
varMaker = Util.stateful 0

nextInt :: Unit -> Int
nextInt _ =
    let val = varMaker.get unit in
    let _ = varMaker.set (val + 1) in
    val

freshMetaVar :: String -> MetaVar
freshMetaVar str = MetaVar (Just str) (nextInt unit)

freshMetaVar' :: Unit -> MetaVar 
freshMetaVar' _ = MetaVar Nothing (nextInt unit)

freshenMetaVar :: MetaVar -> MetaVar
freshenMetaVar (MetaVar (Just str) _) = freshMetaVar str
freshenMetaVar (MetaVar Nothing _) = freshMetaVar' unit
freshenMetaVar (RuleMetaVar _str) = Util.bug "[freshenMetaVar] Should never try to freshen a RuleMetaVar, since it should be substituted away in any instantiated Rule i.e. Derivation"
data Meta a = MV MetaVar | MInj a

derive instance Generic (Meta a) _
derive instance Functor Meta
--derive instance Applicative Meta
instance Show a => Show (Meta a) where show x = genericShow x
instance Eq a => Eq (Meta a) where eq x y = genericEq x y
instance Ord a => Ord (Meta a) where compare x y = genericCompare x y

instance Pretty a => Pretty (Meta a) where
    pretty =
        case _ of
        MV mv -> pretty mv
        MInj a -> pretty a

instance IsExprLabel l => IsExprLabel (Meta l) -- where
--  prettyExprF'_unsafe (MV x /\ _kids) = pretty x
--  prettyExprF'_unsafe (MInj l /\ kids) = prettyExprF (l /\ kids)

--  expectedKidsCount (MV _) = 0
--  expectedKidsCount (MInj l) = expectedKidsCount l

--------------------------------------------------------------------------------
-- MetaExpr
--------------------------------------------------------------------------------

type MetaExpr l = Expr (Meta l)

fromMetaVar :: forall l. MetaVar -> MetaExpr l
fromMetaVar mx = MV mx % []

pureMetaExpr :: forall l. l -> Array (MetaExpr l) -> MetaExpr l
pureMetaExpr l = (MInj l % _)

infixl 7 pureMetaExpr as %*
--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

type MetaVarSub a = Map.Map MetaVar a

--subMetaExpr :: forall l. IsExprLabel l => MetaVarSub (Expr l) -> MetaExpr l -> Expr l
--subMetaExpr sigma = go
--  where
--  go :: _
--  go = case _ of
--    MV mx % [] -> identity
--    MInj l % kids -> l % (go <$> kids)

subMetaExprPartially :: forall l. IsExprLabel l => MetaVarSub (MetaExpr l) -> MetaExpr l -> MetaExpr l
subMetaExprPartially sigma = case _ of
    MV mx % _ -> case Map.lookup mx sigma of
      Nothing -> (MV mx) % []
      Just mexpr -> mexpr
    MInj l % kids -> (MInj l) % (subMetaExprPartially sigma <$> kids)
