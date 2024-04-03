module Unification where

import Prelude
import Data.Expr as Expr
import Data.Maybe -- (Maybe(..))
import Control.Monad.State (State)
import Control.Monad.State as State
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Expr (subMetaExprPartially, (%))
import Data.List (List(..), (:))
import Data.List as List
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Array as Array
import Data.Tuple (fst, snd)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Control.Monad.Error.Class (throwError)
import Util

{-
I copied over this from pantograph.
-}

type Sub l = Expr.MetaVarSub (Expr.MetaExpr l)

flattenHelperInsertVar :: forall l. Expr.IsExprLabel l => Sub l -> Expr.MetaVar -> State (Sub l) (Expr.MetaExpr l)
flattenHelperInsertVar original x = do
    sub <- State.get
    case Map.lookup x sub of
        Just t -> pure t -- If var is already in the new sub, do nothing
        Nothing -> do -- Otherwise, if its in the original sub at all, then add it the output sub
            value <- case Map.lookup x original of
                    Nothing -> pure $ Expr.MV x % []
                    Just t -> do
                        value <- flattenHelper original t
                        _ <- State.modify (Map.insert x value)
                        pure value
            pure value

flattenHelper :: forall l. Expr.IsExprLabel l => Sub l -> Expr.MetaExpr l -> State (Sub l) (Expr.MetaExpr l)
flattenHelper original {-x-} (Expr.MV x % _) = flattenHelperInsertVar original x
flattenHelper original {-x-} (l % kids) = do
    kids' <- sequence $ map (flattenHelper original) kids
    pure $ l % kids'

-- The input is a non-idempotent substitution, and the output in the State is an idempotent subsitution
flattenSubImpl :: forall l. Expr.IsExprLabel l => Sub l -> State (Sub l) Unit
flattenSubImpl original =  do
--    traceM ("in flattenSubImpl, original is: " <> pretty original)
    _ <- sequence (map (\(key /\ _) -> flattenHelperInsertVar original key) (Map.toUnfoldable original :: List _))
--    result <- State.get
--    traceM ("in flattenSubImpl, result is: " <> pretty result)
    pure unit

-- makes a substitution idempotent.
flattenSub :: forall l. Expr.IsExprLabel l => Sub l -> Sub l
flattenSub sub = snd $ State.runState (flattenSubImpl sub) Map.empty

-- occurs check with a non-idempotent sub as an enviroment
recursiveOccurs :: forall l. Expr.IsExprLabel l => Sub l -> Expr.MetaVar -> Expr.MetaExpr l -> Boolean
recursiveOccurs sub x e =
    case e of
        Expr.MV y % [] | Just e' <- Map.lookup y sub -> recursiveOccurs sub x e'
        Expr.MV x' % [] -> x' == x
        _ % kids -> Array.any (recursiveOccurs sub x) kids

-- NOTE: it may be confusing that the State in unifyFImpl is a completely different thing to the State in flatten*
-- The (Sub l) in the State is the non-idempotent substitution being built up
unifyFImpl :: forall l. Expr.IsExprLabel l => Expr.MetaExpr l -> Expr.MetaExpr l -> ExceptT Unit (State (Sub l)) (Expr.MetaExpr l)
unifyFImpl e1@(Expr.Expr l1 kids1) e2@(Expr.Expr l2 kids2) = do
    sub <- State.get
    case l1 /\ l2 of
        Expr.MV x /\ _ | Just e1' <- Map.lookup x sub -> unifyFImpl e1' e2
        _ /\ Expr.MV x | Just e2' <- Map.lookup x sub -> unifyFImpl e1 e2'
        Expr.MV x1 /\ Expr.MV x2 | x1 == x2 -> pure e1
        Expr.MV x /\ _ | not (recursiveOccurs sub x e2) -> do
            _ <- lift $ State.modify (Map.insert x e2)
            pure e2
        _ /\ Expr.MV _ -> unifyFImpl e2 e1
        Expr.MInj l /\ Expr.MInj l' | l == l' -> do
            kids' <- sequence $ Array.zipWith unifyFImpl kids1 kids2
            pure ((Expr.MInj l) % kids')
        _ /\ _ -> throwError unit

--unifyF :: forall l. Expr.IsExprLabel l => Expr.MetaExpr l -> Expr.MetaExpr l -> Maybe (Expr.MetaExpr l /\ Sub l)
--unifyF e1 e2 =
--    let maybeExpr /\ sub = State.runState (runExceptT (unifyFImpl e1 e2)) Map.empty in
--    case maybeExpr of
--        Left _ -> Nothing
--        Right expr ->
--            let flatSub = flattenSub sub in
--            Just (Expr.subMetaExprPartially flatSub expr /\ flatSub)

--runUnifyMonad :: forall l b. Expr.IsExprLabel l => ExceptT Unit (State (Sub l)) b -> Maybe (Sub l /\ b)
--runUnifyMonad m =
--    let maybeExpr /\ sub = State.runState (runExceptT m) Map.empty in
--    case maybeExpr of
--        Left _ -> Nothing
--        Right x -> Just (flattenSub sub /\ x)

unify :: forall l. Expr.IsExprLabel l => Stateful (Sub l) -> Expr.MetaExpr l -> Expr.MetaExpr l -> Either Unit (Expr.MetaExpr l)
unify sub e1@(Expr.Expr l1 kids1) e2@(Expr.Expr l2 kids2) = do
    case l1 /\ l2 of
        Expr.MV x /\ _ | Just e1' <- Map.lookup x (sub.get unit) -> unify sub e1' e2
        _ /\ Expr.MV x | Just e2' <- Map.lookup x (sub.get unit) -> unify sub e1 e2'
        Expr.MV x1 /\ Expr.MV x2 | x1 == x2 -> pure e1
        Expr.MV x /\ _ | not (recursiveOccurs (sub.get unit) x e2) ->
            let _ =  sub.set (Map.insert x e2 (sub.get unit)) in
            pure e2
        _ /\ Expr.MV _ -> unify sub e2 e1
        Expr.MInj l /\ Expr.MInj l' | l == l' -> do
            kids' <- sequence $ Array.zipWith (unify sub) kids1 kids2
            pure ((Expr.MInj l) % kids')
        _ /\ _ -> throwError unit
