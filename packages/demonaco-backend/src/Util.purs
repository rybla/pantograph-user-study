module Util where

import Data.Foldable
import Data.Tuple.Nested
import Prelude

import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map, toUnfoldable, fromFoldable, lookup, member, delete, unionWith, insert)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe (maybe)
import Data.Maybe as Maybe
import Data.UUID (UUID)
import Data.UUID as UUID
--import Hole as Hole
import Data.Array as Array
import Effect.Unsafe (unsafePerformEffect)
import Effect.Ref as Ref
import Data.Enum (class Enum, succ)
import Data.Unfoldable (unfoldr)
import Debug (trace)
import Partial.Unsafe (unsafeCrashWith)

--hole' :: forall a. String -> a
---- hole' msg = unsafeThrow $ "hole: " <> msg
--hole' msg = Hole.hole msg

index' :: forall a. Array a -> Int -> a
index' a i = fromJust $ Array.index a i

lookup' :: forall k v. Ord k => k -> Map k v -> v
lookup' x m = case lookup x m of
  Just v -> v
  Nothing -> bug "lookup failed"

head' :: forall a . List a -> a
head' l = case List.head l of
    Nothing -> bug "head failed"
    Just a -> a

fromJust :: forall a . Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = bug "fromJust failed"

fromRight :: forall a b. Either a b -> b
fromRight (Right b) = b
fromRight _ = bug "error: fromRight failed"

justWhen :: forall a. Boolean -> (Unit -> a) -> Maybe a
justWhen false _ = Nothing
justWhen true k = Just (k unit)

delete' :: forall v k . Ord k => k -> Map k v -> Map k v
delete' k m  = if member k m then delete k m else bug "Tried to delete an element not present in the map"
--delete' k m  = delete k m

insert' :: forall v k . Ord k => k -> v -> Map k v -> Map k v
insert' k v m =
    if member k m then bug "Tried to insert an element already present in the map" else
    insert k v m


mapKeys :: forall k v . Ord k => (k -> k) -> Map k v -> Map k v
mapKeys f m =
--    let bla = toUnfoldable in
    let asList :: List (k /\ v)
        asList = toUnfoldable m in
    fromFoldable (map (\(k /\ v) -> f k /\ v) asList)

-- disjoint union
union' :: forall v k. Ord k => Map k v -> Map k v -> Map k v
union' m1 m2 = unionWith (\_ _ -> bug "duplicate key in union'") m1 m2

------ disjoint union, or returns Nothing if the same key leads to two different values
--unionCheckConflicts :: forall v k. Ord k => Eq v => Map k v -> Map k v -> Maybe (Map k v)
--unionCheckConflicts m1 m2 =
--    foldl (\macc (k /\ v) -> do
--        acc <- macc
--        case lookup k acc of
--            Just v' | not (v' == v) -> Nothing
--            _ -> pure (Map.insert k v acc))
--        (Just m1) (toUnfoldable m2 :: List (k /\ v))

--readUUID :: String -> UUID
--readUUID str = fromJust' ("failed to parse UUID: " <> str) <<< UUID.parseUUID $ str

threeCaseUnion :: forall v1 v2 v3 k . Ord k =>
    (v1 -> v3) -> (v2 -> v3) -> (v1 -> v2 -> v3)
    -> Map k v1 -> Map k v2 -> Map k v3
threeCaseUnion onlyLeft onlyRight both m1 m2 =
    let mLeft = Map.filterWithKey (\k _ -> not (member k m2)) m1 in
    let mRight = Map.filterWithKey (\k _ -> not (member k m1)) m2 in
    union'
        (union' (map onlyLeft mLeft) (map onlyRight mRight))
        (Map.mapMaybeWithKey (\k v -> maybe Nothing (\v2 -> Just $ both v v2) (Map.lookup k m2)) m1)

threeCaseUnionMaybe :: forall v1 v2 v3 k . Ord k =>
    (Maybe v1 -> Maybe v2 -> Maybe v3)
    -> Map k v1 -> Map k v2 -> Map k v3
threeCaseUnionMaybe join m1 m2 = Map.mapMaybe (\x -> x) $ threeCaseUnion (\x -> join (Just x) Nothing) (\y -> join Nothing (Just y))
    (\x y -> join (Just x) (Just y)) m1 m2


findWithIndex :: forall t out. (Int -> t -> Maybe out) -> Array t -> Maybe (out /\ Int)
findWithIndex f l =
--    do -- stupid implementation calls f an extra time
--    i <- Array.findIndex (Maybe.isJust <<< (f i)) l
--    res <- Array.findMap f l
--    pure $ res /\ i
    let impl :: Int -> Maybe (out /\ Int)
        impl i = case Array.index l i of
            Nothing -> Nothing
            Just x -> case f i x of
                Nothing -> impl (i + 1)
                Just res -> Just (res /\ i)
    in impl 0

assertSingleton :: forall t. Array t -> t
assertSingleton [x] = x
assertSingleton _ = bug "assertion failed: was not a singleton"

--  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
-- assumes that the thing is nonempty
foldNonempty :: forall a f. Foldable f => (a -> a -> a) -> f a -> a
foldNonempty f l = case foldl (\acc el ->
        case acc of
            Just a -> Just (f a el)
            Nothing -> Just el
        ) Nothing l of
    Nothing -> bug "assumption violated in foldNonempty: was empty"
    Just res -> res

-- represents a hole but for types
data Hole


type Stateful t = {get :: Unit -> t, set :: t -> Unit}
stateful :: forall t. t -> Stateful t
stateful t = unsafePerformEffect do
    tref <- Ref.new t
    pure {
        get: \_ -> unsafePerformEffect (Ref.read tref)
        , set: \tNew -> unsafePerformEffect (Ref.write tNew tref)
    }

inlineMaybeCase :: forall a out. Maybe a -> (a -> out) -> out -> out
inlineMaybeCase cond thenn elsee = case cond of
    Nothing -> elsee
    Just x -> thenn x

allPossible :: forall a. -- https://stackoverflow.com/questions/74462784/purescript-data-as-array-of-all-possible-data-inhabitants
  Enum a =>
  Bounded a =>
  Array a
allPossible = unfoldr (\b -> b >>= next) $ Just bottom
  where
    next a = Just $ Tuple a $ succ a

traceAfter :: forall b. String -> (Unit -> b) -> b
traceAfter a k =
    let res = k unit in
    trace a (\_ -> res)

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ Array.intercalate "\n"
  [ ""
  , "==[ BUG ]================================================================="
  , msg
  , "=========================================================================="
  , ""
  ]
