module MaybeJs where

import Prelude
import Data.Either (Either(..))

foreign import data Maybe :: Type -> Type

foreign import just :: forall a. a -> Maybe a

foreign import nothing :: forall a. Maybe a

foreign import maybe :: forall a b. b -> (a -> b) -> Maybe a -> b

foreign import realCatchException :: forall a. (forall x y. x -> Either x y) -> (forall x y. x -> Either y x)
    -> (Unit -> a) -> Either String a

instance _Functor_Maybe :: Functor Maybe where
  map f = maybe nothing (f >>> just)

instance _Apply_Maybe :: Apply Maybe where
  apply m_f = maybe nothing (maybe (const nothing) (\f -> f >>> just) m_f)

instance _Applicative_Maybe :: Applicative Maybe where
  pure = just

instance _Bind_Applicative :: Bind Maybe where
  bind m_a k = maybe nothing k m_a

instance _Monad_Applicative :: Monad Maybe
