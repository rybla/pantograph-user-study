module Text.Pretty where

import Prelude

import Data.Array (concat, foldMap, intercalate)
import Data.Functor.Compose (Compose(..))
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe, maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))

class Pretty a where pretty :: a -> String

instance Pretty String where pretty = identity
instance Pretty Int where pretty = show
instance Pretty Boolean where pretty = show
instance Pretty a => Pretty (List.List a) where pretty xs = "[" <> List.intercalate ", " (pretty <$> xs) <> "]"
instance Pretty a => Pretty (Array a) where pretty xs = "[" <> intercalate ", " (pretty <$> xs) <> "]"
instance Pretty a => Pretty (Maybe a) where pretty = maybe "NOTHING" pretty
instance (Pretty a, Pretty b) => Pretty (Tuple a b) where pretty (Tuple a b) = pretty a <> ", " <> pretty b
--instance (Pretty k, Pretty v) => Pretty (Map.Map k v) where
--  pretty m =
--    "map:" <>
--    indent (bullets (Map.toUnfoldable m <#> \(Tuple k v) -> pretty k <> " ↦ " <> pretty v))
--instance (Pretty t) => Pretty (Set.Set t) where
--  pretty s = "{" <> List.intercalate ", " (Set.map pretty s) <> "}"
instance (Pretty a, Pretty b) => Pretty (Either a b) where
    pretty (Left a) = "Left " <> pretty a
    pretty (Right a) = "Right " <> pretty a
