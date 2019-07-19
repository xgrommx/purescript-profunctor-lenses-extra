module Data.Lens.Each where

import Prelude

import Data.Lens as L
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each ∷ L.Traversal s t a b

instance eachArray ∷ Each (Array a) (Array b) a b where
  each = L.traversed

else instance eachMaybe ∷ Each (Maybe a) (Maybe b) a b where
  each = L._Just

else instance eachTuple :: Each (Tuple a a) (Tuple b b) a b where
  each = L.wander (\f (Tuple a b) -> Tuple <$> f a <*> f b)