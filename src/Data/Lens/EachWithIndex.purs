module Data.Lens.EachWithIndex where

import Data.Lens (IndexedTraversal) as L
import Data.Lens.Each (class Each, each)
import Data.Lens.Indexed (itraversed, positions) as L
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

class Each s t a b <= EachWithIndex i s t a b | s -> i, t -> i, s -> a, t -> b, s b -> t, t a -> s where
  ieach ∷ L.IndexedTraversal i s t a b

instance eachWithIndexArray ∷ EachWithIndex Int (Array a) (Array b) a b where
  ieach = L.itraversed

else instance eachWithIndexMaybe ∷ EachWithIndex Int (Maybe a) (Maybe b) a b where
  ieach = L.positions each

else instance eachWithIndexTuple ∷ EachWithIndex Int (Tuple a a) (Tuple b b) a b where
  ieach = L.positions each