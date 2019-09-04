module Data.Lens.Argonaut where

import Prelude

import Data.Argonaut (Json, _Array, _Null, _Object, jsonParser, stringify)
import Data.Either (Either(..))
import Data.Lens (IndexedTraversal', Prism', Traversal', isn't, prism, traversed) as L
import Data.Lens.Index (ix) as L
import Data.Lens.Indexed (itraversed) as L

_Json ∷ L.Prism' String Json
_Json = L.prism stringify jsonParser

key ∷ String -> L.Traversal' Json Json
key i = _Object <<< L.ix i

nth ∷ Int -> L.Traversal' Json Json
nth i = _Array <<< L.ix i

values ∷ L.Traversal' Json Json
values = _Array <<< L.traversed

members ∷ L.IndexedTraversal' String Json Json
members = _Object <<< L.itraversed

nonNull ∷ L.Traversal' Json Json
nonNull = L.prism identity (\v -> if L.isn't _Null v then Right v else Left v)