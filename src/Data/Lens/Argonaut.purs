module Data.Lens.Argonaut where

import Prelude

import Data.Argonaut (Json, _Array, _Null, _Number, _Object, _String, jsonParser, stringify)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Functor.Contravariant (coerce)
import Data.Lens (Getter, Getter', IndexedTraversal', Prism', Traversal', Fold', _Right, collectOf, isn't, non, over, preview, prism, set, to, traversed, (%~), (.~), (^.), (^?)) as L
import Data.Lens.Index (ix) as L
import Data.Lens.Indexed (itraversed) as L
import Data.Lens.Record as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Maybe.First (First(..))
import Data.Newtype (un)
import Data.Profunctor (dimap)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)

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

infixr 4 L.collectOf as %%~
infixr 4 setJust as ?~

setJust l b = L.set l (Just b)

pre :: forall s a. L.Fold' (First a) s a -> L.Getter' s (Maybe a)
pre = L.to <<< L.preview

foo :: String
foo = """
  {"foo": {"bar": 10}}
"""

res1 :: Maybe String
res1 = foo L.^? _Json <<< key "foo" <<< key "bar" <<< _String

res2 :: String
res2 = foo L.^. pre (_Json <<< key "foo" <<< key "bar" <<< _String) <<< L.non "def"

res3 :: String
res3 = foo # _Json <<< key "foo" <<< key "bar" <<< _Number L..~ 100.0

r :: { a :: { b :: Either Int Int} }
r = { a: { b: Left 10 } }

-- res2 = r L.^. L.to (L.preview (L.prop (SProxy :: _ "a") <<< L.prop (SProxy :: _ "b") <<< L._Right)) <<< L.non 100

-- data TambaraMod (ten :: * -> * -> *) p a b = TambaraMod 
--   { runTambaraMod :: (forall c. p (a `ten` c) (b `ten` c),
--                       forall d. p (d `ten` a) (d `ten` b))
--   }

data Tambara t p a b = Tambara (forall c d. Tuple (p (t a c) (t b c)) (p (t d a) (t d b)))

-- applyN = fix (\r n f x -> if n == 1 then f x else f (r (n - 1) f x))
-- applyN = under Endo <<< (fold <<< _) <<< replicate
-- applyN n fn = unwrap <<< foldMap Endo $ replicate fn n