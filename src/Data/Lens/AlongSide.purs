module Data.Lens.AlongSide where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens as L
import Data.Lens.Swapped (swapped)
import Data.Newtype (class Newtype)
import Data.Newtype as N
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, second)
import Data.Tuple (Tuple(..))
import Data.Lens.Assoc

newtype AlongSide p c d a b = AlongSide (p (Tuple c a) (Tuple d b))

derive instance newtypeAlongSide :: Newtype (AlongSide p c d a b) _
derive instance genericAlongSide :: Generic (AlongSide p c d a b) _

instance showAlongSide :: Show (p (Tuple c a) (Tuple d b)) => Show (AlongSide p c d a b) where
  show a = genericShow a

instance profunctorAlongSide :: Profunctor p => Profunctor (AlongSide p c d) where
  dimap f g = N.over AlongSide (dimap (map f) (map g))

instance strongAlongside :: Strong p => Strong (AlongSide p c d) where
  first p = swapped (second p)
  second = N.over AlongSide (shuffled <<< second)

alongside :: forall p sc sd ta tb a b c d. Profunctor p => L.Optic (AlongSide p sc sd) ta tb a b -> L.Optic (AlongSide p a b) sc sd c d -> L.Optic p (Tuple ta sc) (Tuple tb sd) (Tuple a c) (Tuple b d)
alongside lab lcd = swapped <<< N.under AlongSide lab <<< swapped <<< N.under AlongSide lcd

pairing :: forall p s t a b c d. Profunctor p => (s -> a) -> (b -> t) -> L.Optic p (Tuple c s) (Tuple d t) (Tuple c a) (Tuple d b)
pairing f g = N.under AlongSide (dimap f g)

lensOf :: forall s t a b c d. (s -> a) -> (s -> b -> t) -> L.Lens (Tuple c s) (Tuple d t) (Tuple c a) (Tuple d b)
lensOf f g = N.under AlongSide (L.lens f g)