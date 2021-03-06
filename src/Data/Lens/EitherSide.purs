module Data.Lens.EitherSide where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Lens as L
import Data.Lens.Swapped (swapped)
import Data.Newtype (class Newtype)
import Data.Newtype as N
import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Choice (class Choice, right, (|||))
import Data.Lens.Assoc

newtype EitherSide p c d a b = EitherSide (p (Either c a) (Either d b))

derive instance newtypeEitherSide :: Newtype (EitherSide p c d a b) _
derive instance genericEitherSide :: Generic (EitherSide p c d a b) _

instance profunctorEitherSide :: Profunctor p => Profunctor (EitherSide p c d) where
  dimap f g = N.over EitherSide (dimap (map f) (map g))

instance choiceEitherSide :: Choice p => Choice (EitherSide p c d) where
  left p = swapped (right p)
  right = N.over EitherSide (shuffled <<< right)

eitherside :: forall p sc sd ta tb a b c d. Profunctor p => L.Optic (EitherSide p sc sd) ta tb a b -> L.Optic (EitherSide p a b) sc sd c d -> L.Optic p (Either ta sc) (Either tb sd) (Either a c) (Either b d)
eitherside lab lcd = swapped <<< N.under EitherSide lab <<< swapped <<< N.under EitherSide lcd

splitting :: forall p s t a b c d. Profunctor p => (s -> a) -> (b -> t) -> L.Optic p (Either c s) (Either d t) (Either c a) (Either d b)
splitting f g = N.under EitherSide (dimap f g)

prismOf :: forall s t a b c d. (b -> t) -> (s -> Either t a) -> L.Prism (Either c s) (Either d t) (Either c a) (Either d b)
prismOf f g = N.under EitherSide (L.prism f g)