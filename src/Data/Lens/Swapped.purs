module Data.Lens.Swapped where

import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Lens as L
import Data.Profunctor.Choice ((|||))
import Data.Tuple (Tuple, swap)

switch :: forall a b. Either a b -> Either b a
switch = Right ||| Left

class Bifunctor p <= Swapped p where
  swapped ∷ forall a b c d. L.Iso (p a b) (p c d) (p b a) (p d c)

instance swappedTuple ∷ Swapped Tuple where
  swapped = L.iso swap swap

instance swappedEither ∷ Swapped Either where
  swapped = L.iso switch switch