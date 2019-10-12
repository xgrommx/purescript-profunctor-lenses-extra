module Data.Lens.Swapped where

import Data.Bifunctor (class Bifunctor)
import Data.Either (Either(..))
import Data.Lens as L
import Data.Profunctor.Choice ((|||))
import Data.Tuple (Tuple)
import Data.Tuple (swap) as T
import Data.These

class Swap p where
  swap :: forall a b. p a b -> p b a

instance swapTuple :: Swap Tuple where
  swap = T.swap

instance swapEither :: Swap Either where
  swap = Right ||| Left

instance swapThese :: Swap These where
  swap (This a) = That a
  swap (That a) = This a
  swap (Both a b) = Both b a

swapped ∷ forall p a b c d. Swap p => L.Iso (p a b) (p c d) (p b a) (p d c)
swapped = L.iso swap swap