module Control.Applicative.Backwards where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Alternative)
import Control.Apply (lift2)
import Control.Plus (class Plus, empty)
import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Traversable (class Traversable, sequence, traverse)

infixl 4 apApplyFlipped as <**>

apApplyFlipped ∷ forall f b a. Apply f => f a -> f (a -> b) -> f b
apApplyFlipped = lift2 (#)

newtype Backwards f a = Backwards (f a)

forwards :: forall f a. Backwards f a -> f a
forwards (Backwards fa) = fa

instance functorBackwards :: Functor f => Functor (Backwards f) where
  map f (Backwards a) = Backwards (map f a)

instance applyBackwards :: Apply f => Apply (Backwards f) where
  apply (Backwards f) (Backwards a) = Backwards (a <**> f)

instance applicativeBackwards :: Applicative f => Applicative (Backwards f) where
  pure a = Backwards (pure a)

instance altBackwards :: Alt f => Alt (Backwards f) where
  alt (Backwards x) (Backwards y) = Backwards (x <|> y)  

instance plusBackwards :: Plus f => Plus (Backwards f) where
  empty = Backwards empty

instance alternativeBackwards :: Alternative f => Alternative (Backwards f)

instance foldableBackwards :: Foldable f => Foldable (Backwards f) where
  foldMap f (Backwards t) = foldMap f t
  foldr f z (Backwards t) = foldr f z t
  foldl f z (Backwards t) = foldl f z t

instance traversableBackwards :: Traversable f => Traversable (Backwards f) where
  traverse f (Backwards t) = map Backwards (traverse f t)
  sequence (Backwards t) = map Backwards (sequence t)

instance contravariantBackwards :: Contravariant f => Contravariant (Backwards f) where
  cmap f = Backwards <<< cmap f <<< forwards