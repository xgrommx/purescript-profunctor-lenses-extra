module Data.Lens.Assoc where

import Prelude

import Data.Lens as L

import Data.Bifunctor
import Data.Either
import Data.These
import Data.Tuple (Tuple(..))

import Data.Lens.Swapped

shuffle 
  :: forall p a b c
   . Bifunctor p 
  => Swap p 
  => Assoc p 
  => p a (p b c) 
  -> p b (p a c)
shuffle = assoc <<< lmap swap <<< reassoc

shuffled 
  :: forall p a b c d e f
   . Bifunctor p 
   => Swap p 
   => Assoc p 
   => L.Iso (p a (p b c)) (p d (p e f)) (p b (p a c)) (p e (p d f))
shuffled = L.iso shuffle shuffle

assoced 
  :: forall p a b c d e f
   . Assoc p
  => L.Iso (p (p a b) c) (p d (p e f)) (p a (p b c)) (p (p d e) f)
assoced = L.iso assoc assoc

assoced' 
  :: forall p a b c d e f
   . Assoc p
   => L.Iso (p (p a b) c) (p (p d e) f) (p a (p b c)) (p d (p e f))
assoced' = L.iso assoc reassoc

reassoced 
  :: forall p a b c d e f
   . Assoc p
   => L.Iso (p a (p b c)) (p (p d e) f) (p (p a b) c) (p d (p e f))
reassoced = L.iso reassoc reassoc

reassoced' 
  :: forall p c b a p f e d
   . Assoc p
   => L.Iso (p a (p b c)) (p d (p e f)) (p (p a b) c) (p (p d e) f)
reassoced' = L.iso reassoc assoc

class Assoc p where
  assoc :: forall a b c. p (p a b) c -> p a (p b c)
  reassoc :: forall a b c. p a (p b c) -> p (p a b) c

instance assocTuple :: Assoc Tuple where
  assoc ∷ forall x y z. Tuple (Tuple x y) z -> Tuple x (Tuple y z)
  assoc (Tuple (Tuple x y) z) = Tuple x (Tuple y z)

  reassoc ∷ forall x y z. Tuple x (Tuple y z) -> Tuple (Tuple x y) z
  reassoc (Tuple x (Tuple y z)) = Tuple (Tuple x y) z

instance assocThese :: Assoc These where
  assoc ∷ forall a b c. These (These a b) c -> These a (These b c)
  assoc (This (This a)) = This a
  assoc (This (That b)) = That (This b)
  assoc (That c) = That (That c)
  assoc (Both (That b) c) = That (Both b c)
  assoc (This (Both a b)) = Both a (This b)
  assoc (Both (This a) c) = Both a (That c)
  assoc (Both (Both a b) c) = Both a (Both b c)

  reassoc ∷ forall a b c. These a (These b c) -> These (These a b) c
  reassoc (This a) = This (This a)
  reassoc (That (This b)) = This (That b)
  reassoc (That (That c)) = That c
  reassoc (That (Both b c)) = Both (That b) c
  reassoc (Both a (This b)) = This (Both a b)
  reassoc (Both a (That c)) = Both (This a) c
  reassoc (Both a (Both b c)) = Both (Both a b) c

instance assocEither :: Assoc Either where
  assoc :: forall a b c. Either (Either a b) c -> Either a (Either b c)
  assoc (Left (Left a)) = Left a
  assoc (Left (Right b)) = Right (Left b)
  assoc (Right c) = Right (Right c)

  reassoc :: forall a b c. Either a (Either b c) -> Either (Either a b) c
  reassoc (Left a) = Left (Left a)
  reassoc (Right (Left b)) = Left (Right b)
  reassoc (Right (Right c)) = Right c  