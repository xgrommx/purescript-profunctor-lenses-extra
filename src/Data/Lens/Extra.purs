module Data.Lens.Extra where

import Prelude

import Control.Monad.State as S
import Control.Plus (class Plus, (<|>), empty)
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap)
import Data.Bitraversable (class Bitraversable, bitraverse)
import Data.Either (Either(..))
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Lens (APrism, APrism', AnIso, Fold, Forget, Indexed(..), IndexedOptic, IndexedTraversal, Iso, Optic, Optic', Prism, Prism', Traversal, anyOf, collectOf, filtered, foldrOf, has, iso, prism, traverseOf, wander, withIso, withPrism, zipFWithOf, (.~)) as L
import Data.Lens (class Wander)
import Data.Lens.At (class At, at) as L
import Data.Lens.Indexed (iwander) as L
import Data.List.Lazy as LL
import Data.List.ZipList (ZipList(..))
import Data.Maybe (Maybe(..))
import Data.Monoid.Disj (Disj)
import Data.Monoid.Endo (Endo)
import Data.Newtype as N
import Data.Profunctor (class Profunctor)
import Data.Profunctor as P
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Star (Star(..))
import Data.Traversable (class Traversable, traverse)
import Data.Tuple (Tuple(..), swap)
import Unsafe.Coerce (unsafeCoerce)

type FilterLike f s t a b = L.Optic (Star f) s t a (Maybe b)

infixr 4 L.collectOf as %%~

infixr 4 L.zipFWithOf as ~%%

from ∷ forall s t a b. L.AnIso s t a b -> L.Iso b a t s
from l = L.withIso l $ \ sa bt -> L.iso bt sa

cmapping ∷ forall f s t a b. Contravariant f => L.AnIso s t a b -> L.Iso (f a) (f b) (f s) (f t)
cmapping f = L.withIso f $ \ sa bt -> L.iso (cmap sa) (cmap bt)

lcmapping ∷ forall p q s t a b x y. Profunctor p => Profunctor q => L.AnIso s t a b -> L.Iso (p a x) (q b y) (p s x) (q t y)
lcmapping f = L.withIso f $ \ sa bt -> L.iso (P.lcmap sa) (P.lcmap bt)

rmapping ∷ forall p q s t a b x y. Profunctor p => Profunctor q => L.AnIso s t a b -> L.Iso (p x s) (q y t) (p x a) (q y b)
rmapping g = L.withIso g $ \ sa bt -> L.iso (P.rmap sa) (P.rmap bt)

bimapping ∷ forall f g s t a b s' t' a' b'. Bifunctor f => Bifunctor g => L.AnIso s t a b -> L.AnIso s' t' a' b' -> L.Iso (f s s') (g t t') (f a a') (g b b')
bimapping f g = L.withIso f $ \ sa bt -> L.withIso g $ \s'a' b't' -> L.iso (bimap sa s'a') (bimap bt b't')

firsting ∷ forall f g s t a b x y. Bifunctor f => Bifunctor g => L.AnIso s t a b -> L.Iso (f s x) (g t y) (f a x) (g b y)
firsting p = L.withIso p $ \ sa bt -> L.iso (lmap sa) (lmap bt)

seconding ∷ forall f g s t a b x y. Bifunctor f => Bifunctor g => L.AnIso s t a b -> L.Iso (f x s) (g y t) (f x a) (g y b)
seconding p = L.withIso p $ \ sa bt -> L.iso (rmap sa) (rmap bt)

simple ∷ forall p s a. L.Optic' p s a -> L.Optic' p s a
simple = identity

forOf :: forall f s t a b. L.Optic (Star f) s t a b -> s -> (a -> f b) -> f t
forOf l s f = L.traverseOf l f s

transposeOf :: forall s t a. L.Optic (Star ZipList) s t (LL.List a) a -> s -> LL.List t
transposeOf l = N.un ZipList <<< L.traverseOf l ZipList

asumOf :: forall b c t f a. Plus f => (L.Forget (Endo Function (f a)) (f a) b -> L.Forget (Endo Function (f a)) t c) -> t -> f a
asumOf l = L.foldrOf l (<|>) empty

aside ∷ forall s t a b e. L.APrism s t a b -> L.Prism (Tuple e s) (Tuple e t) (Tuple e a) (Tuple e b)
aside k =
  L.withPrism k $ \bt seta ->
    L.prism (map bt) $ \(Tuple e s) ->
      case seta s of
        Left t  -> Left  (Tuple e t)
        Right a -> Right (Tuple e a)

sans ∷ forall m a b. L.At m a b => a -> m -> m
sans k = L.at k L..~ Nothing

both ∷ forall r a b. Bitraversable r => L.Traversal (r a a) (r b b) a b
both = L.wander (join bitraverse)

unsafeCoerced :: forall s t a b. L.Iso s t a b
unsafeCoerced = L.iso unsafeCoerce unsafeCoerce

below ∷ forall f s a. Traversable f => L.APrism' s a -> L.Prism' (f s) (f a)
below k =
  L.withPrism k $ \bt seta ->
    L.prism (map bt) $ \s ->
      case traverse seta s of
        Left _  -> Left s
        Right t -> Right t

mapAccumLOf :: forall acc s t a b. L.Optic (Star (S.State acc)) s t a b -> (acc -> a -> (Tuple acc b)) -> acc -> s -> (Tuple acc t)
mapAccumLOf l f acc s = swap (S.runState (L.traverseOf l (\a -> S.state $ \acc' -> swap (f acc' a)) s) acc)

ifilteredOf
  ∷ forall p i s t a
   . Wander p
  => L.IndexedTraversal i s t a a
  -> (i -> a -> Boolean)
  -> L.IndexedOptic p i s t a a
ifilteredOf tr pr = L.iwander \f -> N.unwrap $ tr $ L.Indexed $ Star $ \(Tuple i a) -> if pr i a then f i a else pure a

filteredOf
  ∷ forall p s t a
   . Wander p
  => L.Traversal s t a a
  -> (a -> Boolean)
  -> L.Optic p s t a a
filteredOf tr pr = L.wander \f -> N.unwrap $ tr $ Star $ \a -> if pr a then f a else pure a

filteredBy :: forall p s t a b. Choice p => (a -> Boolean) -> L.Fold (Disj Boolean) s t a b -> L.Optic' p s s
filteredBy cond lens = L.filtered (L.anyOf lens cond)

filteredByLens :: forall p s t a b. Choice p => L.Fold (Disj Boolean) s t a b -> L.Optic' p s s
filteredByLens lens = L.filtered (L.has lens)