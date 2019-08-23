module Data.Lens.Cons where

import Prelude

import Control.Apply (lift2)
import Data.Array (reverse, (..), (:))
import Data.Array as A
import Data.Array.Partial as AP
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (null)
import Data.Lens (first)
import Data.Lens as L
import Data.Lens.Indexed as L
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe.First (First(..))
import Data.Newtype (un)
import Data.String as SU
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..), curry, uncurry)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)

simply ∷ forall p s a r. (L.Optic' p s a -> r) -> L.Optic' p s a -> r
simply = identity

infixr 5 cons as <|

cons ∷ forall s a. Cons s s a a => a -> s -> s
cons = curry (simply L.review _Cons)

uncons ∷ forall s a. Cons s s a a => s -> Maybe (Tuple a s)
uncons = simply L.preview _Cons

infixl 5 snoc as |>

snoc ∷ forall s a. Snoc s s a a => s -> a -> s
snoc = curry (simply L.review _Snoc)

unsnoc ∷ forall s a. Snoc s s a a => s -> Maybe (Tuple s a)
unsnoc = simply L.preview _Snoc

_head ∷ forall s a. Cons s s a a => L.Traversal' s a
_head = _Cons <<< L._1

_tail ∷ forall s a. Cons s s a a => L.Traversal' s s
_tail = _Cons <<< L._2

_init ∷ forall s a. Snoc s s a a => L.Traversal' s s
_init = _Snoc <<< L._1

_last ∷ forall s a. Snoc s s a a => L.Traversal' s a
_last = _Snoc <<< L._2

-- _drop ∷ forall s a. Cons s s a a => Int -> L.Traversal' s s
-- _drop 0 = identity
-- _drop n = _tail <<< _drop (n - 1)

class Cons s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Cons ∷ L.Prism s t (Tuple a s) (Tuple b t)

instance consArray ∷ Cons (Array a) (Array b) a b where
  _Cons = L.prism (uncurry (:)) (maybe (Left []) (Right <<< lift2 Tuple _.head _.tail) <<< A.uncons)

else instance consString ∷ Cons String String Char Char where
  _Cons = L.withPrism _Cons \r p -> 
    L.prism (String.fromCharArray <<< r <<< map String.toCharArray)
            (bimap String.fromCharArray (map String.fromCharArray) <<< p <<< String.toCharArray)
      where
        arrayCons ∷ L.Prism' (Array Char) (Tuple Char (Array Char))
        arrayCons = _Cons

class Snoc s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Snoc ∷ L.Prism s t (Tuple s a) (Tuple t b)

instance snocArray ∷ Snoc (Array a) (Array b) a b where
  _Snoc = L.prism (\(Tuple as a) -> as <> [a]) $
          \arr -> if A.null arr then Left [] else Right (Tuple (unsafePartial AP.init arr) (unsafePartial AP.last arr))

else instance snocString ∷ Snoc String String Char Char where
  _Snoc = L.prism (\(Tuple as a) -> as <> String.singleton a) $
          \arr -> if SU.null arr
                    then Left ""
                    else Right $ Tuple
                      (String.fromCharArray <<< unsafePartial AP.init <<< String.toCharArray $ arr)
                      (unsafePartial AP.last <<< String.toCharArray $ arr)

class AsEmpty a where
  _Empty :: L.Prism' a Unit

_EmptyDefault :: forall a. Eq a => Monoid a => L.Prism' a Unit
_EmptyDefault = L.only mempty

empty :: forall a. AsEmpty a => a
empty = L.review _Empty unit

instance asEmptyArray :: AsEmpty (Array a) where
  _Empty = L.nearly [] null

splitCons :: forall s a. AsEmpty s => Cons s s a a => Int -> s -> Tuple s s
splitCons 0 s = Tuple empty s
splitCons n s = case uncons s of
    Just (Tuple a rest) -> first (cons a) $ splitCons (n - 1) rest
    Nothing -> Tuple empty empty

_take :: forall s a. Semigroup s => Cons s s a a => AsEmpty s => Int -> L.Traversal' s s
_take n = L.wander \f s -> case splitCons n s of
    Tuple prefix suffix -> lift2 (<>) (f prefix) (pure suffix)

_itake :: forall s i a. Semigroup s => Cons s s a a => AsEmpty s => i -> Int -> L.IndexedTraversal' i s s
_itake i n = L.iwander \f s -> case splitCons n s of
    Tuple prefix suffix -> lift2 (<>) (f i prefix) (pure suffix)

_drop :: forall s a. Semigroup s => Cons s s a a => AsEmpty s => Int -> L.Traversal' s s
_drop n = L.wander \f s -> case splitCons n s of
    Tuple prefix suffix -> lift2 (<>) (pure prefix) (f suffix)

_rotate :: forall s a. Semigroup s => Cons s s a a => AsEmpty s => Int -> L.Traversal' s s
_rotate n = L.wander \f s -> case splitCons n s of
    Tuple prefix suffix -> lift2 (<>) (f suffix) (f prefix)

traverseCons :: forall s a. Cons s s a a => L.Traversal' s a
traverseCons = L.wander go
  where
    go :: forall f. Applicative f => (a -> f a) -> s -> f s
    go f s = case uncons s of
      Just (Tuple a rest) -> cons <$> f a <*> go f rest
      Nothing -> pure s

ipreview l = un First <<< L.ifoldMapOf l (\i a -> First (Just (Tuple i a)))

main :: Effect Unit
main = do
  logShow $ (1..10) L.^. _drop 2 <<< _take 5
  logShow $  ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: Array Int) # _drop 2 <<< _take 2 L..~ [1, 3, 3, 7]
  logShow $ ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10] :: Array Int) # _drop 2 <<< _take 4 L.%~ reverse
  logShow $ ([1, 2, 3] :: Array Int) L.^. _rotate 2