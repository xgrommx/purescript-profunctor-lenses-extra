module Data.Lens.Cons where

import Prelude

import Control.Apply (lift2)
import Data.Array ((:))
import Data.Array as A
import Data.Array.Partial as AP
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Lens as L
import Data.Maybe (Maybe, maybe)
import Data.String as SU
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..), curry, uncurry)
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

_drop ∷ forall s a. Cons s s a a => Int -> L.Traversal' s s
_drop 0 = identity
_drop n = _tail <<< _drop (n - 1)

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