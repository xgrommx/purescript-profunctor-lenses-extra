module Data.Lens.Reversing where

import Prelude

import Data.Array as A
import Data.Lens as L
import Data.String.CodeUnits as S

class Reversing t where
  reversing ∷ t -> t

instance reversingArray ∷ Reversing (Array a) where
  reversing = A.reverse

instance reversingString ∷ Reversing String where
  reversing = S.fromCharArray <<< A.reverse <<< S.toCharArray

reversed ∷ forall a. Reversing a => L.Iso' a a
reversed = involuted reversing

involuted ∷ forall a. (a -> a) -> L.Iso' a a
involuted a = L.iso a a