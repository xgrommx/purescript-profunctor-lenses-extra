module Data.Lens.VariantF where

import Prelude

import Data.Functor.Variant (VariantF, inj, prj)
import Data.Lens as L
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant.Internal (FProxy)
import Prim.Row as Row

_VariantF :: forall l t f v a. IsSymbol l => Functor f => Row.Cons l (FProxy f) t v => SProxy l -> L.Prism' (VariantF v a) (f a)
_VariantF l = L.prism' (inj l) (prj l)