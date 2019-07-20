module Data.Lens.Variant where

import Data.Lens as L
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj, prj)
import Prim.Row as Row

_Variant :: forall l t v a. IsSymbol l => Row.Cons l a t v  => SProxy l -> L.Prism' (Variant v) a
_Variant l = L.prism' (inj l) (prj l)