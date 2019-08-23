module Data.Lens.Plated where

import Prelude

import Control.Lazy (fix)
import Control.Monad.State as S
import Data.Argonaut (Json, caseJson, fromArray, fromBoolean, fromNumber, fromObject, fromString, jsonNull)
import Data.Array as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens as L
import Data.List as List
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.Traversable (maximum, traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

partsOf ∷ forall s a. L.Traversal' s a -> L.Lens' s (Array a)
partsOf o = L.lens getter setter
  where
    getter s = L.foldMapOf o A.singleton s
    setter s xs = S.evalState (L.traverseOf o (S.state <<< fill) s) xs
    fill a as = case A.uncons as of
      Nothing -> Tuple a []
      Just r -> Tuple r.head r.tail

parts :: forall a. Plated a => L.Lens' a (Array a)
parts = partsOf plate

composOpFold :: forall a b. Plated a => b -> (b -> b -> b) -> (a -> b) -> a -> b
composOpFold z c f = L.foldrOf plate (c <<< f) z

para :: forall r a. Plated a => (a -> Array r -> r) -> a -> r
para = paraOf plate

paraOf :: forall a r. L.Fold' (Array a) a a -> (a -> Array r -> r) -> a -> r
paraOf l f = go where go a = f a (go <$> L.foldMapOf l A.singleton a)

cosmosOf ∷ ∀ a . L.Traversal' a a → L.Traversal' a a
cosmosOf d = L.wander (go (L.traverseOf d))
  where
    go :: forall f. Applicative f => ((a -> f a) -> a -> f a) -> (a -> f a) -> a -> f a
    go = fix (\r d f s -> f s *> d (r d f) s)

cosmos :: forall a. Plated a => L.Traversal' a a
cosmos = cosmosOf plate

cosmosOn :: forall s a. Plated a => L.Traversal' s a -> L.Traversal' s a
cosmosOn d = cosmosOnOf d plate

cosmosOnOf :: forall s a. L.Traversal' s a -> L.Traversal' a a -> L.Traversal' s a
cosmosOnOf d p = d <<< cosmosOf p

children ∷ forall a. Plated a => a -> List.List a
children = L.toListOf plate

universe ∷ forall a. Plated a => a -> (Array a)
universe = universeOf plate

universeOf ∷ forall a. L.Fold' (Array a) a a -> a -> Array a
universeOf l = go where go a = A.cons a (L.foldMapOf l go a)

universeOn ::  forall s a. Plated a => L.Fold' (Array a) s a -> s -> Array a
universeOn b = universeOnOf b plate

universeOnOf :: forall s a. L.Fold' (Array a) s a -> L.Fold' (Array a) a a -> s -> Array a
universeOnOf b = L.foldMapOf b <<< universeOf

transform ∷ forall a. Plated a => (a -> a) -> a -> a
transform = transformOf plate

transformOf ∷ forall a b. L.Setter a b a b -> (b -> b) -> a -> b
transformOf l f = go where go a = f (L.over l go a)

transformOn :: forall s t a. Plated a => L.Setter s t a a -> (a -> a) -> s -> t
transformOn b = L.over b <<< transform

transformOnOf :: forall s t a b. L.Setter s t a b -> L.Setter a b a b -> (b -> b) -> s -> t
transformOnOf b l = L.over b <<< transformOf l

rewrite :: forall a. Plated a => (a -> Maybe a) -> a -> a
rewrite = rewriteOf plate

rewriteOf :: forall a b. L.Setter a b a b -> (b -> Maybe a) -> a -> b
rewriteOf l f = go where
  go = transformOf l (\x -> maybe x go (f x))

rewriteOn :: forall s t a. Plated a => L.Setter s t a a -> (a -> Maybe a) -> s -> t
rewriteOn b = L.over b <<< rewrite

rewriteOnOf :: forall s t a b. L.Setter s t a b -> L.Setter a b a b -> (b -> Maybe a) -> s -> t
rewriteOnOf b l = L.over b <<< rewriteOf l

transformM :: forall m a. Monad m => Plated a => (a -> m a) -> a -> m a
transformM = transformMOf plate

transformMOf :: forall m a b. Monad m => L.Traversal a b a b -> (b -> m b) -> a -> m b
transformMOf l f = go where
  go t = L.traverseOf l go t >>= f

transformMOn :: forall m s a. Monad m => Plated a => L.Traversal' s a -> (a -> m a) -> s -> m s
transformMOn b = L.traverseOf b <<< transformM

transformMOnOf :: forall m s t a b. Monad m => L.Traversal s t a b -> L.Traversal a b a b -> (b -> m b) -> s -> m t
transformMOnOf b l = L.traverseOf b <<< transformMOf l

class Plated a where
  plate ∷ L.Traversal' a a

instance platedArray ∷ Plated (Array a) where
  plate = L.wander go
    where
      go ∷ forall f. Applicative f => (Array a -> f (Array a)) -> Array a -> f (Array a)
      go f a = case A.uncons a of
        Nothing -> pure []
        Just r -> (A.cons r.head) <$> f r.tail

instance jsonPlated :: Plated Json where
  plate = L.wander go
    where
      go :: forall f. Applicative f => (Json -> f Json) -> Json -> f Json
      go f = caseJson
        (\_ -> pure jsonNull) 
        (pure <<< fromBoolean) 
        (pure <<< fromNumber) 
        (pure <<< fromString) 
        (map fromArray <<< traverse f)
        (map fromObject <<< traverse f)

-- example

data Expr
  = Val Int
  | Neg Expr
  | Add Expr Expr
  | Mul Expr Expr

derive instance genericExpr ∷ Generic Expr _

instance showExpr ∷ Show Expr where
  show e = genericShow e

instance platedExpr ∷ Plated Expr where
  plate = L.wander plate' where
    plate' ∷ forall f. Applicative f => (Expr -> f Expr) -> Expr -> f Expr
    plate' f (Neg e) = Neg <$> f e
    plate' f (Add a b) = Add <$> f a <*> f b
    plate' f (Mul a b) = Mul <$> f a <*> f b
    plate' _ a = pure a

_Val :: L.Prism' Expr Int
_Val = L.prism Val go
  where
    go (Val x) = Right x
    go e = Left e

evalAlgebra :: Partial => Expr -> Array Int -> Int
evalAlgebra e r = case e, r of
  Val i, _ -> i
  Add _ _, [r1, r2] -> r1 + r2
  Mul _ _, [r1, r2] -> r1 * r2
  Neg _, [r1] -> negate r1

expr ∷ Expr
expr = Add (Val 10) (Neg (Val 20))

expr2 ∷ Expr
expr2 = Mul (Val 10) (Add (Val 20) expr)

eval :: Expr -> Int
eval e = unsafePartial (para evalAlgebra e)

depth :: forall a. Plated a => a -> Int
depth = para (\_ cs -> 1 + fromMaybe 0 (maximum (A.cons 0 cs)))

negLits ∷ Expr -> Expr
negLits = transform $ \x -> case x of
  Neg (Val i) -> Val (negate i)
  _           -> x

distr ∷ Expr -> Expr
distr  = transform $ \x -> case x of
  Mul a (Add c d) -> Add (Mul a c) (Mul a d)
  _ -> x

data T = L | B T T

derive instance genericT :: Generic T _

instance showT :: Show T where
  show t = genericShow t

instance platedT :: Plated T where
  plate = L.wander go
    where
      go :: forall f. Applicative f => (T -> f T) -> T -> f T
      go f L = pure L
      go f (B a b) = B <$> f a <*> f b

t :: T
t = (B L (B (B L L) L))