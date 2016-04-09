-- | An implementation of a spread based on the cofree comonad of lazy lists.
module Data.Tree.Spread.CFL where

import Prelude

import Control.Bind
import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Comonad.Cofree as CF
import Data.List.Lazy
import Math as Math
import Control.Extend (class Extend, extend)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Lazy
import Control.Comonad (class Comonad, extract)
import Data.Foldable (class Foldable, foldr, foldl, foldMap, and)
import Data.Int as Int

import Test.QuickCheck.Arbitrary (class Coarbitrary, class Arbitrary, coarbitrary, arbitrary)
import Test.QuickCheck.Gen (sized)

newtype Spread a = Spread (Cofree List a)

unSpread :: forall a. Spread a -> Cofree List a
unSpread (Spread a) = a

subtrees :: forall a. Spread a -> List (Spread a)
subtrees (Spread a) = map Spread (CF.tail a)

instance eqSpread :: Eq a => Eq (Spread a) where
  eq (Spread a) (Spread b) = and ((==) <$> a <*> b)

instance showSpread :: Show a => Show (Spread a) where
  show s =
    "(Spread "
     <> show (extract s)
     <> " "
     <> show (subtrees s)
     <> " )"


instance functorSpread :: Functor Spread where
  map f (Spread w) = Spread (map f w)

instance applySpread :: Apply Spread where
  apply (Spread fw) (Spread fa) = Spread (apply fw fa)

instance applicativeSpread :: Applicative Spread where
  pure a = Spread (mkCofree a nil)

instance bindSpread :: Bind Spread where
  bind cfa f =
    Spread (unSpread cfa >>= unSpread <<< f)

instance monadSpread :: Monad Spread

instance extendSpread :: Extend Spread where
  extend f = Spread <<< extend (f <<< Spread) <<< unSpread

instance comonadSpread :: Comonad Spread where
  extract = extract <<< unSpread

instance foldableSpread :: Foldable Spread where
  foldr k z (Spread c) = foldr k z c
  foldl k z (Spread c) = foldl k z c
  foldMap k (Spread c) = foldMap k c

instance traversableSpread :: Traversable Spread where
  traverse f (Spread c) = Spread <$> traverse f c
  sequence = traverse id

instance arbitrarySpread :: Arbitrary a => Arbitrary (Spread a) where
  arbitrary = Spread <$> sized go
    where go 0 = (_ `mkCofree` nil) <$> arbitrary
          go n = do
            sz <- Int.floor <<< Math.abs <$> arbitrary
            a <- arbitrary
            rest <- replicateM sz (go (n / (sz + 1)))
            return (mkCofree a rest)

replicateM :: forall a m. Applicative m => Int -> m a -> m (List a)
replicateM i _ | i <= 0 = pure nil
replicateM i ma = (:) <$> ma <*> replicateM (i - 1) ma

instance coarbitrarySpread :: Coarbitrary a => Coarbitrary (Spread a) where
  coarbitrary (Spread c) =
    coarbitrary (CF.head c)
      <<< foldl (<<<) id (map (\_ -> coarbitrary 3) (CF.tail c))
