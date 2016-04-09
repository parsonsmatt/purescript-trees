module Data.Tree.Spread where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Comonad.Cofree as CF
import Data.List.Lazy
import Math as Math
import Control.Extend (Extend, extend)
import Data.Traversable (Traversable, traverse, sequence)
import Data.Lazy
import Control.Comonad (Comonad, extract)
import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Int as Int

import Test.QuickCheck.Arbitrary (Coarbitrary, Arbitrary, coarbitrary, arbitrary)
import Test.QuickCheck.Gen (sized)

newtype Spread a = Spread (Cofree List a)

unSpread :: forall a. Spread a -> Cofree List a
unSpread (Spread a) = a

-- instance arbitrarySpread :: Arbitrary a => Arbitrary (Spread a) where
--   arbitrary = sized go
--     where go 0 = map Spread <<< mkCofree <$> arbitrary <*> arbitrary
--          go n = do
--            sz <- Int.floor <<< Math.abs <$> arbitrary
--            a <- arbitrary
--            rest <- replicateM sz (go (n / (sz + 1)))
--            return (Spread (mkCofree a rest))

replicateM :: forall a m. Applicative m => Int -> m a -> m (List a)
replicateM i _ | i <= 0 = pure nil
replicateM i ma = (:) <$> ma <*> replicateM (i - 1) ma

instance coarbitrarySpread :: Coarbitrary a => Coarbitrary (Spread a) where
  coarbitrary (Spread c) =
    coarbitrary (CF.head c)
      <<< foldl (<<<) id (map (\_ -> coarbitrary 3) (CF.tail c))
