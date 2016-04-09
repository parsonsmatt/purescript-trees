-- | An implementation of a spread based on the cofree comonad of lazy lists.
module Data.Tree.Spread
  ( module X
  ) where

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

import Data.Tree.Spread.CFL as X

