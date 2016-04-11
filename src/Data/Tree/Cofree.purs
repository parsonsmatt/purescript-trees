module Data.Tree.Cofree where

import Prelude

import Control.Plus (class Plus, empty)
import Data.Maybe (Maybe)
import Data.Lazy (Lazy, defer)
import Data.List.Lazy (List)
import Data.Identity (Identity(Identity), runIdentity)
import Data.Functor.Compose (Compose(Compose), decompose)
import Control.Comonad.Cofree (Cofree, mkCofree, tail)

import Optic.Core ((..))
import Optic.Extended ((^?))
import Optic.Index (class Index, ix)

-- | The type of a generalized tree data structure.
-- | The type parameters are:
-- | - `f` : a type constructor that stores the subtrees.
-- | - `g` : a type constructor that specifies further effects of the data
-- | - `a` : a value stored at each node of the tree.
newtype Tree f g a = Tree (Cofree (Compose f g) a)

unTree :: forall a g f. Tree f g a -> Cofree (Compose f g) a
unTree (Tree a) = a

instance functorTree :: (Functor f, Functor g) => Functor (Tree f g) where
  map f = Tree .. map f .. unTree

type TreeI f a = Tree f Identity a

type TreeL f a = Tree f Lazy a

type RoseTree a = TreeI Array a

type Spread a = TreeI List a

type Fan a = TreeL Array a

subtrees
  :: forall a f g. (Functor f, Functor g)
  => Tree f g a
  -> Compose f g (Tree f g a)
subtrees (Tree t) = Tree <$> tail t

index
  :: forall a f g k
   . ( Index (f (g (Tree f g a))) k (g (Tree f g a))
     , Applicative f
     , Functor g
     )
  => k
  -> Tree f g a
  -> Maybe (g (Tree f g a))
index i t = decompose (subtrees t) ^? ix i

index'
  :: forall a f k
   . ( Index (f (Identity (TreeI f a))) k (Identity (TreeI f a))
     , Applicative f
     )
  => k
  -> TreeI f a
  -> Maybe (TreeI f a)
index' i = map runIdentity .. index i


roseTree :: forall a. a -> Array (RoseTree a) -> RoseTree a
roseTree x = Tree .. mkCofree x .. Compose .. map (Identity .. unTree)

spread :: forall a. a -> List (Spread a) -> Spread a
spread x = Tree .. mkCofree x .. Compose .. map (Identity .. unTree)

fan :: forall a. a -> Array (Fan a) -> Fan a
fan x = Tree .. mkCofree x .. Compose .. map ((\a -> defer \_ -> a) .. unTree)

singleton :: forall f g a. (Plus f, Functor g) => a -> Tree f g a
singleton a = Tree (mkCofree a empty)
