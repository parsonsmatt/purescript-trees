module Data.Tree.Cofree where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Comonad.Cofree (Cofree, mkCofree, head, tail)
import Control.Extend (class Extend, extend)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus, empty)

import Data.Lazy (Lazy, defer)
import Data.List.Lazy (List)
import Data.Maybe (Maybe(..))
import Data.Identity (Identity(), runIdentity)
import Data.Foldable (class Foldable, maximum)
import Data.Functor.Compose (Compose(..), decompose)

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

instance extendTree :: (Functor f, Functor g) => Extend (Tree f g) where
  extend f = Tree .. extend (f .. Tree) .. unTree

instance comonadTree :: (Functor f, Functor g) => Comonad (Tree f g) where
  extract = extract .. unTree

instance applyTree :: (Apply f, Apply g) => Apply (Tree f g) where
  apply (Tree ff) (Tree fa) = Tree (apply ff fa)

instance applicativeTree :: (Apply f, Plus f, Applicative g) => Applicative (Tree f g) where
  pure = singleton

-- there are no legal instances of `MonadPlus`, so this instance is meaningless
instance bindTree :: (Apply f, Apply g, MonadPlus (Compose f g)) => Bind (Tree f g) where
  bind (Tree a) f = Tree (bind a (unTree .. f))

-- | The type of strict trees.
type TreeS f a = Tree f Identity a

-- | The type of lazy trees.
type TreeL f a = Tree f Lazy a

type RoseTree a = TreeS Array a

type Spread a = TreeS List a

type Fan a = TreeL Array a

-- | Get the node of the tree.
node
  :: forall a f g. (Functor f, Functor g)
  => Tree f g a
  -> a
node = extract

-- | Get the subtrees of the tree.
subtrees
  :: forall a f g. (Functor f, Functor g)
  => Tree f g a
  -> Compose f g (Tree f g a)
subtrees (Tree t) = Tree <$> tail t

subtrees'
  :: forall a f. (Functor f)
  => TreeS f a
  -> f (TreeS f a)
subtrees' = map runIdentity .. decompose .. subtrees

-- | Index into the subtrees of the current tree.
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
   . ( Index (f (Identity (TreeS f a))) k (Identity (TreeS f a))
     , Applicative f
     )
  => k
  -> TreeS f a
  -> Maybe (TreeS f a)
index' i = map runIdentity .. index i

depth :: forall f g a. (Functor f, Foldable f, Comonad g, Functor g, Eq a)
      => Tree f g a -> Int
depth = depth' .. unG .. unTree where
    unG a = mkCofree (head a) (map extract (decompose .. map unG .. tail $ a))
    depth' a = case maximum (map depth' (tail a)) of
        Nothing -> 1
        Just n  -> n + 1

repeat :: forall f a. (Applicative f) => a -> TreeL f a
repeat = Tree .. repeat'
  where repeat' x = mkCofree x (Compose (pure (defer \_ -> repeat' x)))

replicate :: forall f g a. (Applicative f, Plus f, Applicative g)
          => Int -> a -> Tree f g a
replicate n = Tree .. replicate' n
    where replicate' 1 a = mkCofree a empty
          replicate' n a = mkCofree a (pure (replicate' (n-1) a))

take :: forall f g a. (Plus f, Functor g)
     => Int
     -> Tree f g a
     -> Tree f g a
take n = Tree .. take' n .. unTree
  where take' 1 a = mkCofree (head a) empty
        take' n a = mkCofree (head a) (take' (n-1) <$> tail a)

tree :: forall f g a. (Functor f, Functor g)
     => a -> f (g (Tree f g a)) -> Tree f g a
tree x = Tree .. mkCofree x .. Compose .. map (map unTree)

tree' :: forall f g a. (Functor f, Applicative g)
      => a -> f (Tree f g a) -> Tree f g a
tree' x = Tree .. mkCofree x .. Compose .. map (pure .. unTree)

-- | Construct a rose tree.
roseTree :: forall a. a -> Array (RoseTree a) -> RoseTree a
roseTree = tree'

-- | Construct a spread.
spread :: forall a. a -> List (Spread a) -> Spread a
spread = tree'

-- | Construct a fan.
fan :: forall a. a -> Array (Lazy (Fan a)) -> Fan a
fan = tree

-- | Construct a fan.
fan' :: forall a. a -> Array (Fan a) -> Fan a
fan' = tree'

-- | Construct an empty tree.
singleton :: forall f g a. (Plus f, Functor g) => a -> Tree f g a
singleton a = Tree (mkCofree a empty)
