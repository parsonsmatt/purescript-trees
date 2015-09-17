module Data.Tree
  ( Tree(..)
  , DepthFirst()
  , BreadthFirst()
  , flatten
  ) where

import Prelude
import Data.Array ((:), concatMap)
import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Traversable (Traversable, traverse, sequence)
import Data.NonEmpty (NonEmpty(), (:|))

data DepthFirst
data BreadthFirst

data Tree alg a = Tree a (Array (Tree alg a))

instance showTree :: (Show a) => Show (Tree alg a) where
  show (Tree a ts) = "Tree " <> show a <> " " <> show ts

instance eqTree :: (Eq a) => Eq (Tree alg a) where
  eq (Tree a ts) (Tree a' ts') = a == a' && ts == ts'

instance functorTree :: Functor (Tree alg) where
  map f (Tree a ts) = Tree (f a) (map f <$> ts)

instance applyTree :: Apply (Tree alg) where
  apply (Tree f tf) t@(Tree a ta) = Tree (f a) (map (f <$>) ta <> map (<*> t) tf)

instance applicativeTree :: Applicative (Tree alg) where
  pure a = Tree a []

instance bindTree :: Bind (Tree alg) where
  bind (Tree a ts) f = case f a of
    Tree a' ts' -> Tree a' (ts' <> map (`bind` f) ts)

instance monadTreeDepthFirst :: Monad (Tree DepthFirst)
instance monadTreeBreadthFirst :: Monad (Tree BreadthFirst)

instance foldableTree :: Foldable (Tree alg) where
  foldr f b (Tree a ts) = f a (foldr (flip $ foldr f) b ts)
  foldl f b (Tree a ts) = f (foldl (foldl f) b ts) a
  foldMap f (Tree a ts) = f a <> foldMap (foldMap f) ts

instance traversableTreeDepthFirst :: Traversable (Tree DepthFirst) where
  traverse f (Tree a ts) = Tree <$> f a <*> traverse (traverse f) ts
  sequence = traverse id

instance traversableTreeBreadthFirst :: Traversable (Tree BreadthFirst) where
  traverse f (Tree a ts) = (flip Tree) <$> traverse (traverse f) ts <*> f a
  sequence = traverse id

flatten :: forall alg a. Tree alg a -> NonEmpty Array a
flatten (Tree a ts) = a :| concatMap flatten' ts
  where flatten' (Tree a []) = [a]
        flatten' (Tree a ts) = a : concatMap flatten' ts
