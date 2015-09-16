module Data.Tree
  ( Tree(..)
  , flatten
  ) where

import Prelude
import Data.Array ((:), concatMap)
import Data.Foldable
import Data.NonEmpty (NonEmpty(), (:|))

data Tree a = Tree a (Array (Tree a))

instance showTree :: (Show a) => Show (Tree a) where
  show (Tree a ts) = "Tree " <> show a <> " " <> show ts

instance eqTree :: (Eq a) => Eq (Tree a) where
  eq (Tree a aTs) (Tree b bTs) = a == b && aTs == bTs

instance functorTree :: Functor Tree where
  map f (Tree a ts) = Tree (f a) (map f <$> ts)

instance foldableTree :: Foldable Tree where
  foldr f b (Tree a []) = f a b
  foldr f b (Tree a ts) = f a (foldr (flip $ foldr f) b ts)
  foldl f b (Tree a []) = f b a
  foldl f b (Tree a ts) = f (foldl (foldl f) b ts) a
  foldMap f (Tree a ts) = f a <> foldMap (foldMap f) ts

flatten :: forall a. Tree a -> NonEmpty Array a
flatten (Tree a ts) = a :| concatMap flatten' ts
  where flatten' (Tree a []) = [a]
        flatten' (Tree a ts) = a : concatMap flatten' ts
