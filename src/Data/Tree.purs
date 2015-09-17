module Data.Tree
  ( Tree(..)
  , flatten
  ) where

import Prelude
import Data.Array ((:), concatMap)
import Data.Foldable
import Data.Traversable (Traversable, traverse, sequence)
import Data.NonEmpty (NonEmpty(), (:|))
import Control.Alt (Alt, alt)

data Tree a = Tree a (Array (Tree a))

instance showTree :: (Show a) => Show (Tree a) where
  show (Tree a ts) = "Tree " <> show a <> " " <> show ts

instance eqTree :: (Eq a) => Eq (Tree a) where
  eq (Tree a aTs) (Tree b bTs) = a == b && aTs == bTs

instance functorTree :: Functor Tree where
  map f (Tree a ts) = Tree (f a) (map f <$> ts)

instance applyTree :: Apply Tree where
  apply (Tree f fTs) aT@(Tree a aTs) = Tree (f a) (map (f <$>) aTs <> map (<*> aT) fTs)

instance applicativeTree :: Applicative Tree where
  pure a = Tree a []

instance bindTree :: Bind Tree where
  bind (Tree x ts) f = case f x of
    Tree x' ts' -> Tree x' (ts' <> map (`bind` f) ts)

instance monadTree :: Monad Tree

instance foldableTree :: Foldable Tree where
  foldr f b (Tree a ts) = f a (foldr (flip $ foldr f) b ts)
  foldl f b (Tree a ts) = f (foldl (foldl f) b ts) a
  foldMap f (Tree a ts) = f a <> foldMap (foldMap f) ts

instance traversableTree :: Traversable Tree where
--  traverse :: forall a b m. (Applicative m) => (a -> m b) -> t a -> m (t b)
  traverse f (Tree x ts) = Tree <$> f x <*> traverse (traverse f) ts
  sequence = traverse id

flatten :: forall a. Tree a -> NonEmpty Array a
flatten (Tree a ts) = a :| concatMap flatten' ts
  where flatten' (Tree a []) = [a]
        flatten' (Tree a ts) = a : concatMap flatten' ts
