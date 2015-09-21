module Data.Tree
  ( Tree()
  , View(..)
  , out
  , into
  , transView
  , flatten
  ) where

import Prelude
import Data.Array ((:), concatMap)
import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Traversable (Traversable, traverse, sequence)
import Data.NonEmpty (NonEmpty(), (:|))

-- | An abstract tree type
data Tree a = Tree a (Array (Tree a))

-- | A view which is used to construct and destruct the tree. This type is a
-- | pattern functor/signature, where `f` represents the recursive structure.
data View f a = View a (Array (f a))

-- | Pattern match on a tree. Example:
-- |
-- | ````purescript
-- | case out t of
-- |   View a ts -> ...
-- | ````
out :: forall a. Tree a -> View Tree a
out (Tree a ts) = View a ts

-- | Construct a tree from a tree pattern/view. Example:
-- |
-- | ````purescript
-- | tree :: Tree Int
-- | tree = into $ View 0 []
-- | ````
into :: forall a. View Tree a -> Tree a
into (View a ts) = Tree a ts

-- | Map a tree across a natural transformation.
transView :: forall f g a. (forall b. f b -> g b) -> View f a -> View g a
transView η (View a ts) = View a (η <$> ts)

instance showTree :: (Show a) => Show (Tree a) where
  show (Tree a ts) = "Tree " <> show a <> " " <> show ts

instance eqTree :: (Eq a) => Eq (Tree a) where
  eq (Tree a ts) (Tree a' ts') = a == a' && ts == ts'

instance functorTree :: Functor Tree where
  map f (Tree a ts) = Tree (f a) (map f <$> ts)

instance applyTree :: Apply Tree where
  apply (Tree f tf) t@(Tree a ta) = Tree (f a) (map (f <$>) ta <> map (<*> t) tf)

instance applicativeTree :: Applicative Tree where
  pure a = Tree a []

instance bindTree :: Bind Tree where
  bind (Tree a ts) f = case f a of
    Tree a' ts' -> Tree a' (ts' <> map (`bind` f) ts)

instance monadTree :: Monad Tree

instance foldableTree :: Foldable Tree where
  foldr f b (Tree a ts) = f a (foldr (flip $ foldr f) b ts)
  foldl f b (Tree a ts) = f (foldl (foldl f) b ts) a
  foldMap f (Tree a ts) = f a <> foldMap (foldMap f) ts

instance traversableTree :: Traversable Tree where
  traverse f (Tree a ts) = Tree <$> f a <*> traverse (traverse f) ts
  sequence = traverse id

flatten :: forall a. Tree a -> NonEmpty Array a
flatten (Tree a ts) = a :| concatMap flatten' ts
  where flatten' (Tree a []) = [a]
        flatten' (Tree a ts) = a : concatMap flatten' ts
