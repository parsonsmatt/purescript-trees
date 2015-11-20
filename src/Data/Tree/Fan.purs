module Data.Tree.Fan
  ( Fan()
  , View(..)
  , into
  , intoLazy
  , out
  , outLazy
  , transView
  ) where

import Prelude
import Data.Array ((:), concatMap)
import Data.Foldable (Foldable, foldr, foldl, foldMap)
import Data.Functor.Compose
import Data.Lazy
import Data.Traversable (Traversable, traverse, sequence)

-- | A fan is a finitarily-branching spread (i.e. a non-wellfounded tree where
-- | each node has only finitely many immediate children).
data Fan a = Fan a (Lazy (Array (Lazy (Fan a))))

-- | A view which is used to construct and destruct the fan. This type is a
-- | pattern functor/signature, where `f` represents the recursive structure.
data View f a = View a (Array (f a))

into :: forall a. View Fan a -> Fan a
into (View a ts) = Fan a (defer \_ -> pure <$> ts)

out :: forall a. Fan a -> View Fan a
out (Fan a ts) = View a (force <$> force ts)

intoLazy :: forall a. View (Compose Lazy Fan) a -> Fan a
intoLazy (View a ts) = Fan a (defer \_ -> decompose <$> ts)

outLazy :: forall a. Fan a -> View (Compose Lazy Fan) a
outLazy (Fan a ts) = View a (Compose <$> force ts)

transView :: forall f g a. (forall b. f b -> g b) -> View f a -> View g a
transView η (View a ts) = View a (η <$> ts)

instance showFan :: (Show a) => Show (Fan a) where
  show (Fan a ts) = "Fan " <> show a <> show (force <$> force ts)

instance eqFan :: (Eq a) => Eq (Fan a) where
  eq (Fan a ts) (Fan a' ts') = a == a' && ts == ts'

instance functorFan :: Functor Fan where
  map f (Fan a ts) = Fan (f a) (map (map (map f)) <$> ts)

instance applyFan :: Apply Fan where
  apply (Fan f fts) t@(Fan a ats) =
    Fan (f a) (map (map (map (f <$>))) ats <> map (map (map (<*> t))) fts)

instance applicativeFan :: Applicative Fan where
  pure a = Fan a (pure [])

instance bindFan :: Bind Fan where
  bind (Fan a ts) f = f a # \(Fan a' ts') -> Fan a' (ts' <> map (map (map (`bind` f))) ts)

instance monadFan :: Monad Fan

instance foldableFan :: Foldable Fan where
  foldr f b (Fan a ts) = f a (force (foldr g b <$> ts))
    where g t b = force (map (foldr f b) t)
  foldl f b (Fan a ts) = f (force (foldl g b <$> ts)) a
    where g b t = force (map (foldl f b) t)
  foldMap f (Fan a ts) = f a <> (force (foldMap (map (foldMap f) >>> force) <$> ts))
