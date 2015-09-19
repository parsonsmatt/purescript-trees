module Data.Fan
  ( Fan()
  , View(..)
  , into
  , intoLazy
  , out
  , outLazy
  , transView
  ) where

import Prelude
import Data.Lazy
import Data.Array
import Data.Functor.Compose

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

