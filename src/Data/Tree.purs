module Data.Tree
  ( Tree(..)
  ) where

import Prelude

data Tree a = Tree a [Tree a]
