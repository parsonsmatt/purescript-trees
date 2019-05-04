module Test.Main where

import Test.Data.Tree
import Effect (Effect)
import Prelude (Unit)

main :: Effect Unit
main = do
  testTree
