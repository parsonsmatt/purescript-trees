module Test.Main where

import Test.Data.Tree
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Prelude (Unit)

main :: forall eff. Eff( console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) Unit
main = do
  testTree
