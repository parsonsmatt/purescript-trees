module Test.Main where

import Prelude

import Control.Monad.Eff.Console as Console
import Test.Data.Tree
import Test.Data.Tree.Fan

main = do
  Console.log "Tests for Data.Tree"
  testTree
  Console.log "Tests for Data.Tree.Fan"
  testFan
