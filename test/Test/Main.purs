module Test.Main where

import Prelude

import Control.Monad.Eff.Console as Console
import Test.Data.Tree
import Test.Data.Tree.Fan
import Test.Data.Tree.Spread

main = do
  Console.log "Tests for Data.Tree"
  testTree
  Console.log "Tests for Data.Tree.Fan"
  testFan
  Console.log "Tests for Data.Tree.Spread"
  testSpread
