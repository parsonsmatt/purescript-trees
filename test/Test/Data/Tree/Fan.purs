module Test.Data.Tree.Fan (testFan) where

import Prelude

import Data.Tree.Fan

import Control.Monad.Eff.Console (log)

import Type.Proxy (Proxy(..), Proxy2(..))

import Test.QuickCheck.Laws.Control.Comonad (checkComonad)
import Test.QuickCheck.Laws.Control.Apply (checkApply)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Bind (checkBind)
import Test.QuickCheck.Laws.Control.Monad (checkMonad)
import Test.QuickCheck.Laws.Control.Extend (checkExtend)
import Test.QuickCheck.Laws.Data.Functor (checkFunctor)
import Test.QuickCheck.Laws.Data.Eq (checkEq)

proxy2 :: Proxy2 Fan
proxy2 = Proxy2

proxyN :: Proxy (Fan Number)
proxyN = Proxy

testFan = do
  log "Testing Laws for Instances"
  checkFunctor proxy2
  checkApply proxy2
  checkApplicative proxy2
  checkBind proxy2
  checkMonad proxy2
  checkExtend proxy2
  checkComonad proxy2
  checkEq proxyN
