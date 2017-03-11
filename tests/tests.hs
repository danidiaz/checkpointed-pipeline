{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding ((.),id)
import Data.Monoid
import Data.Foldable
import Data.Tree
import Data.Functor.Const

import Control.Category
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Comonad

import Control.Checkpointed

import Test.Tasty
import Test.Tasty.HUnit (testCase,assertEqual,assertBool)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testCase "simple" testSimple
                          ]
testSimple :: IO ()
testSimple = do
    assertEqual "" [] []

