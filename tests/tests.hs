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
import Data.IORef
import Data.Functor.Const

import Control.Category
import Control.Arrow
import Control.Monad

import System.Directory

import Test.Tasty
import Test.Tasty.HUnit (testCase,assertEqual,assertBool)

import Control.Checkpointed.Simple

testdir :: FilePath
testdir = "String"

testfolder :: String
testfolder = "/tmp/_384sd9f9_checkpointed_tests_"

withTestFolder :: (IO (FilePath,a -> IO (),IO [a]) -> TestTree) -> TestTree
withTestFolder =
    withResource (do exists <- doesDirectoryExist testfolder
                     when exists $ removeDirectory testfolder
                     createDirectory testfolder
                     ref <- newIORef []
                     return (testfolder,\x -> modifyIORef ref (\xs -> xs ++ [x]),readIORef ref))
                 (\(folder,_,_) -> removeDirectory folder)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testCase "simple" testSimple
                          ]
testSimple :: IO ()
testSimple = do
    assertEqual "" [] ([]::[Int])

