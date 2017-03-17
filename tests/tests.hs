{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE Arrows #-}

module Main where

import Prelude hiding ((.),id)
import Data.Monoid
import Data.IORef
import Data.Functor.Const
import Data.List.NonEmpty
import qualified Data.ByteString as Bytes

import Control.Category
import Control.Arrow
import Control.Monad

import System.Directory
import System.FilePath

import Test.Tasty
import Test.Tasty.HUnit (testCase,assertEqual,assertBool)

import Control.Checkpointed.Simple

testdir :: FilePath
testdir = "String"

testfolder :: String
testfolder = "/tmp/_384sd9f9_checkpointed_tests_"

withTestFolder :: (IO (NonEmpty String -> FilePath,a -> IO (),IO [a]) -> TestTree) -> TestTree
withTestFolder =
    withResource (do exists <- doesDirectoryExist testfolder
                     when exists $ removeDirectory testfolder
                     createDirectory testfolder
                     ref <- newIORef []
                     let makePath xs = testfolder </> join (toList (intersperse "_" xs))
                     return (makePath,\x -> modifyIORef ref (++[x]),readIORef ref))
                 (\_ -> return ())

appender :: Char -> Bytes.ByteString -> Pipeline' Char Bytes.ByteString Bytes.ByteString
appender what = undefined 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testCase "simple" testSimple
                          ]
testSimple :: IO ()
testSimple = do
    assertEqual "" [] ([]::[Int])

