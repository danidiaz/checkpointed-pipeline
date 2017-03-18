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

withTestFolder :: (IO (NonEmpty String -> FilePath,a -> IO (),IO (),IO [a]) -> TestTree) -> TestTree
withTestFolder =
    withResource (do exists <- doesDirectoryExist testfolder
                     when exists $ removeDirectoryRecursive testfolder
                     createDirectory testfolder
                     ref <- newIORef []
                     let makePath xs = testfolder </> join (toList (intersperse "_" xs))
                     return (makePath,\x -> modifyIORef ref (++[x]),writeIORef ref [],readIORef ref))
                 (\_ -> return ())

appender :: String -> (String -> IO ()) -> Bytes.ByteString -> Pipeline' String Bytes.ByteString Bytes.ByteString
appender atag writea appended = 
    stage atag Bytes.readFile Bytes.writeFile (\b -> writea atag *> return (b <> appended))  

stage0 :: String -> (String -> IO ()) -> FilePath -> Pipeline' String () Bytes.ByteString
stage0 atag writea apath = initial atag (writea atag *> Bytes.readFile apath)

test0 :: IO (NonEmpty String -> FilePath,String -> IO (),IO (),IO [String]) -> TestTree
test0 resource = testCase "test0" $ do
    (toPath,awrite,clear,final) <- resource
    let initialFile = toPath (pure "initial")
        pipeline = 
              appender "c" awrite "c" 
          `o` appender "b" awrite "b" 
          `o` appender "a" awrite "a" 
          `o` stage0 "i" awrite initialFile
    writeFile initialFile "i"
    do result <- prepare toPath pipeline 
       resultw <- final
       assertEqual "" ("iabc",["i","a","b","c"]) (result,resultw)
       clear
    do result <- prepare toPath pipeline 
       resultw <- final
       assertEqual "" ("iabc",[]) (result,resultw)
       clear
    do removeFile (toPath ("i" :| ["a","b","c"]))
       result <- prepare toPath pipeline 
       resultw <- final
       assertEqual "" ("iabc",["c"]) (result,resultw)
       clear
    do removeFile (toPath ("i" :| ["a","b","c"]))
       removeFile (toPath ("i" :| ["a","b"]))
       result <- prepare toPath pipeline 
       resultw <- final
       assertEqual "" ("iabc",["b","c"]) (result,resultw)
       clear

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [withTestFolder test0
                          ]
testSimple :: IO ()
testSimple = do
    assertEqual "" [] ([]::[Int])

