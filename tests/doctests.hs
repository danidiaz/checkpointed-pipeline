module Main where

import Test.DocTest

main :: IO ()
main = doctest ["lib/Control/Checkpointed.hs"]
