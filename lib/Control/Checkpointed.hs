{-# LANGUAGE NamedFieldPuns #-}
module Control.Checkpointed where

import Data.List.NonEmpty
import Data.Semigroupoid

data Pipeline t r a i o = Pipeline
                        {
                          tag :: NonEmpty t 
                        , recover :: (NonEmpty t -> r) -> IO (Maybe (a () o)) 
                        , calculate :: (NonEmpty t -> r) -> a i o
                        }

preparePipeline :: (NonEmpty t -> r) -> Pipeline t r a () o -> IO (a () o)
preparePipeline f (Pipeline {tag,recover,calculate}) = 
  do recovery <- recover f
     return (case recovery of
        Just p -> p
        Nothing -> calculate f)
    

