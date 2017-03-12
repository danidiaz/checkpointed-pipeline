{-# LANGUAGE NamedFieldPuns #-}
module Control.Checkpointed (
        Pipeline
    ,   stage
    ,   prepare
    ) where

import Prelude hiding ((.),id)
import Control.Category
import Control.Arrow
import Data.List.NonEmpty
import Data.Profunctor
import Data.Semigroup
import Data.Semigroupoid

data Pipeline t r a b c = 
    Pipeline
    {
      tag :: NonEmpty t 
    , recover :: (NonEmpty t -> r) -> IO (Maybe (a () c)) 
    , calculate :: (NonEmpty t -> r) -> a b c
    } 

stage :: t 
      -> (r -> IO (Maybe (a () c)))
      -> (r -> a b c)
      -> Pipeline t r a b c
stage = undefined 
   
-- -- maybe the Functor and Profuctor instances shouldn't really be here...
-- instance Arrow c => Functor (Pipeline t r c i) where
--     fmap f (Pipeline tag' recover' calculate') =
--         Pipeline tag' 
--                  (fmap (fmap (fmap (flip (>>^) f))) recover')
--                  (fmap (flip (>>^) f) calculate')
-- 
-- instance Arrow c => Profunctor (Pipeline t r c) where
--     rmap = fmap
--     lmap f (Pipeline tag' recover' calculate') =
--         Pipeline tag' recover' (fmap ((^>>) f) calculate')

prepare :: (NonEmpty t -> r) -> Pipeline t r c () o -> IO (c () o)
prepare f (Pipeline {recover,calculate}) = 
  do recovery <- recover f
     return (case recovery of
        Just p -> p
        Nothing -> calculate f)
    
instance Category c => Semigroupoid (Pipeline t r c) where
    o (Pipeline tag2 recover2 calculate2) (Pipeline tag1 recover1 calculate1) =
        Pipeline 
        {
           tag = tag1 <> tag2
        ,  recover = \f ->
               do let prepending s = f (tag1 <> s)
                  r2 <- recover2 prepending
                  case r2 of
                     Just a2 -> return $ Just a2
                     Nothing -> do
                        r1 <- recover1 f
                        case r1 of 
                           Just a1 -> return $ Just $ calculate2 prepending . a1
                           Nothing -> return Nothing
        ,  calculate = \f -> 
               let prepending s = f (tag1 <> s)
               in calculate2 prepending . calculate1 f  
        }

