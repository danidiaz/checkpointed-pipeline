{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Arrows #-}
module Control.Checkpointed (
        Pipeline
    ,   stage
    ,   prepare
    ,   unlift
    ,   Data.Semigroupoid.o
    ) where

import Prelude hiding ((.),id)
import Control.Category
import Control.Arrow
import Data.List.NonEmpty
import Data.Semigroup
import Data.Semigroupoid

data Pipeline t r a b c = 
    Pipeline
    {
      tag :: NonEmpty t 
    , recover :: (NonEmpty t -> r) -> IO (Maybe (a () c)) 
    , calculate :: a b c
    , calculate' :: (NonEmpty t -> r) -> a b c
    } 

stage :: Arrow a
      => t -- ^ Tag 
      -> (r -> IO (Maybe (a () c))) -- ^ Recover action
      -> (r -> a c ()) -- ^ Save action
      -> a b c -- ^ Computation to perform
      -> Pipeline t r a b c
stage atag arecover asaver acalculate = Pipeline
        {
            tag = pure atag
        ,   recover = \f -> arecover (f (pure atag))
        ,   calculate = acalculate
        ,   calculate' = \f -> 
                proc b -> do
                    c <- acalculate -< b
                    _ <- asaver (f (pure atag)) -< c
                    returnA -< c
        }

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

prepare :: (NonEmpty t -> r) -> Pipeline t r a () c -> IO (a () c)
prepare f (Pipeline {recover,calculate'}) = 
  do recovered <- recover f
     return (case recovered of
        Just p -> p
        Nothing -> calculate' f)

unlift :: Pipeline t r a b c -> a b c
unlift = calculate
    
instance Category c => Semigroupoid (Pipeline t r c) where
    (Pipeline tag2 recover2 calculate2 calculate2') `o` 
        (Pipeline tag1 recover1 calculate1 calculate1') =
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
                           Just a1 -> return $ Just $ calculate2' prepending . a1
                           Nothing -> return Nothing
        ,  calculate = calculate2 . calculate1
        ,  calculate' = \f -> 
               let prepending s = f (tag1 <> s)
               in calculate2' prepending . calculate1' f  
        }

