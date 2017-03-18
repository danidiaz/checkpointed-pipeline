{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Arrows #-}
{-| See the documentation for 'Control.Checkpointed.Simple'.
 
-}
module Control.Checkpointed (
    -- * General pipelines
        Pipeline
    ,   stage
    ,   transient
    ,   prepare
    ,   unlift
    ,   mapTag
    ,   contramapResource
    -- * Re-exports
    ,   Data.Semigroupoid.o
    ) where

import Prelude hiding ((.),id)
import Control.Category
import Control.Arrow
import Control.Monad
import Data.List.NonEmpty
import Data.Semigroup
import Data.Semigroupoid

{-| A computation in the arrow @a@ taking a value @b@ and producing a value @c@,
    identified with a tag type @tag@ and augmented with the ability to write and
    read checkpoints located by @r@.
-}
data Pipeline tag r a b c = 
    Pipeline
    {
      tag :: NonEmpty tag 
    , recover :: (NonEmpty tag -> r) -> IO (Maybe (a () c)) 
    , calculate :: a b c
    , calculatew :: (NonEmpty tag -> r) -> a b c
    } 

-- | Compose two compatible pipelines with 'o'.
instance Category c => Semigroupoid (Pipeline tag r c) where
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
        ,  calculatew = \f -> 
               let prepending s = f (tag1 <> s)
               in calculate2' prepending . calculate1' f  
        }

-- | Build a stage of a checkpointed pipeline.
stage :: Arrow a
      => tag -- ^ Tag identifying the stage
      -> (r -> IO (Maybe (a () c))) -- ^ Given a checkpoint location @r@, check if the checkpoint exists. If it exists, return an arrow @a@ that reads the checkpoint.
      -> (r -> a c ()) -- ^ Given a checkpoint location @r@, return an arrow @a@ that writes its input to the checkpoint.
      -> a b c -- ^ Computation to perform
      -> Pipeline tag r a b c
stage atag arecover asaver acalculate = Pipeline
        {
            tag = pure atag
        ,   recover = \f -> arecover (f (pure atag))
        ,   calculate = acalculate
        ,   calculatew = \f -> 
                proc b -> do
                    c <- acalculate -< b
                    _ <- asaver (f (pure atag)) -< c
                    returnA -< c
        }

-- | A special kind of stage that doesn't read or write checkpoints. 
transient :: Arrow a
          => tag -- ^ Tag 
          -> a b c -- ^ Computation to perform
          -> Pipeline tag r a b c
transient tag = stage tag (\_ -> return Nothing) (\_ -> arr (\_ -> ()))
        
-- | Given a function that builds checkpoint locations @r@ out of a list of
-- tags, return a self-contained checkpointed computation @a () c@.
prepare :: (NonEmpty tag -> r) -> Pipeline tag r a () c -> IO (a () c)
prepare f (Pipeline {recover,calculatew}) = 
  do recovered <- recover f
     return (case recovered of
        Just p -> p
        Nothing -> calculatew f)

-- | Return a computation @a b c@ that doesn't read or write
-- any checkpoint.
unlift :: Pipeline tag r a b c -> a b c
unlift = calculate
    
mapTag :: (tag -> tag') -> Pipeline tag r a b c -> Pipeline tag' r a b c 
mapTag retag (Pipeline {tag,recover,calculate,calculatew}) = 
    Pipeline { tag = retag <$> tag
             , recover = \f ->  recover (f . fmap retag)
             , calculate = calculate
             , calculatew =  \f ->  calculatew (f . fmap retag)
             }

contramapResource :: (r' -> r) -> Pipeline tag r a b c -> Pipeline tag r' a b c 
contramapResource rf (Pipeline {tag,recover,calculate,calculatew}) = 
    Pipeline { tag = tag
             , recover = \f ->  recover (rf . f)
             , calculate = calculate
             , calculatew =  \f ->  calculatew (rf . f)
             }
