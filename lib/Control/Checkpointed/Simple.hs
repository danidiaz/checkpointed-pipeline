{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Arrows #-}
module Control.Checkpointed.Simple (
    -- * Simple pipelines in IO
        Pipeline'
    ,   stage'
    ,   prepare'
    ,   unlift'
    ,   P.mapTag
    ,   P.contramapResource
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
import System.Directory

import Control.Checkpointed(Pipeline)
import qualified Control.Checkpointed as P

type Pipeline' tag b c = Pipeline tag FilePath (Kleisli IO) b c

stage' :: tag -- ^ Tag 
       -> (FilePath -> IO c) -- ^ Recover action
       -> (FilePath -> c -> IO ()) -- ^ Save action
       -> (b -> IO c) -- ^ Computation to perform
       -> Pipeline' tag b c
stage' atag arecover asaver acomputation = 
    P.stage atag
            (\filepath -> 
              do exists <- doesFileExist filepath
                 if exists
                 then return (Just (Kleisli (\() -> arecover filepath)))
                 else return Nothing)
            (Kleisli <$> asaver)
            (Kleisli acomputation)

prepare' :: (NonEmpty tag -> FilePath) -> Pipeline' tag () c -> IO c
prepare' f pipeline = join $ (($ ()) . runKleisli <$> P.prepare f pipeline)

unlift' :: Pipeline' tag b c -> b -> IO c 
unlift' = runKleisli . P.unlift

-- TODO add mapTag function.
