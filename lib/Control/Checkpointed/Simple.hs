{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Arrows #-}

{-|
    Define and run self-contained pipelines that write checkpoints at each
    stage, so they can be restarted from the latest checkpoint. 

    The pipelines in this module work in `IO` and write its checkpoints as
    files.  See `Control.Checkpointed` for more general pipelines.

    Here, "self-contained" means that the pipelines must begin with the
    uninformative @()@ value and read fixed data from the filesystem.
    Otherwise, it would now make sense to checkpoint them!

    Pipelines are composed with the 'o' operator from 'Data.Semigroupoid'. A
    semigroupoid is like a category in that you can compose things. However,
    there's no 'id'.

@
import qualified Data.ByteString as B

example :: Pipeline' String () B.ByteString
example =
      'stage' "c" B.readFile B.writeFile (\\b -> return (b <> "somesuffix"))
    `o` 'stage' "b" B.readFile B.writeFile (\\b -> return ("someprefix" <> b)) 
    `o` 'initial' "a" (B.readFile "initialdata.dat")
@

    If we execute this pipeline with

@
'prepare' (\\tags -> "\/tmp\/" ++ concat (toList tags)) example
@

    It will return the final result and create two intermediate checkpoint
    files, @\/tmp\/ab@ and @\/tmp\/abc@.

    If we re-run the pipeline, it will check if @\/tmp\/abc@ exists and read it,
    otherwise it will check for @\/tmp\/ab@ and start from there. If no
    checkpoint is found, it will start again from the beginning.
-}

module Control.Checkpointed.Simple (
    -- * Simple pipelines in IO
        Pipeline'
    ,   stage
    ,   transient
    ,   initial
    ,   prepare
    ,   unlift
    ,   P.mapTag
    -- * Re-exports
    ,   Data.Semigroupoid.o
    ) where

import Prelude hiding ((.),id)
import Control.Category
import Control.Arrow
import Control.Monad
import Data.List.NonEmpty
import Data.Semigroupoid
import System.Directory

import Control.Checkpointed(Pipeline)
import qualified Control.Checkpointed as P

type Pipeline' tag b c = Pipeline tag FilePath (Kleisli IO) b c

-- | Build a stage of a checkpointed pipeline.
stage :: tag -- ^ Tag identifying the stage
      -> (FilePath -> IO c) -- ^ Recover result from file
      -> (FilePath -> c -> IO ()) -- ^ Write result to file
      -> (b -> IO c) -- ^ Computation to perform
      -> Pipeline' tag b c
stage atag arecover asaver acomputation = 
    P.stage atag
            (\filepath -> 
              do exists <- doesFileExist filepath
                 if exists
                 then return (Just (Kleisli (\() -> arecover filepath)))
                 else return Nothing)
            (Kleisli <$> asaver)
            (Kleisli acomputation)

-- | A special kind of stage that doesn't read or write checkpoints. 
transient :: tag -- ^ Tag 
          -> (b -> IO c) -- ^ Computation to perform
          -> Pipeline' tag b c
transient tag k = P.transient tag (Kleisli k)

-- | Build the initial stage of a pipeline out of a simple `IO` action, for
-- example one that reads some fixed initial data from a file. The resulting
-- stage won't read or create any checkpoints.
initial :: tag -- ^ Tag 
        -> IO b
        -> Pipeline' tag () b
initial tag action = transient tag (\() -> action)

-- | Given a function that builds checkpoint 'FilePath's out of a list of tags,
-- return a self-contained checkpointed computation in `IO`.
prepare :: (NonEmpty tag -> FilePath) -> Pipeline' tag () c -> IO c
prepare f pipeline = join $ (($ ()) . runKleisli <$> P.prepare f pipeline)

-- | Return a computation that doesn't read or write
-- any checkpoint.
unlift :: Pipeline' tag b c -> b -> IO c 
unlift = runKleisli . P.unlift
