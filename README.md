# checkpointed-pipeline

Do you have a computation that ends in `c`? Do you know how to read `c` values
from disk, and how to write them as well? 

You might want to chain a number of these computations to form a self-contained
pipeline, and avoid uneccesary work when running the pipeline by recovering
from the latest existing checkpoint.

Idea suggested by [this Reddit
thread](https://www.reddit.com/r/haskell/comments/5y06pz/caching_of_intermediate_data/).

## What does this library buy me?

Not much. 

It makes individual stages more portable between pipelines, because you don't
have to specify the full path to the stage checkpoint, only a tag identifying
the stage. The checkpoint paths are constructed when running the whole
pipeline.

This library doesn't give you any special functions to read or write
checkpoints, either.

## When NOT to use this library?

- You need streaming pipelines where several stages might be active at once.
- You work with large datasets that do not fit wholly in memory.
- You need non-linear pipelines with branching.
- You need dynamic pipelines that can reconfigure themselves dependign on
  intermediate results.

