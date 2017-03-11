# checkpointed-pipeline

Do you have a computation that ends in `r`? Do you know how to read `r` values
from disk, and how to read them as well? 

You might want to chain a number of these computations to form a self-contained pipeline, and avoid uneccesary work when running the pipeline by recovering
from the latest existing checkpoint.

Idea suggested by [this Reddit thread](https://www.reddit.com/r/haskell/comments/5y06pz/caching_of_intermediate_data/).
