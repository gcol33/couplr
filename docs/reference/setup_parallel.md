# Setup parallel processing with future

Setup parallel processing with future

## Usage

``` r
setup_parallel(parallel = FALSE, n_workers = NULL)
```

## Arguments

- parallel:

  Logical or plan specification

- n_workers:

  Number of workers (NULL for auto-detect)

## Value

List with original plan and whether we set up parallelization
