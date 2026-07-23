## Release notes (1.5.2)

This is a performance and installation release. It follows 1.5.1 closely
because both items affect users of the released version.

`method = "ssap_bucket"` built Dial's bucket queue as a vector of vectors grown
to the largest distance in the shortest-path tree, so costs carrying six
decimals allocated on the order of 15 million bucket vectors per augmentation.
It is now the textbook circular ring, sized by the largest reduced edge cost,
which bounds the queue to O(maxC) rather than O(N * maxC) memory. Over 200
randomised solves at six decimals this takes 36.0 s down to 2.5 s. Accepted
inputs and returned optima are unchanged.

Four packages with no call site anywhere in the package, tests, vignettes or
scripts are dropped from `Suggests`: `OpenImageR`, `reticulate`, `xml2` and
`farver`.

`lap_solve_batch(n_threads = NULL)` sized its cluster from
`parallel::detectCores()`, which ignores `_R_CHECK_LIMIT_CORES_`. It now uses
two workers when that variable is set.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: r-devel
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
