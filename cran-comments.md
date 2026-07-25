## Release notes (1.5.3)

This release supersedes 1.5.0, the version currently on CRAN. It fixes a
correctness bug and is submitted promptly for that reason.

`lap::CostMatrix` computed its flat row-major index as `i * ncol + j` in
32-bit `int` arithmetic, which silently overflows once `nrow * ncol` exceeds
`INT_MAX` (about a 46,341-square matrix) -- a scale the package's own
vignettes already use as a normal example. The same pattern was duplicated in
several solvers' own index arithmetic (`auction`, `csa`, `cycle_cancel`,
`munkres`, `ramshaw_tarjan`, `ssap_bucket`, `sap`/`ssp`, `network_simplex`)
and in the Rcpp boundary's matrix conversion. All flat-index arithmetic is
now computed in 64-bit, with a new arithmetic-only regression test covering
the exact overflow point without allocating an overflow-sized matrix.

Also included since 1.5.0:

* New `memory_mode = "lazy"` option for `match_couples()`, `compute_distances()`,
  and `assignment()`: computes each pairwise distance on demand instead of
  materializing the full cost matrix, with an `"auto"` mode that estimates
  memory footprint against free RAM before a large dense allocation.
* `method = "ssap_bucket"` is markedly faster on fine-grained fractional
  costs (36.0s to 2.5s over 200 randomised solves at six decimals); accepted
  inputs and returned optima are unchanged.
* `lap_solve_batch(n_threads = NULL)` now honours `_R_CHECK_LIMIT_CORES_`
  (two workers instead of `parallel::detectCores()`) in both the
  matrix-list and grouped-data-frame paths.
* `LinkingTo: RcppEigen` removed: it was declared but no Eigen type was ever
  instantiated in `src/`, and the unused declaration forced every source
  install to build RcppEigen first. `LinkingTo` is now `Rcpp` alone.
* `htmlwidgets` moved from `Imports` to `Suggests`, checked at call time by
  `lap_animate()`; this drops about 24 packages from a default install.
* `OpenImageR`, `reticulate`, `xml2`, and `farver` dropped from `Suggests`:
  no call site anywhere in the package, tests, vignettes, or scripts.
* Corrected memory-usage claims in the vignettes and `compute_distances()`
  docs for greedy matching and the `strategy = "pq"` candidate-pair search.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: r-devel
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
