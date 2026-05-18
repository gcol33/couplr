## Release notes (1.4.0)

This is a minor release adding animation coverage for the remaining ten
`assignment()` methods (`auction_gs`, `ramshaw_tarjan`, `ssap_bucket`, `hk01`,
`csflow`, `cycle_cancel`, `push_relabel`, `csa`, `orlin`, `network_simplex`)
and fixing three correctness bugs:

* `prepare_cost_matrix.cpp` previously treated `+Inf` as a finite cost rather
  than a forbidden marker, which caused `assignment(method = X, maximize = TRUE)`
  to silently skip the maximize-flip and return the minimizing answer on any
  matrix containing `Inf`. Fixed: `NA` and any non-finite value are now marked
  forbidden consistently.

* `lap_solve_orlin()` and `lap_solve_network_simplex_wrapper()` used
  `work[is.na(work)] <- Inf`, which missed the `-Inf` produced by negating
  `+Inf` in maximize mode. Fixed: `work[!is.finite(work)] <- Inf`.

* The network-simplex initial spanning tree (`src/solvers/network_simplex/`
  `ns_init.h`) was built from a greedy matching that could leave rows
  unmatched, producing an infeasible starting basis that the pivot loop
  could not recover from. Fixed by adding an augmenting-path repair after
  the greedy pass.

A new parametric test (`tests/testthat/test-trace-parity.R`) exercises every
registered animation trace on a battery of cost matrices including forbidden
cells, verifying per-frame matching validity and final-frame agreement with
the C++ oracle.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: R-devel (Status: OK,
  https://win-builder.r-project.org/EO1CC0CS8iE5, 18 May 2026)
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
