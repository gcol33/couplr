## Release notes (1.7.1)

### Breaking changes

* `method = "orlin"` is now `method = "sap_dense"`. The C++ behind it runs
  successive shortest paths with a Johnson potential shift after each
  augmentation, with no scaling phase and no auction warm-up, so it is not the
  Orlin-Ahuja algorithm the old name named, and its `alpha` and
  `auction_rounds` arguments were never read. Calls passing `"orlin"` raise an
  error listing the valid methods. Results, duals and timings are unchanged.

* `method = "auto"` no longer diverts on sparsity or aspect ratio. Measured
  across a 189-cell grid of cost regimes, admissibility patterns and aspect
  ratios, neither rule beat the Jonker-Volgenant default in the regime it was
  written for. Both solvers stay reachable by name.

* `memory_mode = "auto"` sizes the solve rather than the cost matrix. A dense
  solve peaks at 7.2 to 10.5 times the raw cell bytes, against the 4 the guard
  assumed, so it could start a solve on a machine it did not fit. The guard
  switches to the lazy path earlier than it did.

### Bug fixes

* The ball-tree pricing bound now covers the cost source's own evaluation. The
  bound has to sit below the number the distance routine returns, since the
  reduced costs a prune is read against are built from it. The tree measures a
  sum of squares while the source evaluates a quadratic form by row sums, and
  the allowance charged nothing for the latter's rounding. The floor could sit
  5e-7 above a member's cost, about 500 times the default 1e-9 tolerance, so a
  node holding a genuine violator could be skipped with `certified_optimal`
  still `TRUE`.

* `full_match()` solved a narrower design than it documented. It fixed the
  group centres to the globally smaller side, so a matched set holding one unit
  of the larger side could not form. It is stated as an edge cover now and
  agrees with `optmatch::fullmatch()` on 80 random instances. Only
  `min_controls = 1` was affected.

* The dense-solve guard's multiplier now covers every peak it is read against.
  `estimate_dense_solve_mb()` defaulted `solve_factor` to 10 while a dense
  one-to-one solve peaked at 10.5, 7.2 and 8.8 times the raw cell bytes at
  5,000, 10,000 and 20,000 units, so at the first of those the estimate came in
  about 20 MB under the peak it exists to bound. The default is 12.

* `verify_assignment()` certified a matching that left rows unmatched under its
  default arithmetic. The numerical conclusion asked for primal feasibility, and
  the primal feasibility it asked for permitted an uncovered row, so on an input
  whose duals are all zero the two objectives agree at zero and a matching making
  no pairs came back certified. The row cover is part of primal feasibility in
  the model the package solves, and `primal_feasible` now means it, reporting its
  two halves as `structurally_valid_matching` and `all_rows_matched`. The exact
  arithmetic was unaffected.

* `method = "csa"` returned a suboptimal assignment on a wide cost range while
  reporting `status = "optimal"`. Its integer conversion scaled the largest
  absolute cost rather than the span, so costs clustered far from the origin lost
  their variation to the offset and a heavy-tailed matrix rounded its smallest
  entries together, after which the solver could not order the cheapest pairs.
  The conversion now shifts the smallest cost to zero and scales the span, and a
  range whose resolution cannot order those pairs is refused with `"jv"` and
  `"auction"` named instead of answered.

* Under `memory_mode = "lazy"` and `"implicit"` the reported distance for a
  matched pair was recomputed in R from a second copy of the metric's formula,
  agreeing with the solver's own evaluation to rounding and not to the last
  bit. A `max_distance` set at a distance the package had reported could
  therefore exclude the pair it was read from: on a Mahalanobis problem a
  caliper at the widest matched arc returned a complete matching on the dense
  path and none on the lazy and implicit paths. The reported distance now comes
  from the routine the solve priced the pair with, and the formula is written
  once.

### Improvements

* `verify_assignment()` decides its conditions in exact arithmetic. Every
  condition is the sign of `c_ij - u_i - v_j`, which has an exact answer, and
  the new `arithmetic` argument takes `"auto"`, `"exact"` and `"double"`.

* The certificate reports `max_suboptimality` and
  `certified_reduced_cost_floor`, so a caller reads what the returned answer can
  still be beaten by rather than only whether the check passed.

* `estimate_dense_matrix_mb()` and `estimate_dense_solve_mb()` are exported.
  Both were documented and reachable only through `:::`, while the memory-mode
  documentation and the guard itself are written around them.

* The edge-generation loop reads its seed width off the number of columns
  instead of using a fixed five, which settles it in two rounds instead of four
  to seven on the sizes measured.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the incoming-feasibility one, reporting seven updates in the past
six months. That is more than I would like. 1.6.1 was published on 2026-08-23
and I have held this submission back to keep the usual interval.

1.7.0 was tagged and never submitted. A critical review of that candidate
found two exported paths returning an answer to a different question than the
caller asked, so the version number moved rather than the tag.

What is in this one: two correctness fixes found in review. The ball-tree
pricing bound could report `certified_optimal = TRUE` for a matching that a
subtree it skipped would have improved, and `full_match()` solved a narrower
design than its documentation described. Both are wrong answers returned
without saying they were wrong. The three breaking changes are why this is a
minor rather than a patch version.

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: r-devel, r-release
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
