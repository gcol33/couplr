## Release notes (1.7.0)

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
  solve peaks at 7 to 10 times the raw cell bytes, against the 4 the guard
  assumed, so it could start a solve on a machine it did not fit. The guard
  switches to the lazy path earlier than it did.

### Bug fixes

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

* The edge-generation loop reads its seed width off the number of columns
  instead of using a fixed five, which settles it in two rounds instead of four
  to seven on the sizes measured.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the incoming-feasibility one, reporting the number of updates in
the past six months. After 1.6.1 I said I would leave a longer gap, and this
submission is fourteen days after it. The release carries a correctness fix to
a result the package returned without saying it was wrong, and three breaking
changes, which is why it is a minor rather than a patch version.

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0 (0 errors, 0
  warnings, 1 note)
* win-builder: r-devel, r-release
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
