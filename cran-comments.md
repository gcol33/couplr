## Release notes (1.6.0)

This release supersedes 1.5.5, the version currently on CRAN.

### New features

* `verify_assignment()` checks an assignment against the optimality conditions
  and reports which of them hold: primal feasibility, dual feasibility, and
  complementary slackness on both matched arcs and unmatched columns.
  `certified_optimal` is `TRUE` only when every one of them holds. Dual
  feasibility is tested over every admissible pair, so duals that certify
  nothing fail the check rather than pass it.

* `assignment()` gained a `cardinality` argument, with `"maximum"` and
  `"fixed"` alongside the previous behaviour, now named `"complete"`. All
  three are solved exactly by the same solver: dummy columns are appended,
  priced so that the solver's own optimum is the requested objective.

* `explain_dispatch()` reports which solver `method = "auto"` selects and why,
  without solving. The dispatch rules moved out of the branch chain in
  `assignment()` into one ordered table that both the dispatch and the report
  read. Dispatch decisions themselves are unchanged.

* `solver_status_values()` gives the closed set of values `status` can take. A
  status outside the set is now an error at the point a result is constructed.
  `match_couples()` and `full_match()` results carry a `status` from the same
  vocabulary.

### Bug fixes

* The min-cost flow search no longer runs forever on a cost matrix with many
  tied entries. The two directions of one residual arc are priced by
  expressions that are negatives of each other in exact arithmetic and not in
  floating point, so both could round below zero at once and form a
  negative-reduced-cost cycle for the search to circle. It was reachable from
  `match_couples()` with default arguments.

* `status` is computed from what the solver terminated on instead of being
  assigned as a literal. `network_simplex` now reports `"iteration_limit"`
  when the pivot cap ends the loop with an improving arc still available, and
  decides infeasibility from Hall's condition before the loop rather than from
  unmatched rows after it. `full_match()` no longer reports `"optimal"` for
  results that are not, and every unit now appears in exactly one of `groups`
  and `unmatched`.

* Constrained matching is optimal instead of greedy. When calipers,
  `max_distance` or explicit forbidden pairs left the admissible graph without
  a complete matching, `match_couples()` returned a greedy matching while still
  reporting the optimal solver that had been asked for. It now reaches the
  lexicographic optimum -- most admissible pairs first, then least total
  distance -- by replacing forbidden entries with a finite sentinel and
  dropping the pairs that come back on one.

### Documentation

* `?assignment`, `?verify_assignment` and `?explain_dispatch` state the
  dispatch rules and the certificate's conditions rather than summarising them.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: r-devel
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
