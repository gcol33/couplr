## Release notes (1.6.1)

This release supersedes 1.5.3, the version currently on CRAN. Versions 1.5.4,
1.5.5 and 1.6.0 were published on GitHub and never submitted, so their changes
are collected here as well.

### New features

* `memory_mode = "implicit"` solves an assignment by generating the pairs it
  needs, on `assignment()` and `match_couples()`. Every row starts with its
  nearest admissible partners, that sparse problem is solved by the flow model,
  and the pairs left out are priced against the duals it returns until none
  prices in. The duals then certify the sparse solution optimal for the
  complete problem, which is how the answer matches a dense solve on every
  problem small enough to run both.

* `match_path()` solves a matching per value of one argument as one sequence.
  `vary = "max_distance"` sweeps the distance cut over ascending values, each
  point resuming from the matching the point before it found. It returns a
  `couplr_path` carrying one row per point and the certificate for each.

* `cardinality_match()` maximizes matched cardinality subject to balance
  constraints and reports an optimality gap. Fine and refined covariate balance
  are represented in the matching network; moment constraints are dualized and
  searched by branch and bound under `node_limit` and `time_limit`.

* `method = "push_relabel"` runs cost-scaling push-relabel (Goldberg and
  Tarjan 1990) on the compiled flow problem. The value previously dispatched to
  successive shortest paths with Johnson potentials.

* `verify_assignment()`, `explain_dispatch()`, `solver_status_values()` and a
  `cardinality` argument on `assignment()` (from 1.6.0).

* `assignment_duals()` gained `certify` and reads a lazy cost specification.

### Bug fixes

* `method = "gabow_tarjan"` returns the optimum on rectangular problems, and
  takes its integer scale from the range the sentinel leaves. The previous
  conversion placed the largest magnitude at 1e6, so an instance carrying one
  large entry among small ones could come back above the optimum while
  reporting that it was optimal.

* Every reader drops a pair the optimum was forced onto a forbidden edge, so a
  cost at or above `BIG_COST` is read as no edge everywhere in the package.

* Every reader in the matching layer joins on the id column. Columns had been
  attached by row order in five places, two of which scrambled what they
  attached. Duplicated ids are rejected at extraction.

* `verify_flow()`'s per-arc tolerance scales with the numbers behind the
  reduced cost, so an exactly optimal flow no longer fails its own certificate
  on rounding.

* `time_limit` reaches the flow solver, which asks between augmentations and
  returns the new status `"interrupted"`. Ctrl+C raises an R interrupt
  condition from inside the solve.

* A matching's status is derived from what the design asked for, which covers
  the k:1 and with-replacement designs.

* `get_free_ram_mb()` reads the page size from `vm_stat` rather than assuming
  4096 bytes, so Apple Silicon no longer sees a quarter of its memory (1.5.4).

* `method = "auto"` selects a solver from one C++ pass over the cost matrix
  instead of several full-size temporaries (1.5.5). Dispatch decisions are
  unchanged.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the incoming-feasibility one, reporting seven updates in the past
six months. 1.5.3 is the last version that reached CRAN; 1.5.4, 1.5.5 and 1.6.0
went to GitHub only, and this submission collects them. It carries corrections
to results the package returned without saying they were wrong: matched data
assembled by row order rather than by id, `method = "gabow_tarjan"` returning a
matching above the optimum on rectangular problems and on wide cost ranges
while reporting it optimal, and a status of "optimal" on matchings that placed
a forbidden pair. I intend to leave a longer gap after this one.

`methods` has been dropped from Imports, which clears the unused-import note
1.5.3 carried.

## Test environments

* local: Windows 11 x64, R 4.6.0 ucrt, Rtools45 g++ 14.3.0
* win-builder: r-devel
* GitHub Actions: macOS-latest, windows-latest, ubuntu-latest
  (devel, release, oldrel-1)

## Downstream dependencies

None.
