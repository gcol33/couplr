## Release notes (1.8.0)

1.7.2 was tagged but never submitted; its changes are included here.

* `verify_assignment()` and `verify_flow()` decide their optimality
  conditions exactly on computed distances (Euclidean, Mahalanobis). When the
  solver's floating-point duals miss exact tightness by rounding, the
  potentials the matching determines are recovered in exact multi-component
  arithmetic, and an exact certificate returns them so it can be re-checked
  from the cost matrix alone.

* `memory_mode = "implicit"` certifies over every admissible pair by pricing
  omitted pairs at zero against exact potentials, instead of at a tolerance.

* `assignment()`, `match_couples()` and `cardinality_match()` return the
  optimal duals or potentials on every design with a linear program, so a
  certificate costs one pass over the pairs and no second solve.

* `cardinality_match()` certifies with no tolerance. Each node's bound is the
  dual objective of its Lagrangian relaxation evaluated exactly, and the
  pruning tests and constraint values behind a certificate are decided
  exactly.

* Exact potential recovery no longer refuses an optimal flow on networks with
  zero-cost arcs, which it did on some balance networks.

* `verify_assignment()` reads duals off a solve result by exact name rather
  than by partial matching.

No exported function is removed or renamed, and no hard dependency is
added. 'clue' and 'lpSolve' join Suggests: the tests check every solver
against their optima.

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the incoming-feasibility one reporting the number of recent
updates. 1.7.1 was published on 2026-09-16.

## Test environments

* local: Windows 11 x64, Rtools45 g++ 14.3.0. R 4.6.1: Status OK.
  R-devel (2026-09-29 r90598 ucrt): Status OK apart from math rendering
  in the HTML manual being skipped, as 'V8' is not installed there.
* win-builder was unavailable on 2026-09-30: every run, including one of
  a minimal test package, stopped at "checking CRAN incoming feasibility"
  after a few seconds. R-devel on Windows was therefore checked locally,
  at the revision win-builder runs.
* GitHub Actions at the release commit: macOS-latest (release),
  windows-latest (release), ubuntu-latest (devel, release, oldrel-1)

## Downstream dependencies

None.
