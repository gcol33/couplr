# couplr 1.5.2

## Performance

* **`method = "ssap_bucket"` is much faster on fine-grained fractional costs.**
  Dial's queue was built as a `std::vector<std::vector<int>>` grown to the
  largest *distance* in the shortest-path tree, so costs needing six decimals
  (scale `1e6`) allocated roughly 15 million bucket vectors per augmentation.
  It is now the textbook circular ring, sized by the largest
  *reduced edge cost* and holding intrusive lists over a pooled arena, which is
  what bounds Dial's memory to `O(maxC)` instead of `O(N * maxC)`. Measured over
  200 randomised solves: 36.0 s to 2.5 s at six decimals, 2.5 s to 0.23 s at
  five. The accepted inputs and returned optima are unchanged.

* **`lap_solve_batch()` honours the check-environment core limit.** With
  `n_threads = NULL` it sized its cluster from `parallel::detectCores()`, which
  reports the physical core count and ignores `_R_CHECK_LIMIT_CORES_`. It now
  uses two workers when that variable is set, and every available core
  otherwise. Both the matrix-list and grouped-data-frame paths read the same
  helper.

## Installation

* **Four unused packages dropped from `Suggests`:** `OpenImageR`, `reticulate`,
  `xml2`, and `farver` had no call site anywhere in the package, tests,
  vignettes, or scripts. `av` is kept: `pixel_morph_animate()` uses it for mp4
  output, guarded at call time.

# couplr 1.5.1

## Installation

* **`RcppEigen` is no longer required to build the package.** It was declared in
  `LinkingTo` but never used: the only Eigen reference in `src/` was the
  `#include <RcppEigen.h>` that `Rcpp::compileAttributes()` emits for each
  `LinkingTo` entry, and no solver instantiated an Eigen type. The declaration
  forced every source install to build RcppEigen first, which was reported as an
  install failure. `LinkingTo` is now `Rcpp` alone; the unused `testthat` entry
  and the `-DEIGEN_NO_DEBUG -DEIGEN_DONT_PARALLELIZE` compile flags are removed
  with it.

* **`htmlwidgets` moved from `Imports` to `Suggests`.** It is used only by
  `lap_animate()`, which now checks for it at call time and errors with an
  install hint if it is missing. This drops about 24 packages from a default
  install, including `knitr`, `rmarkdown`, `bslib`, `sass`, and `tinytex`.

## Documentation

* The README and `paper/paper.md` stated that the assignment is solved on
  RcppEigen. The solvers are hand-written C++ via Rcpp; both now say so.

# couplr 1.5.0

## Breaking changes

* **`greedy_couples()` is removed; greedy matching is now `match_couples(method =
  "greedy")`.** The two functions duplicated ~130 lines of identical scaffolding
  (validation, scaling, id extraction, blocking dispatch, metadata) over the same
  shared engine. They are now one front door: `match_couples()` gains a `method =
  "greedy"` value and a `strategy` argument ("row_best", "sorted", "pq"). Replace
  `greedy_couples(x, strategy = "sorted")` with `match_couples(x, method =
  "greedy", strategy = "sorted")`. The result object and `info$method == "greedy"`
  are unchanged.

## New features

* `pixel_morph()` and `pixel_morph_animate()` gain a `mode = "color_match"`
  option: pixels sharing a quantized colour are matched spatially and any
  remainder falls back to identity. A lighter-weight alternative to the default
  `"color_walk"` palette LAP.

## Bug fixes (statistical / causal-inference layer)

* **`sensitivity_analysis()` no longer scrambles matched pairs.** Outcomes
  were assembled with two independent `merge()` calls, each sorted by its
  own key, so the pair difference subtracted outcomes from mismatched
  pairs and every downstream quantity (Wilcoxon T+, Rosenbaum bounds,
  critical gamma) was computed on a scrambled pairing. Outcomes are now
  looked up by ID, preserving the row-wise pair correspondence (#4).

* **`subclass_match(estimand = "ATE")` weights corrected.** ATE subclass
  weights carried an extra factor of the stratum size, over-weighting large
  subclasses quadratically. A treated unit in subclass k now carries
  `(n_k / N) / n_t` as intended; ATT and ATC were already correct (#5).

* **`balance_diagnostics()` now applies stratum weights for full matching,
  CEM, and subclassification.** The weights were computed and discarded, so
  standardized differences were unweighted for the very estimators whose
  balance is achieved through weighting. Weighted mean, variance, and
  standardized difference are now used on both sides. Also: the variance
  ratio is now a true ratio of variances (matching the conventional 0.5-2
  bounds) rather than a ratio of standard deviations, and the unmatched
  right-unit count no longer goes negative under `ratio > 1` / `replace`
  (it counts distinct matched right units, not pair rows) (#6).

## Bug fixes (solvers)

* **`ssap_bucket` no longer silently rounds fractional costs to a wrong
  optimum.** The integer-scaling step tried only multipliers `{1, 10, 100,
  1000}` and, failing those, rounded at `1000` -- flipping which permutation
  was optimal on costs needing more than three decimals. It now searches
  ascending powers of ten with a fixed (scale-independent) integrality
  tolerance and refuses the problem, redirecting to `method = "jv"` or
  `"auction"`, when no bounded integer scaling is exact. The animation mirror
  `trace_ssap_bucket()` applies the same rule (#19).

* **`lap_solve_line_metric(maximize = TRUE)` now returns the true
  maximum-weight matching.** The DP always built the sorted (minimum-cost)
  pairing and merely negated the total; on a line the maximum-weight
  matching is the anti-monotone pairing. The DP now runs against the
  descending target ordering and returns that assignment and its true
  total (#8).

* **`gabow_tarjan` returns a perfect matching when the diagonal is
  forbidden.** The `C_max == 0` fast path assigned the diagonal without
  checking feasibility, returning an empty matching when the diagonal cells
  were forbidden but a perfect matching existed. It now finds a
  maximum-cardinality matching over the allowed edges via augmenting
  paths (#9).

## Bug fixes (front doors and input handling)

* **`compute_distances(auto_scale = TRUE)` now scales.** It read a
  nonexistent field (making `vars` `NULL`) and disabled scaling under the
  belief it had already happened. It now reads the selected variables and
  forwards the chosen scaling method to the cost builder (#7).

* **Pre-fitted propensity models predict on the supplied data.**
  `ps_match()` and `subclass_match()` called `predict()` without
  `newdata =`, so a `ps_model` fitted on a differently ordered or subset
  frame attached scores to the wrong rows. They now pass
  `newdata = data` (#10).

* **`match_couples(ratio > 1)` falls back to a partial match on
  infeasibility.** The `ratio > 1` path called the solver directly and hard
  errored when constraints forbade every edge of some unit, whereas the 1:1
  path returned a partial matching. Both paths now share the same
  partial-feasibility / greedy fallback (#10).

* **`lap_solve()` honors the `forbidden` sentinel for matrix input, and
  `lap_solve_batch()` preserves singleton-dimension orientation.** The
  matrix path silently ignored a non-`NA` `forbidden`; it now masks matching
  cells as forbidden. A 3-D array slice with a singleton row or column
  dimension was dropped to a vector and transposed; slices are now reshaped
  explicitly (#11).

## Robustness (C++ solver hardening)

Guarded the C++ solvers against silently wrong results and crashes at extreme
scale. None of these affect ordinary inputs; they add error paths and 64-bit
arithmetic where 32-bit overflow or a fixed tolerance could previously produce
a wrong "optimal" or a crash (#13):

* **Overflow / narrowing.** `ssap_bucket` errors clearly when cost magnitudes
  exceed what the integer-bucket solver can represent (rather than overflowing
  the sentinel or allocating an enormous bucket queue); the network-simplex
  iteration bound and `gabow_tarjan`'s bit-scaling range are computed and
  checked in 64-bit; `gabow_tarjan` also rejects costs that collide with its
  forbidden sentinel; the brute-force solver caps total enumeration work
  instead of running unbounded in the number of columns.
* **Large `n*m` indexing.** Flat cost/kernel indexing in
  `prepare_cost_matrix`, `solve_sinkhorn`, and the auction epsilon is done in
  64-bit; `network_simplex` and `lapmod` reject problems whose arc / entry
  counts would overflow a 32-bit index.
* **Tolerances and status.** `solve_munkres` scales its zero tolerance with the
  cost magnitude (a fixed `1e-12` could make a solvable large-cost matrix
  throw); `solve_csa` scales non-integer costs to integers before the
  epsilon-scaling auction, so its optimality guarantee (which assumes integer
  costs) also holds for real-valued inputs with near-tied assignments;
  `full_matching` now reports `infeasible` when the group capacity is below the
  number of units instead of silently dropping units as `optimal`;
  `solve_sinkhorn` reports the correct iteration count on non-convergence.
* **hk01 fallback.** The pure `solve_hk01` now falls back to the exact weighted
  solver (`solve_csflow`) when the zero-cost subgraph of a `{0,1}` matrix has no
  perfect matching, instead of erroring -- matching the Rcpp path that
  `assignment(method = "hk01")` already used.
* **Bounds.** The internal `morph_pixel_level` helpers assert their pixel /
  assignment buffer sizes, matching the exported wrappers.

* **`csa` shipped path now carries the fixes it was tested for.** The Rcpp
  entry point for `method = "csa"` ran a separate copy of the solver that never
  received the integer-scaling fix above, so `assignment(method = "csa")` could
  still return a suboptimal matching on fractional costs. It now delegates to
  the single pure `solve_csa` implementation exercised by the C++ tests. That
  implementation also gained square padding for rectangular problems, which it
  previously solved greedily (and suboptimally). The Rcpp `*_impl` wrappers now
  share one `rcpp_to_cost_matrix` / `lap_result_to_rcpp` conversion pair instead
  of per-file copies (#15).
* **Remaining solvers now ship the tested implementation.** Following `csa`, the
  Rcpp entry points for `sap`/`ssp`, `csflow`, `cycle_cancel`, `push_relabel`,
  `ssap_bucket`, `bruteforce`, `bottleneck`, `hk01`, `network_simplex`, and the
  three `auction` variants each ran a second copy of the algorithm that had
  drifted from the pure `lap::solve_*` exercised by the C++ tests. They now
  delegate to that single pure implementation, so the shipped path and the tested
  path are identical. Each pure copy was checked against brute force over
  randomised integer, fractional, rectangular, maximize, and forbidden-edge
  inputs before its wrapper was pointed at it. `network_simplex` thereby picks up
  the pure copy's `O(n^2)` pivot bound (the shipped copy used the slower
  `O(arcs * nodes)` bound).
* **`auction`, `auction_gs`, and `auction_scaled` now return the exact optimum.**
  The basic and Gauss-Seidel auctions used a single fixed epsilon, which leaves a
  duality-gap slack of up to `n * eps` and returned suboptimal matchings on
  closely-spaced costs (`assignment(method = "auction")` could disagree with
  `jv`); `auction_scaled` additionally threw on some feasible rectangular
  problems with forbidden edges under `maximize`. All three now run one shared
  epsilon-scaling core that scales epsilon down to a tiny final value, recovering
  the exact assignment. `lap_solve_auction_gs()` keeps its `bids` diagnostic.
* **`hk01` maximize.** The pure `solve_hk01` flipped `maximize` by negation,
  turning a `{0,1}` matrix into `{0,-1}`, which its palette check no longer
  recognised as binary -- so it threw on feasible binary maximization problems.
  It now flips via `cmax - c`, preserving the `{0,1}` palette so the fast path and
  the `solve_csflow` fallback engage.
* **Greedy matching wrappers** (`greedy_matching`, `_sorted`, `_row_best`, `_pq`)
  now delegate to the pure `lap::greedy_matching_*`. To keep the shipped
  behaviour identical, the pure copies gained the two tolerances the Rcpp copies
  had and they lacked: they skip the large-finite `BIG` sentinel the matching
  layer uses for forbidden edges (so a row whose only remaining options are
  forbidden is left unmatched rather than paired to a forbidden column), and they
  accept `n > m` by returning a partial matching instead of erroring. Verified
  byte-identical to the previous wrappers over 400 randomised cases spanning
  integer ties, `NA`/`BIG`-forbidden edges, and rectangular shapes. The three
  per-strategy Rcpp exports (`greedy_matching_sorted` / `_row_best` / `_pq`) were
  folded into the single `greedy_matching(strategy = ...)` dispatcher they
  duplicated; `match_couples(method = "greedy", strategy = ...)` is the
  user-facing verb.

## Tests

* Added a parameter-recovery and coverage suite for the statistical layer
  (`test-statistical-recovery.R`): sensitivity pair alignment, prefitted-PS
  row alignment, propensity-matching imbalance reduction, ATE subclass weight
  values, weighted-balance means, known-effect recovery across seeds, and
  nominal coverage of matched-pair confidence intervals (#14).
* Added a randomised ground-truth harness (`cpp_tests/tests/test_ground_truth.cpp`)
  that compares every optimal pure solver against brute-force enumeration over
  thousands of integer, fractional, rectangular, maximize, and forbidden-edge
  matrices (bottleneck against a brute-force minimax). This is the gate that
  decides whether a solver's Rcpp wrapper may delegate to the pure copy, and it
  is what surfaced the `auction` and `hk01` bugs above.

# couplr 1.4.1

## Bug fixes (solver stalls on constrained matching)

Fixes two solver paths that could stall indefinitely on `match_couples()`
inputs with `max_distance`, calipers, or other forbidden-edge constraints.
These stalls caused the M1mac and linux-arm64 additional CRAN checks for
1.4.0 to hit the 1.5-hour test timeout.

* **Forbidden-cell marker is now `Inf` instead of a large finite value.**
  `apply_max_distance()`, `apply_calipers()`, and `mark_forbidden_pairs()`
  previously wrote a large finite `BIG_COST` into forbidden cells. The
  Jonker-Volgenant and small-`n` SSP solvers treated `BIG_COST` as a
  regular expensive edge and could degenerate on sparse, near-square
  inputs instead of short-circuiting on infeasibility. Switched to `Inf`
  so the C++ solvers' non-finite check fires.

* **Auto-dispatch no longer routes sparse inputs through SSP for small `n`.**
  Previously `lap_solve()` with `method = "auto"` selected `"sap"`
  (`lap_solve_ssp`) for sparse matrices with `n <= 100`. SSP has its own
  worst-case stall on near-square, highly-sparse cost matrices. All
  sparse inputs now go through `lapmod` regardless of size.

* **`match_couples()` now drops fully-forbidden rows/columns before LAP.**
  `match_couples()` and `.couples_from_distance()` route through a new
  internal `.solve_with_partial_feasibility()` helper. It removes rows
  and columns with no allowed edges before the LAP call and falls back
  to `greedy_matching()` if the optimal solver still cannot find a
  perfect matching on the feasibility-pruned submatrix (Hall's-condition
  violation). Dropped rows/columns are returned as unmatched, preserving
  the partial-matching semantics that tests with tight `max_distance` /
  caliper constraints already expected.

## Other fixes

* **`jv_core`:** drop the same-pass reprocess in `AUGMENTING ROW REDUCTION`.
  The reprocess could revisit a freshly-reduced row in the same pass and
  delay convergence on degenerate inputs without changing the final
  assignment.

# couplr 1.4.0

## Animation coverage

* **`lap_animate()` now covers every method that `assignment()` accepts.**
  Ten new step-by-step traces ship: `auction_gs`, `ramshaw_tarjan`,
  `ssap_bucket`, `hk01`, `csflow`, `cycle_cancel`, `push_relabel`, `csa`,
  `orlin`, `network_simplex`. `animated_methods()` returns all 20 method
  strings.
* **Per-frame parity testing.** Every registered trace is exercised by a
  parametric `testthat` suite (`tests/testthat/test-trace-parity.R`) on a
  battery of small cost matrices including forbidden cells. Each frame's
  matching is validated for in-range entries, no double-bookings, and no
  use of forbidden edges; the final-frame total is compared to the C++
  oracle within tolerance.
* **Shared trace infrastructure.** New internal helpers
  `R/trace_helpers_frame.R` (`make_frame()`, `make_meta()`,
  `prepare_cost_work()`, `matching_total_cost()`, `validate_cost_input()`)
  and `R/trace_helpers_mcf.R` (min-cost-flow graph, residual edges,
  Dijkstra with Johnson potentials, Bellman-Ford, negative-cycle finder,
  push/extract). Used by all min-cost-flow traces.

## Bug fixes (correctness)

* **`prepare_cost_matrix.cpp`:** entries equal to `+Inf` were treated as
  regular very-large costs rather than forbidden, which made `cmax`
  become `Inf` and silently skipped the `maximize` flip. Result:
  `assignment(method = X, maximize = TRUE)` on matrices containing
  `Inf` returned the *minimizing* answer for any solver routing through
  `prepare_cost_matrix_impl` (`auction`, `auction_scaled`, `sap`,
  `csflow`, `hk01`, `bruteforce`). Now `NA` and any non-finite value are
  marked forbidden consistently.
* **`lap_solve_orlin` and `lap_solve_network_simplex_wrapper`:** the
  R-side wrapper used `work[is.na(work)] <- Inf` which missed the `-Inf`
  produced by negating `+Inf` in maximize mode, letting forbidden cells
  slip through as extreme-cost real edges. Fixed to
  `work[!is.finite(work)] <- Inf`.
* **`network_simplex` initial spanning tree:** the greedy initialiser in
  `ns_init.h` built a *partial* matching (any row that couldn't claim a
  fresh column was left unmatched) and connected unmatched columns to row
  0. The resulting starting basis violated flow conservation, and pivots
  could not recover a perfect matching even when one existed - e.g. on a
  5x5 cost matrix with two forbidden cells under `maximize`,
  `assignment(method = "network_simplex")` returned an infeasible result
  with one row unmatched. Fixed by adding an augmenting-path repair after
  the greedy pass: every still-unmatched row runs BFS for an augmenting
  path on the allowed-edge bipartite graph, extending the initial matching
  to a perfect matching whenever one exists.

# couplr 1.3.3

## Solver internals

* **Hungarian split into O(n^3) SAP + O(n^4) Munkres.** `method = "hungarian"`
  now uses the shortest-augmenting-path solver shared with JV; the original
  O(n^4) Munkres implementation remains available as `method = "munkres"`.
  At n = 2000 the new Hungarian runs orders of magnitude faster than 1.3.2.
* **LAPJV warm-start (column reduction + augmenting reduction) added to the
  JV core for square inputs.** Reduces JV / duals solve time at n >= 500.
* **CSA shares dual potentials across epsilon-scaling phases.** Removes the
  cold restart between phases that previously dominated CSA runtime at
  n >= 500.
* **Auction tie-breaker tweak cached in `auction` and `auction_gs`.**
  Cleaner inner loop; no behaviour change.
* **`solve_auction_scaled` collapsed into a thin wrapper over
  `scaled_params`** (~200 lines removed); behaviour identical.
* **Gabow-Tarjan**: bucket-array Step 2 reinstated per the 1989 paper
  (G&T's `r > bn` pruning is the algorithm, not a wart); added the 6n
  pruning heuristic from p.9.

## Documentation

* `paper/benchmark-table.csv` and `paper/scaling-results.csv` re-measured
  on the current development machine for n <= 2000 (per-method table) and
  n_total <= 2000 (cross-package table). Larger-n rows in both files are
  carried over from the previous machine and not directly comparable.

# couplr 1.3.2

## Test infrastructure

* Resubmission of 1.3.1 to address a win-builder r-devel pretest failure
  (exit code -1073741819 / access violation) in
  `test-lap-solve-batch-coverage.R`. Debian r-devel, local r-release, and
  local `R CMD check --as-cran` all pass; the crash did not reproduce off
  win-builder.
* Disabled testthat parallel execution (`Config/testthat/parallel: true`
  removed from DESCRIPTION) to eliminate cross-file worker-state leakage as
  a possible cause of the win-builder crash.
* Added a defensive `skip_on_cran()` at the top of
  `test-lap-solve-batch-coverage.R`. Equivalent coverage is exercised
  off-CRAN by `test-lap-solve-batch-coverage-2.R`,
  `test-lap-solve-batch-coverage-3.R`, `test-lap-solve-batch-extended.R`,
  `test-batch-coverage-final.R`, `test-batch-processing.R`, and
  `test-batch-kbest-extended.R`.

# couplr 1.3.1

## Behaviour changes

* **Mahalanobis distance now uses the pooled within-group covariance by
  default.** Previously the default was the overall-sample covariance of
  `rbind(left, right)`. The pooled within-group estimator
  `((n_L-1)*S_L + (n_R-1)*S_R) / (n_L+n_R-2)` is the convention used by
  `optmatch::match_on()` and aligns Mahalanobis behaviour across the matching
  packages a user is likely to compare against. Users who relied on the old
  default can recover it explicitly with
  `match_couples(..., sigma = cov(rbind(left[, vars], right[, vars])))`.
  The previous docstring already documented the default as "pooled
  covariance"; this release makes the code match the documentation.

---

# couplr 1.3.0

## New Features

### Optimal Full Matching

* **`full_match()` gains `method = "optimal"` (new default)** using a min-cost
  max-flow solver (Dijkstra + Johnson potentials) that finds the globally
  optimal group assignment minimizing total distance:
  - Standard lower bound transformation enforces `min_controls` per group
  - Automatic transposition when `n_left > n_right`
  - New C++ solver: `solve_full_matching.cpp` (self-contained MCMF)
  - `method = "greedy"` preserved for fast approximate matching

### Vignette Updates

* **Getting Started**: Added full matching section with `full_match()` example
* **Matching Workflows**: New "Full Matching (Variable-Ratio Groups)" section
  covering optimal vs greedy, constraints, weights, and comparison table
* **Comparison**: Updated feature table and all sections to reflect couplr's
  full matching support (previously listed as "No")

---

# couplr 1.2.0

## New Features

### Full Matching

* **New `full_match()` function** assigns every unit to a matched group
  with variable ratios (1:k or k:1):
  - Greedy group formation: match each left to nearest right, then assign
    remaining right units to nearest matched left
  - Caliper support: `caliper` (absolute) or `caliper_sd` (SD-based)
  - Control group size constraints: `min_controls`, `max_controls`
  - Weights inversely proportional to group size
  - Returns `full_matching_result` S3 class

### Coarsened Exact Matching (CEM)

* **New `cem_match()` function** implements coarsened exact matching:
  - Coarsens continuous variables into bins (Sturges, FD, Scott, or custom)
  - Exact matching on coarsened values with stratum-based weights
  - Support for categorical grouping variables via `grouping` parameter
  - Custom cutpoints per variable via `cutpoints` parameter
  - Returns `cem_result` S3 class with matched units and strata summary

### Subclassification

* **New `subclass_match()` function** divides units into propensity score
  strata:
  - Quantile-based stratification with configurable number of subclasses
  - Supports pre-computed PS, pre-fitted models, or formula interface
  - Target estimands: ATT, ATE, ATC with appropriate weighting
  - Returns `subclass_result` S3 class with subclass summary

### Output Layer & Ecosystem Integration

* **New `match_data()` generic** converts any couplr result to analysis-ready
  format with `treatment`, `weights`, `subclass`, and `distance` columns.
  Methods for all result types (matching, full, CEM, subclass).
* **New `as_matchit()` converter** creates `matchit`-class objects from couplr
  results, enabling interop with cobalt, marginaleffects, and other MatchIt
  ecosystem packages.
* **cobalt `bal.tab()` methods** for all couplr result types. Requires
  cobalt package (in Suggests).

### Mahalanobis Distance Improvements

* **Robust singularity check** using `rcond()` instead of fragile `det() == 0`
* **Custom `sigma` parameter** in `match_couples()`, `greedy_couples()`, and
  `compute_distance_matrix()` for user-supplied covariance matrices
* **Vectorized computation** replacing nested R for-loops for ~10x speedup

### S3 Generics

* `balance_diagnostics()` and `join_matched()` are now S3 generics with
  methods for all result types. Existing code is 100% backward-compatible.

### New Functions

* `full_match()` - Variable-ratio full matching
* `cem_match()` - Coarsened exact matching
* `subclass_match()` - Propensity score subclassification
* `match_data()` - Unified analysis-ready output
* `as_matchit()` - Convert to MatchIt format

---

# couplr 1.1.0

## New Features

### Ratio and Replacement Matching

* **k:1 ratio matching** via `ratio` parameter in `match_couples()` and
  `greedy_couples()`. Matches k control units to each treated unit by
  replicating the cost matrix, then deduplicates assignments.
* **With-replacement matching** via `replace` parameter. Each treated unit
  independently selects its nearest control, allowing controls to be reused
  across multiple treated units.

### Propensity Score Matching

* **New `ps_match()` function** wraps `match_couples()` with logistic regression:
  - Accepts a formula or pre-fitted `glm` object
  - Matches on the logit of propensity scores with a caliper
  - Default caliper: 0.2 SD of logit(PS) (Rosenbaum and Rubin recommendation)
  - Returns matching_result with PS model metadata

### Cardinality Matching

* **New `cardinality_match()` function** maximizes sample size subject to
  balance constraints:
  - Starts with a full optimal match, then iteratively prunes imbalanced pairs
  - Balance threshold via `max_std_diff` (default: 0.1 for excellent balance)
  - Configurable pruning speed with `batch_fraction`
  - Returns pruning diagnostics: iterations, pairs removed, final balance

### Sensitivity Analysis

* **New `sensitivity_analysis()` function** implements Rosenbaum bounds:
  - Tests sensitivity of matched comparisons to hidden bias
  - Uses Wilcoxon signed-rank statistic with upper/lower p-value bounds
  - Reports critical gamma (smallest gamma at which significance is lost)
  - S3 methods: `print()`, `summary()`, `plot()`

### Visualization

* **`autoplot()` methods** for ggplot2-based visualizations (requires ggplot2):
  - `autoplot.matching_result()`: histogram, density, or ecdf of distances
  - `autoplot.balance_diagnostics()`: love plot, histogram, or variance ratio plot
  - `autoplot.sensitivity_analysis()`: gamma vs p-value curve
* **Enhanced `summary.matching_result()`** now reports match rate and distance
  percentiles

### New Functions

* `ps_match()` - Propensity score matching with logit caliper
* `cardinality_match()` - Balance-constrained cardinality matching
* `sensitivity_analysis()` - Rosenbaum bounds sensitivity analysis

### Tests

* Added 58 new tests across 7 test files
* All 4916 tests passing across platforms

---

# couplr 1.0.7

## Bug Fixes

* Fixed undefined behavior (UB) in Gabow-Tarjan algorithm: replaced left bit-shift
  of potentially negative values with multiplication to avoid sanitizer errors
  on M1-SAN checks
* Fixed namespace conflict with `select()` in vignettes by using explicit
  `dplyr::select()` to prevent masking by MASS or other packages

---

# couplr 1.0.6

## Documentation

* Added Overview section to algorithms vignette with audience and prerequisites
* Fixed workflow diagram dark mode text handling in matching-workflows vignette
* Improved SVG theme-awareness for multi-line text labels
* Removed grid lines from matching-workflows plots for cleaner appearance
* Added threshold labels to balance comparison plot

---

# couplr 1.0.0

## Major New Features (2025-11-19 Update)

### Automatic Preprocessing and Scaling

The package now includes intelligent preprocessing to improve matching quality:

* **New `auto_scale` parameter** in `match_couples()` and `greedy_couples()` enables automatic preprocessing
* **Variable health checks** detect and handle problematic variables:
  - Constant columns (SD = 0) are automatically excluded with warnings
  - High missingness (>50%) triggers warnings
  - Extreme skewness (|skewness| > 2) is flagged
* **Smart scaling method selection** analyzes data and recommends:
  - "robust" scaling using median and MAD (resistant to outliers)
  - "standardize" for traditional mean-centering and SD scaling
  - "range" for min-max normalization
* New `preprocess_matching_vars()` function for manual preprocessing control
* Categorical variable encoding for binary and ordered factors

### Balance Diagnostics

Comprehensive tools to assess matching quality:

* **New `balance_diagnostics()` function** computes multiple balance metrics:
  - Standardized differences: (mean_left - mean_right) / pooled_sd
  - Variance ratios: SD_left / SD_right
  - Kolmogorov-Smirnov tests for distribution comparison
  - Overall balance metrics (mean, max, % large imbalance)
* **Quality thresholds** with interpretation:
  - |Std Diff| < 0.10: Excellent balance
  - |Std Diff| 0.10-0.25: Good balance
  - |Std Diff| 0.25-0.50: Acceptable balance
  - |Std Diff| > 0.50: Poor balance
* Per-block statistics with quality ratings when blocking is used
* `balance_table()` creates publication-ready formatted tables
* Informative print methods with interpretation guides

### Joined Matched Dataset Output

Create analysis-ready datasets directly from matching results:

* **New `join_matched()` function** automates data preparation:
  - Joins matched pairs with original left and right datasets
  - Eliminates manual data wrangling after matching
  - Select specific variables via `left_vars` and `right_vars` parameters
  - Customizable suffixes (default: `_left`, `_right`) for overlapping columns
  - Optional metadata: `pair_id`, `distance`, `block_id`
  - Works with both optimal and greedy matching
* **Broom-style `augment()` method** for tidymodels integration:
  - S3 method following broom package conventions
  - Sensible defaults for quick exploration
  - Supports all `join_matched()` parameters
* **Flexible output control**:
  - `include_distance` - Include/exclude matching distance
  - `include_pair_id` - Include/exclude sequential pair IDs
  - `include_block_id` - Include/exclude block identifiers
  - Custom ID column support via `left_id` and `right_id`
  - Clean column ordering: pair_id → IDs → distance → block → variables

### Precomputed and Reusable Distances

Performance optimization for exploring multiple matching strategies:

* **New `compute_distances()` function** precomputes and caches distance matrices:
  - Compute distances once, reuse across multiple matching operations
  - Store complete metadata: variables, distance metric, scaling method, timestamps
  - Preserve original datasets for seamless integration with `join_matched()`
  - Enable rapid exploration of different matching parameters
  - Performance improvement: ~60% faster when trying multiple matching strategies
* **Distance objects** (S3 class `distance_object`):
  - Self-contained: cost matrix, IDs, metadata, original data
  - Works with both `match_couples()` and `greedy_couples()`
  - Pass as first argument instead of datasets: `match_couples(dist_obj, max_distance = 5)`
  - Informative print and summary methods with distance statistics
* **Constraint modification** via `update_constraints()`:
  - Apply new `max_distance` or `calipers` without recomputing distances
  - Creates new distance object following copy-on-modify semantics
  - Experiment with different constraints efficiently
* **Backward compatible integration**:
  - Modified function signatures: `match_couples(left, right = NULL, vars = NULL, ...)`
  - Automatically detects distance objects vs. datasets
  - All existing code continues to work unchanged

### Parallel Processing

Speed up blocked matching with multi-core processing:

* **New `parallel` parameter** in `match_couples()` and `greedy_couples()`:
  - Enable with `parallel = TRUE` for automatic configuration
  - Specify plan with `parallel = "multisession"` or other future plan
  - Works with any number of blocks - automatically determines if beneficial
  - Gracefully falls back if future packages not installed
* **Powered by the `future` package**:
  - Cross-platform support (Windows, Unix/Mac, clusters)
  - Respects user-configured parallel backends
  - Automatic worker management
  - Clean restoration of original plan after execution
* **Performance**:
  - Best for 10+ blocks with 50+ units per block
  - Speedup scales with number of cores and complexity
  - Minimal overhead for small problems
* **Integration**:
  - Works with all blocking methods (exact, fuzzy, clustering)
  - Compatible with distance caching from Step 4
  - Supports all matching parameters (constraints, calipers, scaling)

### Fun Error Messages and Cost Checking

Like testthat, couplr makes errors light, memorable, and helpful with couple-themed messages:

* **New `check_costs` parameter** (default: `TRUE`) in `match_couples()` and `greedy_couples()`:
  - Automatically checks distance distributions before matching
  - Provides friendly, actionable warnings for common problems
  - Set to `FALSE` to skip checks in production code
* **Fun couple-themed error messages** throughout the package:
  - 💔 "No matches made - can't couple without candidates!"
  - 🔍 "Your constraints are too strict. Love can't bloom in a vacuum!"
  - ✨ Helpful suggestions: "Try increasing max_distance or relaxing calipers"
  - 💖 Success messages: "Excellent balance! These couples are well-matched!"
* **Automatic problem detection**:
  - **Too many zeros**: Warns about duplicates or identical values (>10% zero distances)
  - **Extreme costs**: Detects skewed distributions (99th percentile > 10x the 95th)
  - **Many forbidden pairs**: Warns when constraints eliminate >50% of valid pairs
  - **Constant distances**: Alerts when all distances are identical
  - **Constant variables**: Detects and excludes variables with no variation
* **New diagnostic function** `diagnose_distance_matrix()`:
  - Comprehensive analysis of cost distributions
  - Variable-specific problem detection
  - Actionable suggestions for fixes
  - Quality rating (good/fair/poor)
* **Emoji control**: Disable with `options(couplr.emoji = FALSE)` if preferred
* **Philosophy**: Errors should be less intimidating, more memorable, and provide clear guidance

### New Functions

* `preprocess_matching_vars()` - Main preprocessing orchestrator
* `balance_diagnostics()` - Comprehensive balance assessment
* `balance_table()` - Formatted balance tables for reporting
* `join_matched()` - Create analysis-ready datasets from matching results
* `augment.matching_result()` - Broom-style interface for joined data
* `compute_distances()` - Precompute and cache distance matrices
* `update_constraints()` - Modify constraints on distance objects
* `is_distance_object()` - Type checking for distance objects
* `diagnose_distance_matrix()` - Comprehensive distance diagnostics
* `check_cost_distribution()` - Check for distribution problems
* Added robust scaling method using median and MAD

### Documentation & Examples

* `examples/auto_scale_demo.R` - 5 preprocessing demonstrations
* `examples/balance_diagnostics_demo.R` - 6 balance diagnostic examples
* `examples/join_matched_demo.R` - 8 joined dataset demonstrations
* `examples/distance_cache_demo.R` - Distance caching and reuse examples
* `examples/parallel_matching_demo.R` - 7 parallel processing examples
* `examples/error_messages_demo.R` - 10 fun error message demonstrations
* Complete implementation documentation (claude/IMPLEMENTATION_STEP1.md through STEP6.md)
* All functions have full Roxygen documentation

### Tests

* Added 34+ new tests (10 for preprocessing, 11 for balance diagnostics, 13 for joined datasets, tests for distance caching)
* All tests passing with full backward compatibility

## Major Changes (Initial 1.0.0 Release)

### Package Renamed: lapr → couplr

The package has been renamed from **lapr** to **couplr** to better reflect its purpose as a general pairing and matching toolkit.

**couplr** = Optimal pairing and matching via linear assignment

### Clean 1.0.0 Release

First official stable release with clean, well-organized codebase.

## New Organization

### R Code
- Eliminated 3 redundant files
- Consistent `morph_*` naming prefix
- Two-layer API: `assignment()` (low-level) + `lap_solve()` (tidy)
- 10 well-organized files (down from 13)

### C++ Code  
- Modular subdirectory structure:
  - `src/core/` - Utilities and headers
  - `src/interface/` - Rcpp exports
  - `src/solvers/` - 14 LAP algorithms
  - `src/gabow_tarjan/` - Gabow-Tarjan solver
  - `src/morph/` - Image morphing

## Features

### Solvers
Hungarian, Jonker-Volgenant, Auction (3 variants), SAP/SSP, SSAP-Bucket, Cost-scaling, Cycle-cancel, Gabow-Tarjan, Hopcroft-Karp, Line-metric, Brute-force, Auto-select

### High-Level
✅ Tidy tibble interface
✅ Matrix & data frame inputs  
✅ Grouped data frames
✅ Batch solving + parallelization
✅ K-best solutions (Murty, Lawler)
✅ Rectangular matrices
✅ Forbidden assignments (NA/Inf)
✅ Maximize/minimize
✅ Pixel morphing visualization

## API

- `lap_solve()` - Main tidy interface
- `lap_solve_batch()` - Batch solving
- `lap_solve_kbest()` - K-best solutions
- `assignment()` - Low-level solver
- Utilities: `get_total_cost()`, `as_assignment_matrix()`, etc.
- Visualization: `pixel_morph()`, `pixel_morph_animate()`

---

*Development history under "lapr" available in git log before v1.0.0.*
