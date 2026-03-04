# Changelog

## couplr 1.3.0

### New Features

#### Optimal Full Matching

- **[`full_match()`](https://gillescolling.com/couplr/reference/full_match.md)
  gains `method = "optimal"` (new default)** using a min-cost max-flow
  solver (Dijkstra + Johnson potentials) that finds the globally optimal
  group assignment minimizing total distance:
  - Standard lower bound transformation enforces `min_controls` per
    group
  - Automatic transposition when `n_left > n_right`
  - New C++ solver: `solve_full_matching.cpp` (self-contained MCMF)
  - `method = "greedy"` preserved for fast approximate matching

#### Vignette Updates

- **Getting Started**: Added full matching section with
  [`full_match()`](https://gillescolling.com/couplr/reference/full_match.md)
  example
- **Matching Workflows**: New “Full Matching (Variable-Ratio Groups)”
  section covering optimal vs greedy, constraints, weights, and
  comparison table
- **Comparison**: Updated feature table and all sections to reflect
  couplr’s full matching support (previously listed as “No”)

------------------------------------------------------------------------

## couplr 1.2.0

### New Features

#### Full Matching

- **New
  [`full_match()`](https://gillescolling.com/couplr/reference/full_match.md)
  function** assigns every unit to a matched group with variable ratios
  (1:k or k:1):
  - Greedy group formation: match each left to nearest right, then
    assign remaining right units to nearest matched left
  - Caliper support: `caliper` (absolute) or `caliper_sd` (SD-based)
  - Control group size constraints: `min_controls`, `max_controls`
  - Weights inversely proportional to group size
  - Returns `full_matching_result` S3 class

#### Coarsened Exact Matching (CEM)

- **New
  [`cem_match()`](https://gillescolling.com/couplr/reference/cem_match.md)
  function** implements coarsened exact matching:
  - Coarsens continuous variables into bins (Sturges, FD, Scott, or
    custom)
  - Exact matching on coarsened values with stratum-based weights
  - Support for categorical grouping variables via `grouping` parameter
  - Custom cutpoints per variable via `cutpoints` parameter
  - Returns `cem_result` S3 class with matched units and strata summary

#### Subclassification

- **New
  [`subclass_match()`](https://gillescolling.com/couplr/reference/subclass_match.md)
  function** divides units into propensity score strata:
  - Quantile-based stratification with configurable number of subclasses
  - Supports pre-computed PS, pre-fitted models, or formula interface
  - Target estimands: ATT, ATE, ATC with appropriate weighting
  - Returns `subclass_result` S3 class with subclass summary

#### Output Layer & Ecosystem Integration

- **New
  [`match_data()`](https://gillescolling.com/couplr/reference/match_data.md)
  generic** converts any couplr result to analysis-ready format with
  `treatment`, `weights`, `subclass`, and `distance` columns. Methods
  for all result types (matching, full, CEM, subclass).
- **New
  [`as_matchit()`](https://gillescolling.com/couplr/reference/as_matchit.md)
  converter** creates `matchit`-class objects from couplr results,
  enabling interop with cobalt, marginaleffects, and other MatchIt
  ecosystem packages.
- **cobalt `bal.tab()` methods** for all couplr result types. Requires
  cobalt package (in Suggests).

#### Mahalanobis Distance Improvements

- **Robust singularity check** using
  [`rcond()`](https://rdrr.io/r/base/kappa.html) instead of fragile
  `det() == 0`
- **Custom `sigma` parameter** in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md),
  [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md),
  and
  [`compute_distance_matrix()`](https://gillescolling.com/couplr/reference/compute_distance_matrix.md)
  for user-supplied covariance matrices
- **Vectorized computation** replacing nested R for-loops for ~10x
  speedup

#### S3 Generics

- [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  and
  [`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md)
  are now S3 generics with methods for all result types. Existing code
  is 100% backward-compatible.

#### New Functions

- [`full_match()`](https://gillescolling.com/couplr/reference/full_match.md) -
  Variable-ratio full matching
- [`cem_match()`](https://gillescolling.com/couplr/reference/cem_match.md) -
  Coarsened exact matching
- [`subclass_match()`](https://gillescolling.com/couplr/reference/subclass_match.md) -
  Propensity score subclassification
- [`match_data()`](https://gillescolling.com/couplr/reference/match_data.md) -
  Unified analysis-ready output
- [`as_matchit()`](https://gillescolling.com/couplr/reference/as_matchit.md) -
  Convert to MatchIt format

------------------------------------------------------------------------

## couplr 1.1.0

CRAN release: 2026-03-03

### New Features

#### Ratio and Replacement Matching

- **k:1 ratio matching** via `ratio` parameter in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md).
  Matches k control units to each treated unit by replicating the cost
  matrix, then deduplicates assignments.
- **With-replacement matching** via `replace` parameter. Each treated
  unit independently selects its nearest control, allowing controls to
  be reused across multiple treated units.

#### Propensity Score Matching

- **New
  [`ps_match()`](https://gillescolling.com/couplr/reference/ps_match.md)
  function** wraps
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  with logistic regression:
  - Accepts a formula or pre-fitted `glm` object
  - Matches on the logit of propensity scores with a caliper
  - Default caliper: 0.2 SD of logit(PS) (Rosenbaum and Rubin
    recommendation)
  - Returns matching_result with PS model metadata

#### Cardinality Matching

- **New
  [`cardinality_match()`](https://gillescolling.com/couplr/reference/cardinality_match.md)
  function** maximizes sample size subject to balance constraints:
  - Starts with a full optimal match, then iteratively prunes imbalanced
    pairs
  - Balance threshold via `max_std_diff` (default: 0.1 for excellent
    balance)
  - Configurable pruning speed with `batch_fraction`
  - Returns pruning diagnostics: iterations, pairs removed, final
    balance

#### Sensitivity Analysis

- **New
  [`sensitivity_analysis()`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  function** implements Rosenbaum bounds:
  - Tests sensitivity of matched comparisons to hidden bias
  - Uses Wilcoxon signed-rank statistic with upper/lower p-value bounds
  - Reports critical gamma (smallest gamma at which significance is
    lost)
  - S3 methods: [`print()`](https://rdrr.io/r/base/print.html),
    [`summary()`](https://rdrr.io/r/base/summary.html),
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

#### Visualization

- **[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods** for ggplot2-based visualizations (requires ggplot2):
  - [`autoplot.matching_result()`](https://gillescolling.com/couplr/reference/autoplot.matching_result.md):
    histogram, density, or ecdf of distances
  - [`autoplot.balance_diagnostics()`](https://gillescolling.com/couplr/reference/autoplot.balance_diagnostics.md):
    love plot, histogram, or variance ratio plot
  - [`autoplot.sensitivity_analysis()`](https://gillescolling.com/couplr/reference/autoplot.sensitivity_analysis.md):
    gamma vs p-value curve
- **Enhanced
  [`summary.matching_result()`](https://gillescolling.com/couplr/reference/summary.matching_result.md)**
  now reports match rate and distance percentiles

#### New Functions

- [`ps_match()`](https://gillescolling.com/couplr/reference/ps_match.md) -
  Propensity score matching with logit caliper
- [`cardinality_match()`](https://gillescolling.com/couplr/reference/cardinality_match.md) -
  Balance-constrained cardinality matching
- [`sensitivity_analysis()`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md) -
  Rosenbaum bounds sensitivity analysis

#### Tests

- Added 58 new tests across 7 test files
- All 4916 tests passing across platforms

------------------------------------------------------------------------

## couplr 1.0.7

### Bug Fixes

- Fixed undefined behavior (UB) in Gabow-Tarjan algorithm: replaced left
  bit-shift of potentially negative values with multiplication to avoid
  sanitizer errors on M1-SAN checks
- Fixed namespace conflict with
  [`select()`](https://dplyr.tidyverse.org/reference/select.html) in
  vignettes by using explicit
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
  to prevent masking by MASS or other packages

------------------------------------------------------------------------

## couplr 1.0.6

CRAN release: 2026-01-20

### Documentation

- Added Overview section to algorithms vignette with audience and
  prerequisites
- Fixed workflow diagram dark mode text handling in matching-workflows
  vignette
- Improved SVG theme-awareness for multi-line text labels
- Removed grid lines from matching-workflows plots for cleaner
  appearance
- Added threshold labels to balance comparison plot

------------------------------------------------------------------------

## couplr 1.0.0

### Major New Features (2025-11-19 Update)

#### Automatic Preprocessing and Scaling

The package now includes intelligent preprocessing to improve matching
quality:

- **New `auto_scale` parameter** in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)
  enables automatic preprocessing
- **Variable health checks** detect and handle problematic variables:
  - Constant columns (SD = 0) are automatically excluded with warnings
  - High missingness (\>50%) triggers warnings
  - Extreme skewness (\|skewness\| \> 2) is flagged
- **Smart scaling method selection** analyzes data and recommends:
  - “robust” scaling using median and MAD (resistant to outliers)
  - “standardize” for traditional mean-centering and SD scaling
  - “range” for min-max normalization
- New
  [`preprocess_matching_vars()`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  function for manual preprocessing control
- Categorical variable encoding for binary and ordered factors

#### Balance Diagnostics

Comprehensive tools to assess matching quality:

- **New
  [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  function** computes multiple balance metrics:
  - Standardized differences: (mean_left - mean_right) / pooled_sd
  - Variance ratios: SD_left / SD_right
  - Kolmogorov-Smirnov tests for distribution comparison
  - Overall balance metrics (mean, max, % large imbalance)
- **Quality thresholds** with interpretation:
  - \|Std Diff\| \< 0.10: Excellent balance
  - \|Std Diff\| 0.10-0.25: Good balance
  - \|Std Diff\| 0.25-0.50: Acceptable balance
  - \|Std Diff\| \> 0.50: Poor balance
- Per-block statistics with quality ratings when blocking is used
- [`balance_table()`](https://gillescolling.com/couplr/reference/balance_table.md)
  creates publication-ready formatted tables
- Informative print methods with interpretation guides

#### Joined Matched Dataset Output

Create analysis-ready datasets directly from matching results:

- **New
  [`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md)
  function** automates data preparation:
  - Joins matched pairs with original left and right datasets
  - Eliminates manual data wrangling after matching
  - Select specific variables via `left_vars` and `right_vars`
    parameters
  - Customizable suffixes (default: `_left`, `_right`) for overlapping
    columns
  - Optional metadata: `pair_id`, `distance`, `block_id`
  - Works with both optimal and greedy matching
- **Broom-style
  [`augment()`](https://gillescolling.com/couplr/reference/augment.md)
  method** for tidymodels integration:
  - S3 method following broom package conventions
  - Sensible defaults for quick exploration
  - Supports all
    [`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md)
    parameters
- **Flexible output control**:
  - `include_distance` - Include/exclude matching distance
  - `include_pair_id` - Include/exclude sequential pair IDs
  - `include_block_id` - Include/exclude block identifiers
  - Custom ID column support via `left_id` and `right_id`
  - Clean column ordering: pair_id → IDs → distance → block → variables

#### Precomputed and Reusable Distances

Performance optimization for exploring multiple matching strategies:

- **New
  [`compute_distances()`](https://gillescolling.com/couplr/reference/compute_distances.md)
  function** precomputes and caches distance matrices:
  - Compute distances once, reuse across multiple matching operations
  - Store complete metadata: variables, distance metric, scaling method,
    timestamps
  - Preserve original datasets for seamless integration with
    [`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md)
  - Enable rapid exploration of different matching parameters
  - Performance improvement: ~60% faster when trying multiple matching
    strategies
- **Distance objects** (S3 class `distance_object`):
  - Self-contained: cost matrix, IDs, metadata, original data
  - Works with both
    [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
    and
    [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)
  - Pass as first argument instead of datasets:
    `match_couples(dist_obj, max_distance = 5)`
  - Informative print and summary methods with distance statistics
- **Constraint modification** via
  [`update_constraints()`](https://gillescolling.com/couplr/reference/update_constraints.md):
  - Apply new `max_distance` or `calipers` without recomputing distances
  - Creates new distance object following copy-on-modify semantics
  - Experiment with different constraints efficiently
- **Backward compatible integration**:
  - Modified function signatures:
    `match_couples(left, right = NULL, vars = NULL, ...)`
  - Automatically detects distance objects vs. datasets
  - All existing code continues to work unchanged

#### Parallel Processing

Speed up blocked matching with multi-core processing:

- **New `parallel` parameter** in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md):
  - Enable with `parallel = TRUE` for automatic configuration
  - Specify plan with `parallel = "multisession"` or other future plan
  - Works with any number of blocks - automatically determines if
    beneficial
  - Gracefully falls back if future packages not installed
- **Powered by the `future` package**:
  - Cross-platform support (Windows, Unix/Mac, clusters)
  - Respects user-configured parallel backends
  - Automatic worker management
  - Clean restoration of original plan after execution
- **Performance**:
  - Best for 10+ blocks with 50+ units per block
  - Speedup scales with number of cores and complexity
  - Minimal overhead for small problems
- **Integration**:
  - Works with all blocking methods (exact, fuzzy, clustering)
  - Compatible with distance caching from Step 4
  - Supports all matching parameters (constraints, calipers, scaling)

#### Fun Error Messages and Cost Checking

Like testthat, couplr makes errors light, memorable, and helpful with
couple-themed messages:

- **New `check_costs` parameter** (default: `TRUE`) in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md):
  - Automatically checks distance distributions before matching
  - Provides friendly, actionable warnings for common problems
  - Set to `FALSE` to skip checks in production code
- **Fun couple-themed error messages** throughout the package:
  - 💔 “No matches made - can’t couple without candidates!”
  - 🔍 “Your constraints are too strict. Love can’t bloom in a vacuum!”
  - ✨ Helpful suggestions: “Try increasing max_distance or relaxing
    calipers”
  - 💖 Success messages: “Excellent balance! These couples are
    well-matched!”
- **Automatic problem detection**:
  - **Too many zeros**: Warns about duplicates or identical values
    (\>10% zero distances)
  - **Extreme costs**: Detects skewed distributions (99th percentile \>
    10x the 95th)
  - **Many forbidden pairs**: Warns when constraints eliminate \>50% of
    valid pairs
  - **Constant distances**: Alerts when all distances are identical
  - **Constant variables**: Detects and excludes variables with no
    variation
- **New diagnostic function**
  [`diagnose_distance_matrix()`](https://gillescolling.com/couplr/reference/diagnose_distance_matrix.md):
  - Comprehensive analysis of cost distributions
  - Variable-specific problem detection
  - Actionable suggestions for fixes
  - Quality rating (good/fair/poor)
- **Emoji control**: Disable with `options(couplr.emoji = FALSE)` if
  preferred
- **Philosophy**: Errors should be less intimidating, more memorable,
  and provide clear guidance

#### New Functions

- [`preprocess_matching_vars()`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md) -
  Main preprocessing orchestrator
- [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md) -
  Comprehensive balance assessment
- [`balance_table()`](https://gillescolling.com/couplr/reference/balance_table.md) -
  Formatted balance tables for reporting
- [`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md) -
  Create analysis-ready datasets from matching results
- [`augment.matching_result()`](https://gillescolling.com/couplr/reference/augment.matching_result.md) -
  Broom-style interface for joined data
- [`compute_distances()`](https://gillescolling.com/couplr/reference/compute_distances.md) -
  Precompute and cache distance matrices
- [`update_constraints()`](https://gillescolling.com/couplr/reference/update_constraints.md) -
  Modify constraints on distance objects
- [`is_distance_object()`](https://gillescolling.com/couplr/reference/is_distance_object.md) -
  Type checking for distance objects
- [`diagnose_distance_matrix()`](https://gillescolling.com/couplr/reference/diagnose_distance_matrix.md) -
  Comprehensive distance diagnostics
- [`check_cost_distribution()`](https://gillescolling.com/couplr/reference/check_cost_distribution.md) -
  Check for distribution problems
- Added robust scaling method using median and MAD

#### Documentation & Examples

- `examples/auto_scale_demo.R` - 5 preprocessing demonstrations
- `examples/balance_diagnostics_demo.R` - 6 balance diagnostic examples
- `examples/join_matched_demo.R` - 8 joined dataset demonstrations
- `examples/distance_cache_demo.R` - Distance caching and reuse examples
- `examples/parallel_matching_demo.R` - 7 parallel processing examples
- `examples/error_messages_demo.R` - 10 fun error message demonstrations
- Complete implementation documentation (claude/IMPLEMENTATION_STEP1.md
  through STEP6.md)
- All functions have full Roxygen documentation

#### Tests

- Added 34+ new tests (10 for preprocessing, 11 for balance diagnostics,
  13 for joined datasets, tests for distance caching)
- All tests passing with full backward compatibility

### Major Changes (Initial 1.0.0 Release)

#### Package Renamed: lapr → couplr

The package has been renamed from **lapr** to **couplr** to better
reflect its purpose as a general pairing and matching toolkit.

**couplr** = Optimal pairing and matching via linear assignment

#### Clean 1.0.0 Release

First official stable release with clean, well-organized codebase.

### New Organization

#### R Code

- Eliminated 3 redundant files
- Consistent `morph_*` naming prefix
- Two-layer API:
  [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  (low-level) +
  [`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
  (tidy)
- 10 well-organized files (down from 13)

#### C++ Code

- Modular subdirectory structure:
  - `src/core/` - Utilities and headers
  - `src/interface/` - Rcpp exports
  - `src/solvers/` - 14 LAP algorithms
  - `src/gabow_tarjan/` - Gabow-Tarjan solver
  - `src/morph/` - Image morphing

### Features

#### Solvers

Hungarian, Jonker-Volgenant, Auction (3 variants), SAP/SSP, SSAP-Bucket,
Cost-scaling, Cycle-cancel, Gabow-Tarjan, Hopcroft-Karp, Line-metric,
Brute-force, Auto-select

#### High-Level

✅ Tidy tibble interface ✅ Matrix & data frame inputs  
✅ Grouped data frames ✅ Batch solving + parallelization ✅ K-best
solutions (Murty, Lawler) ✅ Rectangular matrices ✅ Forbidden
assignments (NA/Inf) ✅ Maximize/minimize ✅ Pixel morphing
visualization

### API

- [`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md) -
  Main tidy interface
- [`lap_solve_batch()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md) -
  Batch solving
- [`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md) -
  K-best solutions
- [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md) -
  Low-level solver
- Utilities:
  [`get_total_cost()`](https://gillescolling.com/couplr/reference/get_total_cost.md),
  [`as_assignment_matrix()`](https://gillescolling.com/couplr/reference/as_assignment_matrix.md),
  etc.
- Visualization:
  [`pixel_morph()`](https://gillescolling.com/couplr/reference/pixel_morph.md),
  [`pixel_morph_animate()`](https://gillescolling.com/couplr/reference/pixel_morph_animate.md)

------------------------------------------------------------------------

*Development history under “lapr” available in git log before v1.0.0.*
