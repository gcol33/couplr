# Changelog

## couplr 1.0.0

### Major New Features (2025-11-19 Update)

#### Automatic Preprocessing and Scaling

The package now includes intelligent preprocessing to improve matching
quality:

- **New `auto_scale` parameter** in
  [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)
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
  [`preprocess_matching_vars()`](https://gcol33.github.io/couplr/reference/preprocess_matching_vars.md)
  function for manual preprocessing control
- Categorical variable encoding for binary and ordered factors

#### Balance Diagnostics

Comprehensive tools to assess matching quality:

- **New
  [`balance_diagnostics()`](https://gcol33.github.io/couplr/reference/balance_diagnostics.md)
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
- [`balance_table()`](https://gcol33.github.io/couplr/reference/balance_table.md)
  creates publication-ready formatted tables
- Informative print methods with interpretation guides

#### Joined Matched Dataset Output

Create analysis-ready datasets directly from matching results:

- **New
  [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)
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
  [`augment()`](https://gcol33.github.io/couplr/reference/augment.md)
  method** for tidymodels integration:
  - S3 method following broom package conventions
  - Sensible defaults for quick exploration
  - Supports all
    [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)
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
  [`compute_distances()`](https://gcol33.github.io/couplr/reference/compute_distances.md)
  function** precomputes and caches distance matrices:
  - Compute distances once, reuse across multiple matching operations
  - Store complete metadata: variables, distance metric, scaling method,
    timestamps
  - Preserve original datasets for seamless integration with
    [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)
  - Enable rapid exploration of different matching parameters
  - Performance improvement: ~60% faster when trying multiple matching
    strategies
- **Distance objects** (S3 class `distance_object`):
  - Self-contained: cost matrix, IDs, metadata, original data
  - Works with both
    [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
    and
    [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)
  - Pass as first argument instead of datasets:
    `match_couples(dist_obj, max_distance = 5)`
  - Informative print and summary methods with distance statistics
- **Constraint modification** via
  [`update_constraints()`](https://gcol33.github.io/couplr/reference/update_constraints.md):
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
  [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md):
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
  [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md):
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
  [`diagnose_distance_matrix()`](https://gcol33.github.io/couplr/reference/diagnose_distance_matrix.md):
  - Comprehensive analysis of cost distributions
  - Variable-specific problem detection
  - Actionable suggestions for fixes
  - Quality rating (good/fair/poor)
- **Emoji control**: Disable with `options(couplr.emoji = FALSE)` if
  preferred
- **Philosophy**: Errors should be less intimidating, more memorable,
  and provide clear guidance

#### New Functions

- [`preprocess_matching_vars()`](https://gcol33.github.io/couplr/reference/preprocess_matching_vars.md) -
  Main preprocessing orchestrator
- [`balance_diagnostics()`](https://gcol33.github.io/couplr/reference/balance_diagnostics.md) -
  Comprehensive balance assessment
- [`balance_table()`](https://gcol33.github.io/couplr/reference/balance_table.md) -
  Formatted balance tables for reporting
- [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md) -
  Create analysis-ready datasets from matching results
- [`augment.matching_result()`](https://gcol33.github.io/couplr/reference/augment.matching_result.md) -
  Broom-style interface for joined data
- [`compute_distances()`](https://gcol33.github.io/couplr/reference/compute_distances.md) -
  Precompute and cache distance matrices
- [`update_constraints()`](https://gcol33.github.io/couplr/reference/update_constraints.md) -
  Modify constraints on distance objects
- [`is_distance_object()`](https://gcol33.github.io/couplr/reference/is_distance_object.md) -
  Type checking for distance objects
- [`diagnose_distance_matrix()`](https://gcol33.github.io/couplr/reference/diagnose_distance_matrix.md) -
  Comprehensive distance diagnostics
- [`check_cost_distribution()`](https://gcol33.github.io/couplr/reference/check_cost_distribution.md) -
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
- All 1509+ tests passing with full backward compatibility

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
  [`assignment()`](https://gcol33.github.io/couplr/reference/assignment.md)
  (low-level) +
  [`lap_solve()`](https://gcol33.github.io/couplr/reference/lap_solve.md)
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

- [`lap_solve()`](https://gcol33.github.io/couplr/reference/lap_solve.md) -
  Main tidy interface
- [`lap_solve_batch()`](https://gcol33.github.io/couplr/reference/lap_solve_batch.md) -
  Batch solving
- [`lap_solve_kbest()`](https://gcol33.github.io/couplr/reference/lap_solve_kbest.md) -
  K-best solutions
- [`assignment()`](https://gcol33.github.io/couplr/reference/assignment.md) -
  Low-level solver
- Utilities:
  [`get_total_cost()`](https://gcol33.github.io/couplr/reference/get_total_cost.md),
  [`as_assignment_matrix()`](https://gcol33.github.io/couplr/reference/as_assignment_matrix.md),
  etc.
- Visualization:
  [`pixel_morph()`](https://gcol33.github.io/couplr/reference/pixel_morph.md),
  [`pixel_morph_animate()`](https://gcol33.github.io/couplr/reference/pixel_morph_animate.md)

------------------------------------------------------------------------

*Development history under “lapr” available in git log before v1.0.0.*
