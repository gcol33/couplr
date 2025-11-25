# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project
adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0](https://github.com/gcol33/couplr/releases/tag/v1.0.0) - 2025-11-19

### Added

#### Matching Enhancements - Step 1: Automatic Scaling and Preprocessing

- **Automatic preprocessing** with `auto_scale` parameter in
  [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)
- **Variable health checks** via
  [`check_variable_health()`](https://gcol33.github.io/couplr/reference/check_variable_health.md):
  - Detects constant columns (SD = 0) and excludes with warning
  - Detects nearly-constant columns (SD \< threshold) and warns
  - Detects all-NA columns and excludes with warning
  - Detects high missingness (\>50%) and warns
  - Detects extreme skewness (\|skewness\| \> 2) and provides info
    messages
  - Returns detailed diagnostics including per-variable statistics
- **Smart scaling method selection** via
  [`suggest_scaling()`](https://gcol33.github.io/couplr/reference/suggest_scaling.md):
  - Analyzes variable distributions and detects outliers using IQR
    method
  - Checks for different scales across variables
  - Recommends “robust”, “standardize”, “range”, or “none”
- **Robust scaling method** added to
  [`apply_scaling()`](https://gcol33.github.io/couplr/reference/apply_scaling.md):
  - Uses median and MAD (median absolute deviation)
  - Resistant to outliers and skewed distributions
  - Formula: `(x - median) / MAD`
- **Categorical variable encoding** via
  [`auto_encode_categorical()`](https://gcol33.github.io/couplr/reference/auto_encode_categorical.md):
  - Binary variables → 0/1 encoding
  - Ordered factors → numeric codes
  - Error for unordered categorical (requires Gower distance)
- **Main preprocessing orchestrator**
  [`preprocess_matching_vars()`](https://gcol33.github.io/couplr/reference/preprocess_matching_vars.md):
  - Runs variable health checks
  - Automatically excludes problematic variables
  - Suggests and applies scaling method
  - Returns preprocessing result with metadata
  - Exported for direct user access
- **Print methods** for preprocessing results:
  - [`print.variable_health()`](https://gcol33.github.io/couplr/reference/print.variable_health.md) -
    Pretty printing for health diagnostics
  - [`print.preprocessing_result()`](https://gcol33.github.io/couplr/reference/print.preprocessing_result.md) -
    Summary of preprocessing results
- **10 comprehensive tests** for preprocessing functionality
- **Example file** `examples/auto_scale_demo.R` with 5 demonstrations

#### Matching Enhancements - Step 2: Balance Diagnostics

- **Balance diagnostics** via
  [`balance_diagnostics()`](https://gcol33.github.io/couplr/reference/balance_diagnostics.md):
  - Computes standardized differences (mean diff / pooled SD)
  - Calculates variance ratios (SD_left / SD_right)
  - Performs Kolmogorov-Smirnov tests for distribution comparison
  - Overall balance metrics (mean, max, % large imbalance)
  - Per-block statistics with quality ratings when blocking used
  - Counts matched and unmatched units
  - Works with both
    [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
    and
    [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)
    results
- **Balance table formatting** via
  [`balance_table()`](https://gcol33.github.io/couplr/reference/balance_table.md):
  - Clean tabular output suitable for reports and publications
  - Configurable decimal precision
- **Standardized difference calculation** via
  [`standardized_difference()`](https://gcol33.github.io/couplr/reference/standardized_difference.md):
  - Internal function with robust edge case handling
  - Supports pooled or group-specific SD
  - Handles NA values, empty vectors, constant data
- **Helper function**
  [`calculate_var_balance()`](https://gcol33.github.io/couplr/reference/calculate_var_balance.md):
  - Per-variable balance statistics
  - Mean, SD, mean difference for both groups
  - Standardized difference, variance ratio, KS statistic
- **Print method** for balance diagnostics:
  - [`print.balance_diagnostics()`](https://gcol33.github.io/couplr/reference/print.balance_diagnostics.md) -
    Comprehensive output with:
    - Matching summary (method, matched/unmatched counts)
    - Variable-level balance table
    - Overall balance assessment with quality ratings
    - Block-level statistics (if blocking used)
    - Interpretation guide for standardized differences
- **Block-level quality ratings**:
  - Excellent: \|Std Diff\| \< 0.10
  - Good: \|Std Diff\| 0.10-0.25
  - Fair: \|Std Diff\| 0.25-0.50
  - Poor: \|Std Diff\| \> 0.50
  - Unknown: For edge cases (empty blocks, all NA)
- **11 comprehensive tests** for balance diagnostics
- **Example file** `examples/balance_diagnostics_demo.R` with 6
  demonstrations:
  - Basic balance diagnostics
  - Comparing optimal vs greedy matching
  - Balance with blocking
  - Detecting poor balance
  - Extracting specific metrics
  - Using balance tables for publication

#### Matching Enhancements - Step 3: Joined Matched Dataset Output

- **Analysis-ready dataset creation** via
  [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md):
  - Automatically joins matched pairs with original left and right data
  - Eliminates manual data wrangling after matching
  - Selectable variables via `left_vars` and `right_vars` parameters
  - Customizable suffixes for disambiguating overlapping column names
    (default: `_left`, `_right`)
  - Optional metadata columns: `pair_id`, `distance`, `block_id`
  - Custom ID column support via `left_id` and `right_id` parameters
  - Clean column ordering: pair_id → left_id → right_id → distance →
    block_id → variables
  - Works with all matching methods (optimal and greedy)
  - Preserves block information when blocking was used
- **Broom-style interface** via
  [`augment.matching_result()`](https://gcol33.github.io/couplr/reference/augment.matching_result.md):
  - S3 method following broom package conventions
  - Thin wrapper around
    [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)
    with sensible defaults
  - Integration with tidymodels workflows
  - Supports all
    [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)
    parameters via `...`
- **Comprehensive input validation**:
  - Checks for matching_result object type
  - Validates data frame inputs
  - Verifies ID column existence
  - Confirms variable availability
  - Validates suffix format (must be length 2)
  - Handles empty matching results with informative warning
- **13 comprehensive tests** covering:
  - Basic joining functionality
  - Custom suffixes
  - Variable selection
  - Blocking integration
  - Include/exclude options (distance, pair_id, block_id)
  - Custom ID columns
  - Input validation
  - Empty matches
  - Greedy matching compatibility
  - augment() method
  - Column ordering
- **Example file** `examples/join_matched_demo.R` with 8 demonstrations:
  - Basic treatment effect analysis
  - Custom variable selection
  - Matched analysis with blocking
  - Minimal output for compact datasets
  - Using augment() (broom-style)
  - Greedy matching integration
  - Custom ID columns
  - Complete workflow with balance diagnostics

#### Matching Enhancements - Step 4: Precomputed and Reusable Distances

- **Distance caching** via
  [`compute_distances()`](https://gcol33.github.io/couplr/reference/compute_distances.md):
  - Precomputes distance matrix between left and right datasets
  - Stores complete metadata (vars, distance metric, scaling,
    timestamps)
  - Preserves original datasets for seamless integration with
    join_matched()
  - Enables reuse across multiple matching operations
  - Performance: ~60% faster when trying multiple matching parameters
- **Reusable distance objects** (S3 class: `distance_object`):
  - Contains: cost_matrix, left/right IDs, block information, metadata,
    original datasets
  - Works with both match_couples() and greedy_couples()
  - Can be passed as first parameter instead of datasets
  - Example:
    `dist_obj <- compute_distances(left, right, vars); result <- match_couples(dist_obj)`
- **Constraint modification** via
  [`update_constraints()`](https://gcol33.github.io/couplr/reference/update_constraints.md):
  - Apply new max_distance or calipers without recomputing distances
  - Creates new distance_object with updated cost matrix
  - Follows R’s copy-on-modify semantics
- **Integration with existing functions**:
  - Modified match_couples() signature:
    `match_couples(left, right = NULL, vars = NULL, ...)`
  - Modified greedy_couples() signature:
    `greedy_couples(left, right = NULL, vars = NULL, ...)`
  - Automatically detects distance_object and routes to specialized
    handlers
  - 100% backward compatible with existing code
- **Helper functions**:
  - [`is_distance_object()`](https://gcol33.github.io/couplr/reference/is_distance_object.md) -
    Type checking
  - [`print.distance_object()`](https://gcol33.github.io/couplr/reference/print.distance_object.md) -
    Informative summary with distance statistics
  - [`summary.distance_object()`](https://gcol33.github.io/couplr/reference/summary.distance_object.md) -
    Detailed statistics with quantiles and sparsity analysis
- **Internal implementations**:
  - [`match_couples_from_distance()`](https://gcol33.github.io/couplr/reference/match_couples_from_distance.md) -
    Handles optimal matching from cached distances
  - [`greedy_couples_from_distance()`](https://gcol33.github.io/couplr/reference/greedy_couples_from_distance.md) -
    Handles greedy matching from cached distances

#### Matching Enhancements - Step 5: Parallel Processing

- **Parallel block matching** via `parallel` parameter in
  [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
  and
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md):
  - Distributes blocked matching across multiple cores using the
    `future` package
  - Automatic worker setup with `parallel = TRUE` (uses
    availableCores() - 1)
  - Custom plan support: pass plan name as string (e.g.,
    `parallel = "multisession"`)
  - Works with both optimal and greedy matching strategies
  - Graceful fallback to sequential processing if future packages
    unavailable
- **Parallel infrastructure** in `R/matching_parallel.R`:
  - [`setup_parallel()`](https://gcol33.github.io/couplr/reference/setup_parallel.md) -
    Configure parallel backend with auto-detection
  - [`restore_parallel()`](https://gcol33.github.io/couplr/reference/restore_parallel.md) -
    Restore original future plan after execution
  - [`can_parallelize()`](https://gcol33.github.io/couplr/reference/can_parallelize.md) -
    Check if future packages are available
  - [`parallel_lapply()`](https://gcol33.github.io/couplr/reference/parallel_lapply.md) -
    Unified parallel/sequential lapply interface
  - [`match_blocks_parallel()`](https://gcol33.github.io/couplr/reference/match_blocks_parallel.md) -
    Parallel optimal matching across blocks
  - [`greedy_blocks_parallel()`](https://gcol33.github.io/couplr/reference/greedy_blocks_parallel.md) -
    Parallel greedy matching across blocks
- **Performance benefits**:
  - Scales with number of blocks and block size
  - Best for 10+ blocks with 50+ units per block
  - Speedup depends on available cores and problem complexity
  - Minimal overhead for small problems (automatic detection)
- **Cross-platform support**:
  - Windows: `multisession` plan (separate R processes)
  - Unix/Mac: `multicore` or `multisession` plans
  - Cluster: Distributed computing via future’s cluster plan
  - Respects user-configured future plans
- **Integration**:
  - Works seamlessly with blocking (via `block_id` parameter)
  - Compatible with distance caching from Step 4
  - Supports all existing matching parameters
  - Automatic plan restoration prevents side effects
- **7 comprehensive examples** in `examples/parallel_matching_demo.R`:
  - Basic parallel vs sequential comparison
  - Custom parallel plan configuration
  - Greedy matching with parallelization
  - When parallel processing helps most
  - Combining parallel + distance caching
  - Platform-specific plans
  - Performance tips and best practices

#### Matching Enhancements - Step 6: Fun Error Messages and Cost Checking

- **Automatic cost distribution checking** via `check_costs` parameter
  (default: `TRUE`):
  - Enabled by default in
    [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
    and
    [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)
  - Detects common problems before matching begins
  - Can be disabled with `check_costs = FALSE` for production code
- **Fun, couple-themed error messages** throughout the package:
  - Light, memorable messages inspired by testthat
  - Themed around coupling, matching, and pairing
  - Less intimidating for new users, more memorable
  - Clear actionable suggestions for fixing problems
- **Emoji support** (optional, controlled via
  `options(couplr.emoji = FALSE)`):
  - 💔 for errors (broken heart)
  - 💌 for warnings (love letter)
  - 💬 for info messages (speech balloon)
  - 💖 for success (sparkling heart)
  - ✨ for suggestions (sparkles)
  - 🔍 for search/investigation
  - Automatically disabled in non-interactive sessions
- **Message helper functions** in `R/matching_messages.R`:
  - [`couplr_stop()`](https://gcol33.github.io/couplr/reference/couplr_stop.md) -
    Fun error messages with emoji
  - [`couplr_warn()`](https://gcol33.github.io/couplr/reference/couplr_warn.md) -
    Fun warning messages
  - [`couplr_inform()`](https://gcol33.github.io/couplr/reference/couplr_inform.md) -
    Info messages
  - [`couplr_success()`](https://gcol33.github.io/couplr/reference/couplr_success.md) -
    Success messages
  - [`couplr_emoji()`](https://gcol33.github.io/couplr/reference/couplr_emoji.md) -
    Get themed emoji
  - Specific error helpers:
    [`err_missing_data()`](https://gcol33.github.io/couplr/reference/err_missing_data.md),
    [`err_missing_vars()`](https://gcol33.github.io/couplr/reference/err_missing_vars.md),
    [`err_no_valid_pairs()`](https://gcol33.github.io/couplr/reference/err_no_valid_pairs.md),
    etc.
  - Specific warning helpers:
    [`warn_constant_var()`](https://gcol33.github.io/couplr/reference/warn_constant_var.md),
    [`warn_many_zeros()`](https://gcol33.github.io/couplr/reference/warn_many_zeros.md),
    [`warn_extreme_costs()`](https://gcol33.github.io/couplr/reference/warn_extreme_costs.md),
    etc.
- **Cost distribution checking** via
  [`check_cost_distribution()`](https://gcol33.github.io/couplr/reference/check_cost_distribution.md):
  - Detects too many zero distances (\>10%) - suggests checking for
    duplicates
  - Detects extreme cost ratios (99th percentile \> 10x 95th) - suggests
    scaling
  - Detects many forbidden pairs (\>50%) - suggests relaxing constraints
  - Detects constant distances - suggests checking variable
    informativeness
  - Returns detailed diagnostic information
- **Comprehensive diagnostics** via
  [`diagnose_distance_matrix()`](https://gcol33.github.io/couplr/reference/diagnose_distance_matrix.md)
  (exported):
  - Full analysis of distance matrix quality
  - Variable-specific problem detection
  - Per-variable issue tracking (constant, extreme scale differences)
  - Actionable suggestions for improvements
  - Quality rating: “good”, “fair”, or “poor”
- **Integration**:
  - Integrated into
    [`match_couples_single()`](https://gcol33.github.io/couplr/reference/match_couples_single.md),
    [`match_couples_from_distance()`](https://gcol33.github.io/couplr/reference/match_couples_from_distance.md)
  - Integrated into greedy matching functions
  - Warnings issued before LAP solving to catch problems early
  - Backward compatible - all tests pass with check_costs enabled
- **10 comprehensive examples** in `examples/error_messages_demo.R`:
  - Basic friendly error messages
  - Duplicate detection warnings
  - Skewed distribution detection
  - Overly strict constraints
  - Constant variable detection
  - No valid pairs scenarios
  - Distance matrix diagnostics
  - Disabling cost checks
  - Emoji control
  - Balance quality messages

### Changed

- Updated `DESCRIPTION` to include new matching features
- Enhanced `NAMESPACE` with new exports:
  - [`preprocess_matching_vars()`](https://gcol33.github.io/couplr/reference/preprocess_matching_vars.md)
  - [`balance_diagnostics()`](https://gcol33.github.io/couplr/reference/balance_diagnostics.md)
  - [`balance_table()`](https://gcol33.github.io/couplr/reference/balance_table.md)
  - [`join_matched()`](https://gcol33.github.io/couplr/reference/join_matched.md)
  - [`augment.matching_result()`](https://gcol33.github.io/couplr/reference/augment.matching_result.md)
  - [`augment()`](https://gcol33.github.io/couplr/reference/augment.md)
    generic
  - [`compute_distances()`](https://gcol33.github.io/couplr/reference/compute_distances.md)
  - [`is_distance_object()`](https://gcol33.github.io/couplr/reference/is_distance_object.md)
  - [`update_constraints()`](https://gcol33.github.io/couplr/reference/update_constraints.md)
  - S3 print and summary methods for new classes
- Modified function signatures to accept distance objects, parallel
  processing, and cost checking:
  - `match_couples(left, right = NULL, vars = NULL, ..., parallel = FALSE, check_costs = TRUE)` -
    left can be distance_object, parallel for blocked matching,
    automatic cost checking
  - `greedy_couples(left, right = NULL, vars = NULL, ..., parallel = FALSE, check_costs = TRUE)` -
    left can be distance_object, parallel for blocked matching,
    automatic cost checking
- Added `future` and `future.apply` to Suggests for parallel processing
  support
- Updated error messages throughout package to use fun, couple-themed
  helpers
- Added
  [`diagnose_distance_matrix()`](https://gcol33.github.io/couplr/reference/diagnose_distance_matrix.md)
  to NAMESPACE exports

### Fixed

- Greedy matching functions now properly exported via Rcpp interface
- Block statistics preserved correctly in
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)
  when `return_diagnostics = FALSE`
- Variable health checks now include all required fields for all edge
  cases
- Robust edge case handling in balance diagnostics:
  - Variance ratio calculation handles NA values
  - Block quality determination handles NA/NaN from empty blocks
  - Overall metrics handle all-NA standardized differences

### Documentation

- Added `IMPLEMENTATION_STEP1.md` documenting preprocessing
  implementation
- Added `IMPLEMENTATION_STEP2.md` documenting balance diagnostics
  implementation
- Updated Roxygen documentation for all new functions
- Created comprehensive examples demonstrating new features
- All functions have complete parameter descriptions and return value
  documentation

### Tests

- **Total: 1382 tests passing** (up from 1365)
- Added 10 tests for preprocessing (Step 1)
- Added 11 tests for balance diagnostics (Step 2)
- Added 13 tests for joined dataset output (Step 3)
- All existing tests continue to pass
- No warnings or errors
- Test coverage includes:
  - Variable health detection (constant, all-NA, high missingness,
    skewness)
  - Scaling suggestion
  - Preprocessing integration
  - Standardized difference calculation
  - Balance diagnostics with simple and blocked matching
  - Balance table formatting
  - Joined dataset creation
  - Variable selection and suffix handling
  - Custom ID columns
  - Broom-style augment method
  - Print methods
  - Input validation
  - Edge case handling

### Backward Compatibility

- **100% backward compatible**
- All new parameters default to `FALSE` or previous behavior
- No breaking changes to existing APIs
- All existing code continues to work unchanged
- Return structures extended, not replaced

------------------------------------------------------------------------

## [0.1.0](https://github.com/gcol33/couplr/releases/tag/v0.1.0) - Previous Release

Initial release with core LAP solving functionality, basic matching, and
pixel morphing.
