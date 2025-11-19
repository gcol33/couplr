# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**couplr** (formerly lapr) is an R package providing fast solvers for Linear Assignment Problems (LAP) with modern matching capabilities. It implements 14+ algorithms (Hungarian, Jonker-Volgenant, Auction, Gabow-Tarjan, etc.) with a tidy interface, plus production-ready matching workflows with automatic preprocessing, greedy algorithms, and comprehensive balance diagnostics.

## Development Commands

### Core Package Development

```r
# Install dependencies
install.packages(c("Rcpp", "RcppEigen", "tibble", "dplyr", "testthat", "devtools"))

# Load package (recompiles C++ if needed)
devtools::load_all()  # or Ctrl+Shift+L in RStudio

# Run all tests (1369 tests)
devtools::test()  # or Ctrl+Shift+T

# Run specific test file
testthat::test_file("tests/testthat/test-matching.R")

# Run matching tests only
devtools::test(filter = "matching")

# Build documentation (after editing roxygen comments)
devtools::document()

# Full package check
devtools::check()

# Build tarball
devtools::build()
```

### C++ Development Workflow

After modifying C++ code or adding `[[Rcpp::export]]` functions:

```r
# Regenerate Rcpp exports (REQUIRED after C++ changes)
Rcpp::compileAttributes()

# Update documentation
devtools::document()

# Reload package
devtools::load_all()

# Run tests
devtools::test()
```

**Critical:** C++17 required. Windows users need Rtools with g++ supporting C++17.

### Git Workflow

```bash
# Check status
git status

# Add changes
git add .

# Commit (use descriptive messages)
git commit -m "Description of changes"

# Push to GitHub
git push origin main

# Create release tag
git tag -a v1.0.0 -m "Release message"
git push origin v1.0.0
```

## Code Architecture

### Three-Layer API Design

**Layer 1 - Low-level LAP solvers** ([R/assignment.R](R/assignment.R)):
- `assignment()` - Core solver wrapper returning `list(match, total_cost, status, method_used)`
- Direct mapping to C++ implementations
- Method auto-selection via `method = "auto"`

**Layer 2 - Tidy LAP interface** ([R/lap_solve.R](R/lap_solve.R)):
- `lap_solve()` - Returns tibble with tidy output
- Supports matrix, data frame, and grouped inputs
- Batch solving via `lap_solve_batch()`
- K-best solutions via `lap_solve_kbest()`

**Layer 3 - Matching workflows** (NEW in v1.0.0):
- `match_couples()` - Optimal one-to-one matching with preprocessing ([R/matching_core.R](R/matching_core.R:1-387))
- `greedy_couples()` - Fast greedy matching with 3 strategies ([R/matching_core.R](R/matching_core.R:388-625))
- `matchmaker()` - Blocking/stratification support ([R/matching_blocks.R](R/matching_blocks.R))
- `balance_diagnostics()` - Comprehensive balance assessment ([R/matching_diagnostics.R](R/matching_diagnostics.R))
- `preprocess_matching_vars()` - Automatic variable health checks ([R/matching_preprocessing.R](R/matching_preprocessing.R))

### Matching Layer Architecture (v1.0.0)

The matching layer provides production-ready workflows for observational studies, treatment effect estimation, and sample matching.

**Core Workflow:**
```r
# 1. Optional: Create blocks/strata
blocks <- matchmaker(left, right, block_type = "group", block_by = "site")

# 2. Match with automatic preprocessing
result <- match_couples(
  left, right,
  vars = c("age", "income", "education"),
  auto_scale = TRUE,           # Smart preprocessing
  scale = "robust",             # MAD-based scaling
  max_distance = 0.5,           # Caliper
  return_diagnostics = TRUE
)

# 3. Assess balance quality
balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))
print(balance)  # Standardized differences, variance ratios, KS tests
balance_table(balance)  # Publication-ready table
```

**File Organization:**
- [R/matching_core.R](R/matching_core.R) - Main matching functions (625 lines)
- [R/matching_distance.R](R/matching_distance.R) - Distance computation with 3 scaling methods
- [R/matching_constraints.R](R/matching_constraints.R) - Calipers, weights, max_distance
- [R/matching_blocks.R](R/matching_blocks.R) - Blocking/stratification
- [R/matching_preprocessing.R](R/matching_preprocessing.R) - Auto-scaling and health checks (510 lines)
- [R/matching_diagnostics.R](R/matching_diagnostics.R) - Balance assessment (461 lines)
- [R/matching_utils.R](R/matching_utils.R) - Shared utilities

**Key Features:**

1. **Automatic Preprocessing** ([R/matching_preprocessing.R](R/matching_preprocessing.R)):
   - Detects constant variables (SD = 0) → excludes with warning
   - Detects high missingness (>50%) → warns
   - Detects extreme skewness (|skew| > 2) → info
   - Smart scaling: "robust" (median/MAD), "standardize" (mean/SD), "range" (min-max)
   - Categorical encoding: binary (0/1), ordered factors (numeric)

2. **Greedy Matching Algorithms** ([src/solvers/greedy_matching.cpp](src/solvers/greedy_matching.cpp)):
   - `sorted` - Sort all pairs by cost, assign greedily
   - `row_best` - For each row, pick best available column
   - `pq` - Priority queue for large problems
   - 10-100x faster than optimal for large datasets

3. **Balance Diagnostics** ([R/matching_diagnostics.R](R/matching_diagnostics.R)):
   - Standardized differences: (mean_left - mean_right) / pooled_sd
   - Variance ratios: SD_left / SD_right
   - KS tests for distribution comparison
   - Per-block statistics with quality ratings
   - Quality thresholds: <0.1 excellent, 0.1-0.25 good, 0.25-0.5 acceptable, >0.5 poor

4. **Blocking Support** ([R/matching_blocks.R](R/matching_blocks.R)):
   - Exact blocking: `block_type = "group"` + `block_by = "site"`
   - K-means clustering: `block_type = "cluster"` + `n_clusters = 5`
   - Preserves block IDs through matching pipeline

### C++ Code Organization

**Subdirectory Structure:**
```
src/
├── core/              # Shared utilities
│   ├── lap_internal.h     # Function declarations
│   ├── lap_utils.cpp      # Common helpers
│   └── lap_utils.h
├── interface/         # Rcpp exports
│   └── prepare_cost_matrix.cpp
├── solvers/           # Algorithm implementations (14 solvers)
│   ├── greedy_matching.cpp    # NEW: Greedy strategies
│   ├── solve_jv.cpp          # Jonker-Volgenant
│   ├── solve_hungarian.cpp
│   ├── solve_auction.cpp
│   ├── solve_gabow_tarjan.cpp
│   ├── solve_line_metric.cpp
│   ├── solve_ssap_bucket.cpp
│   └── ... (10 more)
├── gabow_tarjan/      # Gabow-Tarjan specific code
│   ├── solve_gabow_tarjan.cpp
│   ├── utils_gabow_tarjan.cpp
│   └── utils_gabow_tarjan.h
├── morph/             # Pixel morphing
│   ├── morph_pixel_level.cpp
│   └── region_means.cpp
└── rcpp_interface.cpp # Central export point (ALL exports here)
```

**Export Pattern (CRITICAL):**

Subdirectory files (e.g., `src/solvers/greedy_matching.cpp`) do NOT use `[[Rcpp::export]]` directly. Instead:

1. Implementation in subdirectory with `_impl` suffix:
```cpp
// src/solvers/greedy_matching.cpp
List greedy_matching_impl(NumericMatrix cost_matrix, bool maximize, std::string strategy) {
    // Implementation
}
```

2. Forward declaration in [src/rcpp_interface.cpp](src/rcpp_interface.cpp):
```cpp
extern Rcpp::List greedy_matching_impl(Rcpp::NumericMatrix, bool, std::string);
```

3. Exported wrapper in [src/rcpp_interface.cpp](src/rcpp_interface.cpp):
```cpp
// [[Rcpp::export]]
Rcpp::List greedy_matching(Rcpp::NumericMatrix cost_matrix,
                           bool maximize = false,
                           std::string strategy = "row_best") {
    return greedy_matching_impl(cost_matrix, maximize, strategy);
}
```

**Why:** Rcpp only scans the main source directory, not subdirectories. All `[[Rcpp::export]]` tags MUST be in `src/*.cpp` files at the root level.

### Makefile Structure

**Makevars** (Unix/Linux/Mac):
```make
CXX_STD = CXX17
SOURCES = RcppExports.cpp rcpp_interface.cpp \
          $(wildcard interface/*.cpp) \
          $(wildcard core/*.cpp) \
          $(wildcard solvers/*.cpp) \
          $(wildcard gabow_tarjan/*.cpp) \
          $(wildcard morph/*.cpp)
OBJECTS = $(SOURCES:.cpp=.o)
```

**Makevars.win** (Windows):
- Same structure but uses `$(shell ...)` instead of backticks
- Collects all .cpp files from subdirectories using wildcards
- Automatically links all object files

### LAP Solver Implementations

Available algorithms (see [R/assignment.R](R/assignment.R)):
- `jv` - Jonker-Volgenant (general purpose, fast)
- `hungarian` - Classic Hungarian
- `auction` / `auction_gs` / `auction_scaled` - Auction variants
- `ssp` / `sap` - Shortest augmenting path
- `csflow` - Cost-scaling flow
- `cost_scaling` - Gabow's cost-scaling
- `cycle_cancel` - Cycle canceling with Karp
- `gabow_tarjan` - Gabow-Tarjan with complementary slackness
- `ssap_bucket` - Dial's algorithm for integer costs
- `line_metric` - Specialized for 1D matching
- `hk01` - Hopcroft-Karp for binary costs
- `bruteforce` - Exhaustive for tiny problems
- `auto` - Automatic selection

**Greedy Algorithms** (NEW):
- `greedy_sorted` - Sort pairs, assign greedily
- `greedy_row_best` - Row-by-row best match
- `greedy_pq` - Priority queue based

### Testing Strategy

**Test Organization** (1369 total tests):
- LAP solvers: `test-assignment-*.R` (one per solver)
- Matching workflows: [tests/testthat/test-matching.R](tests/testthat/test-matching.R) (98 tests)
  - Preprocessing: 10 tests
  - Balance diagnostics: 11 tests
  - Core matching: 9 tests
  - Greedy strategies: 4 tests
  - Integration tests: remainder
- Gabow-Tarjan modules: `test_gabow_tarjan_moduleA.R` through `moduleH.R`
- Pixel morphing: `test-pixel-morph.R`

**Running Subset of Tests:**
```r
# All matching tests
devtools::test(filter = "matching")

# Specific test
testthat::test_file("tests/testthat/test-matching.R")

# Single test case
testthat::test_that("balance_diagnostics works with blocking", { ... })
```

## Important Conventions

### Indexing
- **R code:** 1-based (R standard)
- **C++ code:** 0-based internally, convert at boundaries
- **Unmatched:** -1 (C++) / 0 (R, displayed as NA in output)

### Matrix Orientation
- Rows = left/treatment units
- Cols = right/control units
- Auto-transpose if rows > cols

### Cost Matrix Semantics
- `NA` or `Inf` = forbidden assignment
- `maximize = TRUE` negates costs internally
- Always returns costs on original scale

### Return Values

**LAP Solvers:**
```r
list(
  match = integer vector (1-based, length = nrow),
  total_cost = numeric,
  status = "optimal",
  method_used = "algorithm_name"
)
```

**Matching Functions:**
```r
list(
  pairs = tibble(left_id, right_id, distance, block_id),
  info = list(
    method = "optimal" or "greedy",
    strategy = "row_best" (greedy only),
    n_matched = integer,
    total_distance = numeric,
    n_blocks = integer (if blocking)
  ),
  # If return_diagnostics = TRUE:
  left_unmatched = tibble(...),
  right_unmatched = tibble(...),
  cost_matrix = matrix,
  distance_info = list(...)
)
```

**Balance Diagnostics:**
```r
balance_diagnostics object with:
  var_stats = tibble(variable, mean_left, mean_right, std_diff, ...),
  overall = list(mean_abs_std_diff, max_abs_std_diff, pct_large_imbalance),
  n_matched, n_unmatched_left, n_unmatched_right,
  has_blocks, block_stats
```

## Adding New Functionality

### Adding a New LAP Solver

1. Create `src/solvers/solve_foo.cpp` with `solve_foo_impl()`
2. Add forward declaration in `src/rcpp_interface.cpp`
3. Add `[[Rcpp::export]]` wrapper in `src/rcpp_interface.cpp`
4. Run `Rcpp::compileAttributes()` to update `R/RcppExports.R`
5. Add case to switch in `assignment()` ([R/assignment.R](R/assignment.R))
6. Create `tests/testthat/test-assignment-foo.R`
7. Update documentation via `devtools::document()`

### Adding Matching Features

**For preprocessing:**
- Edit [R/matching_preprocessing.R](R/matching_preprocessing.R)
- Add tests to [tests/testthat/test-matching.R](tests/testthat/test-matching.R)
- Update print methods if adding new diagnostics

**For balance diagnostics:**
- Edit [R/matching_diagnostics.R](R/matching_diagnostics.R)
- Extend `balance_diagnostics()` or add new functions
- Update print methods for new output

**For new matching algorithms:**
- Add C++ implementation in `src/solvers/`
- Export via `src/rcpp_interface.cpp`
- Add R wrapper in [R/matching_core.R](R/matching_core.R)

## Common Pitfalls

1. **Forgetting `Rcpp::compileAttributes()`** after C++ changes
   - Symptom: "could not find function" errors
   - Fix: Always run after adding/modifying `[[Rcpp::export]]`

2. **Exporting from subdirectories**
   - DON'T: Put `[[Rcpp::export]]` in `src/solvers/*.cpp`
   - DO: Forward declare and wrap in `src/rcpp_interface.cpp`

3. **0-based vs 1-based indexing**
   - Always convert at R/C++ boundary
   - C++ match vectors use -1 for unmatched, R uses 0/NA

4. **Not handling rectangular matrices**
   - Always test with nrow != ncol
   - Check auto-transpose logic

5. **Ignoring NA/Inf costs**
   - Test forbidden edges (calipers, blocking)
   - Verify cost computation skips invalid pairs

6. **Package name references**
   - OLD: lapr (deprecated)
   - NEW: couplr (current)
   - Check tests, examples, vignettes

7. **Object files in commits**
   - Clean before committing: `git clean -fdX src/`
   - Add `src/*.o` to .gitignore

## Documentation Files

**Implementation Guides:**
- [IMPLEMENTATION_STEP1.md](IMPLEMENTATION_STEP1.md) - Automatic preprocessing details
- [IMPLEMENTATION_STEP2.md](IMPLEMENTATION_STEP2.md) - Balance diagnostics details
- [MATCHING_ENHANCEMENTS.md](MATCHING_ENHANCEMENTS.md) - Feature roadmap
- [SETUP_REQUIRED.md](SETUP_REQUIRED.md) - Initial setup for greedy matching

**Package Documentation:**
- [CHANGELOG.md](CHANGELOG.md) - Detailed release notes
- [NEWS.md](NEWS.md) - User-facing changes (R convention)

**Examples:**
- [examples/auto_scale_demo.R](examples/auto_scale_demo.R) - 5 preprocessing demos
- [examples/balance_diagnostics_demo.R](examples/balance_diagnostics_demo.R) - 6 balance demos

## Special Notes

### Pixel Morphing Semantics

Complex multi-mode system ([R/morph_pixel.R](R/morph_pixel.R)):

**Modes:**
- `exact` - Full pixel LAP (< 4096 pixels)
- `color_walk` - Color quantization + spatial LAPs
- `recursive` - Multi-scale 2×2 tiling

**CRITICAL rendering semantics:**
- Assignment uses BOTH images A and B
- Renderer uses ONLY A's colors (transport-only)
- B influences WHERE pixels go, not WHAT colors
- Result is sharp (no motion blur)

### Cost Computation Guarantee

All solvers MUST use `compute_total_cost()` from [src/core/lap_utils.cpp](src/core/lap_utils.cpp):

```cpp
double total_cost = compute_total_cost(original_cost_matrix, assignment_R);
```

**Never** compute cost from transformed/reduced/scaled matrices. The cost field has precise semantics: sum of original_cost[i, match[i]] over matched rows.

### Backward Compatibility

v1.0.0 is 100% backward compatible:
- All new parameters default to previous behavior
- `auto_scale = FALSE` by default
- No breaking changes to return structures
- All 1365 pre-existing tests still pass

## Development History

- **v1.0.0 (2025-11-19):** Major release
  - Added automatic preprocessing and scaling
  - Added greedy matching algorithms
  - Added comprehensive balance diagnostics
  - Renamed from lapr to couplr
  - 1369 tests passing

- **v0.1.0:** Initial release
  - Core LAP solvers
  - Basic matching
  - Pixel morphing
