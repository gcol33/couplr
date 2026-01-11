# Test Coverage Analysis for couplr

## Executive Summary

The `couplr` package has **489 tests across 39 test files**, covering approximately **8,955 lines of test code**. The test suite is comprehensive for the core LAP (Linear Assignment Problem) algorithms but has gaps in several supporting modules. This document identifies areas for test coverage improvement.

## Current Test Coverage Overview

### Well-Tested Areas

| Module | Test File(s) | Coverage Quality |
|--------|-------------|------------------|
| LAP Solvers (18 algorithms) | `test-assignment-*.R` | Excellent |
| Gabow-Tarjan Algorithm | `test-gabow_tarjan_*.R` (10 files) | Excellent |
| K-best Solutions | `test-kbest-lawler.R`, `test-kbest-murty.R` | Good |
| Core Matching | `test-matching.R` | Good |
| Pixel Morphing | `test-pixel-morph.R` | Good |
| Sinkhorn Algorithm | `test-sinkhorn.R` | Good |
| Cost Matrix Preparation | `test-prepare-cost-matrix.R` | Good |

### Areas Needing Improvement

## 1. Batch Processing (`R/lap_solve_batch.R`)

**Current state:** Tested indirectly via `test-assign.R` but lacks dedicated tests.

**Recommended tests:**
- Parallel execution with `n_threads > 1`
- Edge cases: empty groups, single-element problems
- Error handling for malformed 3D arrays
- Print method for `lap_solve_batch_result`
- Memory behavior with large batches

```r
# Example tests to add
test_that("lap_solve_batch handles parallel execution", {
  costs <- lapply(1:10, function(i) matrix(runif(100), 10, 10))
  result <- lap_solve_batch(costs, n_threads = 2)
  expect_equal(length(unique(result$problem_id)), 10)
})

test_that("lap_solve_batch handles single-element problems", {
  costs <- list(matrix(5, 1, 1))
  result <- lap_solve_batch(costs)
  expect_equal(result$cost[1], 5)
})
```

## 2. Parallel Matching (`R/matching_parallel.R`)

**Current state:** No dedicated tests. Internal functions are untested.

**Recommended tests:**
- `can_parallelize()` with/without future package
- `setup_parallel()` and `restore_parallel()` cleanup
- `parallel_lapply()` correctness
- `match_blocks_parallel()` results consistency
- `greedy_blocks_parallel()` results consistency
- Graceful fallback when parallel packages unavailable

```r
# Example tests to add
test_that("can_parallelize returns FALSE when future unavailable", {
  # Mock missing future package
  expect_type(can_parallelize(), "logical")
})

test_that("parallel matching produces same results as sequential", {
  # Create test data and compare parallel vs sequential results
})
```

## 3. Matching Constraints (`R/matching_constraints.R`)

**Current state:** Tested indirectly via `test-matching.R` but internal functions untested.

**Recommended tests:**
- `apply_max_distance()` with various thresholds
- `apply_calipers()` with multiple variables
- `mark_forbidden_pairs()` edge cases
- `has_valid_pairs()` and `count_valid_pairs()` accuracy
- Error handling for invalid inputs (negative max_distance)

```r
# Example tests to add
test_that("apply_max_distance handles edge cases", {
  cost <- matrix(c(1, 5, 10, 2), 2, 2)
  result <- couplr:::apply_max_distance(cost, max_distance = 3)
  expect_true(result[1, 2] > 1e15)  # Should be forbidden
  expect_equal(result[1, 1], 1)  # Should be unchanged
})
```

## 4. Blocking Functions (`R/matching_blocks.R`)

**Current state:** `matchmaker()` is tested but internal helpers are not.

**Recommended tests:**
- `assign_blocks_cluster()` with different clustering methods
- `filter_blocks()` with various filtering criteria
- `summarize_blocks()` output format
- Error messages for missing block_by/block_vars
- Drop imbalanced blocks functionality

```r
# Example tests to add
test_that("cluster blocking uses specified method", {
  left <- data.frame(id = 1:20, x = rnorm(20), y = rnorm(20))
  right <- data.frame(id = 21:40, x = rnorm(20), y = rnorm(20))

  result_kmeans <- matchmaker(left, right, block_type = "cluster",
                              block_vars = c("x", "y"), n_blocks = 3)
  result_hclust <- matchmaker(left, right, block_type = "cluster",
                              block_vars = c("x", "y"), n_blocks = 3,
                              block_method = "hclust")

  expect_equal(result_kmeans$info$n_blocks_kept, 3)
  expect_equal(result_hclust$info$n_blocks_kept, 3)
})
```

## 5. Utility Functions (`R/utils.R`)

**Current state:** Some utilities tested but gaps exist.

**Recommended tests:**
- `get_total_cost()` for batch and kbest results
- `get_method_used()` for batch results
- `as_assignment_matrix()` with empty results
- `validate_cost_data()` error messages

```r
# Example tests to add
test_that("get_method_used works with batch results", {
  costs <- list(matrix(1:4, 2, 2), matrix(5:8, 2, 2))
  result <- lap_solve_batch(costs)
  methods <- get_method_used(result)
  expect_type(methods, "character")
})
```

## 6. Morphing Utilities (`R/morph_tiling.R`, `R/morph_utils.R`)

**Current state:** Main `pixel_morph` function tested but internal helpers untested.

**Recommended tests:**
- `.generate_square_tiles()` coverage verification
- `.solve_tile_lap()` correctness
- `.analyze_tiling()` statistics accuracy
- Image conversion helpers (`.to_array_rgb`, `.from_planar_rgb`)
- `.solve_color_walk_pipeline()` with various color distributions

```r
# Example tests to add
test_that("square tiling covers entire image", {
  tiles <- couplr:::.generate_square_tiles(W = 10, H = 10, P = 3)
  covered <- matrix(FALSE, 10, 10)
  for (tile in tiles) {
    for (dx in 0:(tile$size-1)) {
      for (dy in 0:(tile$size-1)) {
        covered[tile$y0 + dy + 1, tile$x0 + dx + 1] <- TRUE
      }
    }
  }
  expect_true(all(covered))
})
```

## 7. Distance Caching (`R/matching_distance_cache.R`)

**Current state:** Tested via `test-matching.R` but edge cases not covered.

**Recommended tests:**
- `diagnose_distance_matrix()` output
- Distance object with constraints applied multiple times
- Memory efficiency with very large matrices
- Serialization/deserialization of distance objects

## 8. Error Path Testing

**Current state:** Happy paths well tested, error paths less so.

**Recommended tests across all modules:**
- Invalid input types (wrong class, NULL, NA)
- Boundary conditions (empty data, single element)
- Resource exhaustion scenarios
- Interrupt handling

```r
# Example tests to add
test_that("match_couples errors on empty data", {
  left <- data.frame(id = integer(0), x = numeric(0))
  right <- data.frame(id = 1:5, x = rnorm(5))
  expect_error(match_couples(left, right, vars = "x"), "at least one row")
})

test_that("assignment errors on NaN values", {
  cost <- matrix(c(1, NaN, 3, 4), 2, 2)
  expect_error(assignment(cost), "NaN")
})
```

## 9. Print/Summary Methods

**Current state:** Some print methods tested, others not.

**Methods needing tests:**
- `print.distance_object()` - partially tested
- `print.matchmaker_result()` - not tested
- `print.preprocessing_result()` - not tested
- `print.variable_health()` - not tested
- `summary.balance_diagnostics()` - not tested

## 10. Integration Tests

**Current state:** Unit tests good, end-to-end tests sparse.

**Recommended integration tests:**
- Full workflow: matchmaker -> match_couples -> balance_diagnostics -> join_matched
- Large-scale matching with parallel processing
- Pixel morphing with real image files (not just arrays)
- Batch processing with mixed problem types

## Priority Recommendations

### High Priority (Security/Correctness)
1. **Error handling tests** - Ensure invalid inputs don't cause crashes
2. **Constraint application tests** - Verify calipers and max_distance work correctly
3. **Parallel execution tests** - Confirm thread-safety and result consistency

### Medium Priority (Robustness)
4. **Batch processing edge cases** - Empty groups, large batches
5. **Clustering-based blocking** - Different methods and edge cases
6. **Distance caching** - Ensure cached results are correctly applied

### Lower Priority (Polish)
7. **Print methods** - Visual output formatting
8. **Utility functions** - Helper function edge cases
9. **Internal morphing functions** - Performance-oriented helpers

## Metrics to Track

Consider adding these coverage metrics to CI:
- Line coverage target: >80%
- Branch coverage target: >70%
- Function coverage target: >90%

## Suggested Test File Structure

```
tests/testthat/
├── test-assignment-*.R          # (existing) LAP algorithms
├── test-matching.R              # (existing) Core matching
├── test-matching-blocks.R       # NEW: Blocking functions
├── test-matching-parallel.R     # NEW: Parallel processing
├── test-matching-constraints.R  # NEW: Constraint application
├── test-batch-processing.R      # NEW: Batch LAP solving
├── test-distance-cache.R        # NEW: Distance caching
├── test-morph-tiling.R          # NEW: Tiling algorithms
├── test-print-methods.R         # NEW: Output formatting
├── test-error-handling.R        # NEW: Error paths
└── test-integration.R           # NEW: End-to-end workflows
```

## Conclusion

The couplr package has a solid test foundation with excellent coverage of core algorithms. The main gaps are in:
1. Supporting infrastructure (parallel processing, batching)
2. Error handling paths
3. Internal helper functions
4. Print/summary methods

Addressing the high-priority items would significantly improve reliability, while the medium and lower priority items would improve maintainability and user experience.
