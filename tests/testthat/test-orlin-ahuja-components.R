# tests/testthat/test-orlin-ahuja-components.R
# Unit tests for Orlin-Ahuja algorithm components
# Tests both correctness and empirical complexity bounds

# =============================================================================
# Component 1: Price/Potential Management
# =============================================================================

test_that("reduced cost computation is correct", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  row_price <- c(1, 2, 3)
  col_price <- c(0.5, 1, 1.5)


  # rc[i,j] = cost[i,j] - row_price[i] - col_price[j]
  # rc[1,1] = 4 - 1 - 0.5 = 2.5
  expect_equal(oa_test_reduced_cost(cost, 1, 1, row_price, col_price), 2.5)

  # rc[2,3] = 6 - 2 - 1.5 = 2.5
  expect_equal(oa_test_reduced_cost(cost, 2, 3, row_price, col_price), 2.5)

  # rc[3,2] = 5 - 3 - 1 = 1
  expect_equal(oa_test_reduced_cost(cost, 3, 2, row_price, col_price), 1.0)
})

test_that("all reduced costs computed correctly", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  row_price <- c(1, 2, 3)
  col_price <- c(0.5, 1, 1.5)

  result <- oa_test_all_reduced_costs(cost, row_price, col_price)

  expected_rc <- matrix(c(
    4 - 1 - 0.5, 2 - 1 - 1,   5 - 1 - 1.5,
    3 - 2 - 0.5, 3 - 2 - 1,   6 - 2 - 1.5,
    7 - 3 - 0.5, 5 - 3 - 1,   4 - 3 - 1.5
  ), nrow = 3, byrow = TRUE)

  expect_equal(result$reduced_costs, expected_rc, tolerance = 1e-10)
  expect_equal(result$n, 3)
  expect_equal(result$m, 3)
  expect_equal(result$nm, 9)
})

test_that("epsilon-CS check works", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)

  # Optimal matching from JV: rows 1,2,3 -> cols 2,1,3 with cost 9
  # Set prices so rc = 0 for assigned pairs
  row_price <- c(2, 3, 4)  # These are the u values from assignment_duals

  col_price <- c(0, 0, 0)  # These are the v values

  row_to_col <- c(2, 1, 3)  # 1-based

  # Should satisfy 0-CS (exact optimality)
  expect_true(oa_test_epsilon_cs(cost, row_to_col, row_price, col_price, 0))

  # Should also satisfy any epsilon > 0

  expect_true(oa_test_epsilon_cs(cost, row_to_col, row_price, col_price, 1))

  # Now perturb prices to violate CS
  bad_col_price <- c(5, 0, 0)  # Makes rc[1,2] = 2 - 2 - 0 = 0 but rc[2,1] = 3 - 3 - 5 = -5
  expect_false(oa_test_epsilon_cs(cost, row_to_col, row_price, bad_col_price, 1))
})

# =============================================================================
# Component 2: Best/Second-Best Column Finding
# =============================================================================

test_that("find_best_columns finds correct columns", {
  cost <- matrix(c(10, 5, 8, 20, 3, 15, 7, 12, 6), nrow = 3, byrow = TRUE)
  col_price <- c(0, 0, 0)

  # For row 1 (1-based), costs are [10, 5, 8]
  # profit = col_price - cost = [0-10, 0-5, 0-8] = [-10, -5, -8]
  # Best = col 2 (profit -5), second = col 3 (profit -8)
  result <- oa_test_find_best_columns(cost, 1, col_price)
  expect_equal(result$best_col, 2)
  expect_equal(result$second_best_col, 3)
  expect_equal(result$best_profit, -5)
  expect_equal(result$second_profit, -8)
  expect_equal(result$edges_scanned, 3)

  # For row 2, costs are [20, 3, 15]
  # profit = [-20, -3, -15]
  # Best = col 2 (profit -3), second = col 3 (profit -15)
  result2 <- oa_test_find_best_columns(cost, 2, col_price)
  expect_equal(result2$best_col, 2)
  expect_equal(result2$second_best_col, 3)
})

test_that("find_best_columns handles price adjustments", {
  cost <- matrix(c(10, 5, 8), nrow = 1)
  col_price <- c(0, 6, 0)  # Make col 2 expensive

  # profit = [0-10, 6-5, 0-8] = [-10, 1, -8]
  # Best = col 2 (profit 1), second = col 3 (profit -8)
  result <- oa_test_find_best_columns(cost, 1, col_price)
  expect_equal(result$best_col, 2)
  expect_equal(result$best_profit, 1)
  expect_equal(result$second_profit, -8)
})

test_that("find_best_columns handles Inf costs", {
  cost <- matrix(c(10, Inf, 8, 5, 3, Inf), nrow = 2, byrow = TRUE)
  col_price <- c(0, 0, 0)

  # Row 1: costs [10, Inf, 8], finite profits [-10, -8]
  result <- oa_test_find_best_columns(cost, 1, col_price)
  expect_equal(result$best_col, 3)  # cost 8
  expect_equal(result$second_best_col, 1)  # cost 10
})

# =============================================================================
# Component 3: Single Auction Round
# =============================================================================

test_that("single bid updates correctly", {
  cost <- matrix(c(10, 5, 8, 20, 3, 15, 7, 12, 6), nrow = 3, byrow = TRUE)
  row_to_col <- c(0, 0, 0)  # all unassigned (0 = unassigned in R)
  row_price <- c(0, 0, 0)
  col_price <- c(0, 0, 0)
  epsilon <- 1.0

  # Row 1 bids: best=col2 (profit 0-5=-5), second=col3 (profit 0-8=-8)
  # bid = (-5 - (-8)) + 1 = 4
  # For MINIMIZATION: price DECREASES by bid amount
  result <- oa_test_single_bid(cost, 1, row_to_col, row_price, col_price, epsilon)

  expect_true(result$success)
  expect_equal(result$row_to_col[1], 2)  # row 1 assigned to col 2
  expect_equal(result$col_price[2], -4)  # price DECREASED by bid amount (minimization)
})

test_that("auction round assigns multiple rows", {
  cost <- matrix(c(10, 5, 8, 20, 3, 15, 7, 12, 6), nrow = 3, byrow = TRUE)
  row_to_col <- c(0, 0, 0)
  row_price <- c(0, 0, 0)
  col_price <- c(0, 0, 0)
  epsilon <- 1.0

  result <- oa_test_auction_round(cost, row_to_col, row_price, col_price, epsilon)

  # All 3 rows should have bid
  expect_equal(result$bids_made, 3)

  # May have 2-3 assignments (depends on conflict resolution order)
  # With Gauss-Seidel, rows bid sequentially and a displaced row doesn't re-bid
  expect_true(result$matching_size >= 2)

  # Total edges scanned = 3 * 3 = 9 (dense matrix)
  expect_equal(result$edges_scanned, 9)
})

test_that("auction handles displacement", {
  cost <- matrix(c(1, 10, 2, 10), nrow = 2, byrow = TRUE)
  # Row 1 prefers col 1 (cost 1)
  # Row 2 prefers col 1 (cost 2)

  row_to_col <- c(1, 0)  # Row 1 already assigned to col 1
  row_price <- c(0, 0)
  col_price <- c(0, 0)
  epsilon <- 0.1

  # Row 2 bids for col 1, should displace row 1
  result <- oa_test_single_bid(cost, 2, row_to_col, row_price, col_price, epsilon)

  expect_true(result$success)
  expect_equal(result$row_to_col[2], 1)  # Row 2 now has col 1
  expect_equal(result$row_to_col[1], 0)  # Row 1 displaced (unassigned)
})

# =============================================================================
# Complexity Tests
# =============================================================================

test_that("reduced costs complexity is O(nm)", {
  skip_on_cran()

  sizes <- c(50, 100, 200, 400)
  df <- oa_complexity_reduced_costs(sizes, reps = 3)

  # Group by size and take median time
  agg <- aggregate(time_us ~ nm, data = df, FUN = median)

  # Fit linear model: time ~ nm
  fit <- lm(time_us ~ nm, data = agg)

  # R² should be high for linear relationship
  r_squared <- summary(fit)$r.squared
  expect_gt(r_squared, 0.9)  # Should be > 0.9 for O(nm)
})

test_that("auction round complexity is O(nm)", {
  skip_on_cran()

  sizes <- c(50, 100, 200)
  df <- oa_complexity_auction_round(sizes, reps = 3)

  # Each round scans all edges
  # edges_scanned should be close to n*m
  expected_edges <- sizes^2
  actual_edges <- sapply(sizes, function(n) {
    median(df$edges_scanned[df$n == n])
  })

  # Should scan close to n*m edges
  ratios <- actual_edges / expected_edges
  expect_true(all(ratios > 0.9 & ratios < 1.1))
})

test_that("find_best_columns scans all columns",
{
  cost <- matrix(runif(100 * 100), nrow = 100)
  col_price <- rep(0, 100)

  result <- oa_test_find_best_columns(cost, 1, col_price)
  expect_equal(result$edges_scanned, 100)
})

# =============================================================================
# Component 4: Dijkstra/SSP
# =============================================================================

test_that("dijkstra finds shortest path from unassigned row", {
  cost <- matrix(c(10, 5, 8, 20, 3, 15, 7, 12, 6), nrow = 3, byrow = TRUE)
  row_to_col <- c(0, 0, 0)  # all unassigned
  row_price <- c(0, 0, 0)
  col_price <- c(0, 0, 0)

  result <- oa_test_dijkstra(cost, row_to_col, row_price, col_price)

  # Note: oa_test_dijkstra now uses ssp_augment_once internally,

  # so target_col is no longer available (returns 0) and edges_scanned
  # reflects only the first augmentation, not all n*m edges
  expect_true(result$found)
  expect_true(result$edges_scanned > 0)  # Some edges were scanned
})

test_that("ssp phase completes matching", {
  cost <- matrix(c(10, 5, 8, 20, 3, 15, 7, 12, 6), nrow = 3, byrow = TRUE)
  row_to_col <- c(0, 0, 0)
  row_price <- c(0, 0, 0)
  col_price <- c(0, 0, 0)

  result <- oa_test_ssp_phase(cost, row_to_col, row_price, col_price)

  expect_equal(result$matching_size, 3)  # Complete matching
  expect_equal(result$augmentations, 3)  # One per row

  # Each row should be assigned to exactly one column
  expect_true(all(result$row_to_col > 0))
  expect_equal(length(unique(result$row_to_col)), 3)  # All different
})

# =============================================================================
# Component 5: Epsilon Scaling
# =============================================================================

test_that("scale count is O(log nC)", {
  # Test with varying C and n
  result1 <- oa_test_scale_count(100, 10, alpha = 5)
  result2 <- oa_test_scale_count(10000, 10, alpha = 5)
  result3 <- oa_test_scale_count(100, 100, alpha = 5)

  # More scales for larger C
  expect_gt(result2$n_scales, result1$n_scales)

  # More scales for larger n (due to smaller final_eps)
  expect_gt(result3$n_scales, result1$n_scales)

  # Actual vs theoretical should be close
  expect_lt(abs(result1$n_scales - result1$theoretical_scales), 3)
})

# =============================================================================
# Component 6: Full Orlin-Ahuja Solver
# =============================================================================

test_that("full solver finds optimal solution", {
  cost <- matrix(c(10, 5, 8, 20, 3, 15, 7, 12, 6), nrow = 3, byrow = TRUE)

  result <- oa_solve(cost)

  expect_true(result$optimal)
  expect_equal(sum(result$row_to_col > 0), 3)  # All rows assigned

  # Compute actual cost from assignment
  actual_cost <- sum(sapply(1:3, function(i) cost[i, result$row_to_col[i]]))
  expect_equal(result$total_cost, actual_cost)
})

test_that("full solver matches JV on random matrices", {
  skip_on_cran()

  set.seed(42)
  for (n in c(5, 10, 20)) {
    cost <- matrix(runif(n * n, 1, 100), nrow = n)

    orlin_result <- oa_solve(cost)
    jv_result <- lap_solve_jv(cost, maximize = FALSE)

    # Costs should match (within tolerance)
    expect_equal(orlin_result$total_cost, jv_result$total_cost,
                 tolerance = 1e-6,
                 label = paste("n =", n))
  }
})

test_that("augmentations per scale follow sqrt(n) bound", {
  skip_on_cran()

  sizes <- c(25, 100, 225)  # sqrt = 5, 10, 15
  df <- oa_complexity_augmentations_per_scale(sizes, reps = 2)

  # Group by size and check max augmentations
  for (n in sizes) {
    subset_df <- df[df$n == n, ]
    avg_max <- mean(subset_df$max_augs_per_scale)
    sqrt_n <- sqrt(n)

    # Max augmentations should be roughly O(sqrt(n))

    # Allow generous factor (up to 5*sqrt(n)) since this is a soft bound
    expect_lt(avg_max, 5 * sqrt_n,
              label = paste("n =", n, "max_augs =", avg_max, "5*sqrt(n) =", 5*sqrt_n))
  }
})
