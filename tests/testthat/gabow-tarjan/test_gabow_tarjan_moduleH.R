# test-gabow_tarjan_moduleH.R
# Tests for Module H: bit-scaling outer loop (solve_gabow_tarjan_inner)

library(testthat)

# 1-feasible complementary slackness check (Gabow–Tarjan style)
#
# Conditions for cost matrix C:
#  - For all finite edges (i,j): u[i] + v[j] <= C[i,j] + 1
#  - For matched edges (i,j):   u[i] + v[j] >= C[i,j]
#
check_complementary_slackness <- function(cost, row_match, col_match, u, v, tol = 1e-6) {
  n <- nrow(cost)
  m <- ncol(cost)
  BIG_INT <- 1e15

  if (length(u) != n || length(v) != m) return(FALSE)
  if (length(row_match) != n || length(col_match) != m) return(FALSE)

  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      cij <- cost[i, j]
      if (is.finite(cij) && cij < BIG_INT) {
        sum_duals <- u[i] + v[j]

        # Upper bound: u + v <= c + 1
        if (sum_duals - (cij + 1) > tol) {
          return(FALSE)
        }

        # Lower bound on matched edges: u + v >= c
        if (!is.na(row_match[i]) &&
            row_match[i] == j &&
            !is.na(col_match[j]) &&
            col_match[j] == i) {
          if (cij - sum_duals > tol) {
            return(FALSE)
          }
        }
      }
    }
  }

  TRUE
}

# Helper to call Gabow–Tarjan solver (Module H indirectly)
call_module_h <- function(cost, maximize = FALSE) {
  # Use the exported C++ wrapper
  result <- lap_solve_gabow_tarjan(cost, maximize = maximize)

  n <- nrow(cost)
  m <- ncol(cost)

  row_match <- result$assignment

  # Build col_match from row_match (1-based)
  col_match <- rep(NA_integer_, m)
  for (i in seq_len(n)) {
    j <- row_match[i]
    if (!is.na(j) && j >= 1 && j <= m) {
      col_match[j] <- i
    }
  }

  list(
    row_match = row_match,
    col_match = col_match,
    y_u       = result$row_duals,
    y_v       = result$col_duals,
    cost      = result$cost,
    n_matched = result$n_matched
  )
}

# Helper to compute assignment cost
assignment_cost <- function(cost, row_match) {
  n <- length(row_match)
  total <- 0
  for (i in seq_len(n)) {
    j <- row_match[i]
    if (!is.na(j) && j >= 1 && j <= ncol(cost)) {
      total <- total + cost[i, j]
    }
  }
  total
}

test_that("Gabow-Tarjan solves simple 3x3 matrix", {
  cost <- matrix(c(
    4, 1, 3,
    2, 0, 5,
    3, 2, 2
  ), nrow = 3, byrow = TRUE)

  result <- call_module_h(cost)

  # Perfect matching
  expect_equal(sum(result$row_match > 0, na.rm = TRUE), 3)

  # Known optimal cost: 5
  expect_equal(result$cost, 5)

  expect_true(check_complementary_slackness(
    cost, result$row_match, result$col_match, result$y_u, result$y_v
  ))
})

test_that("Gabow-Tarjan handles identity-like matrix", {
  cost <- matrix(c(
    1,   100, 100,
    100,   1, 100,
    100, 100,   1
  ), nrow = 3, byrow = TRUE)

  result <- call_module_h(cost)

  expect_equal(sum(result$row_match > 0, na.rm = TRUE), 3)
  expect_equal(result$cost, 3)
  expect_equal(result$row_match, c(1L, 2L, 3L))

  expect_true(check_complementary_slackness(
    cost, result$row_match, result$col_match, result$y_u, result$y_v
  ))
})

test_that("Gabow-Tarjan handles maximization", {
  profit <- matrix(c(
    10,  5,  3,
     7, 12,  4,
     6,  8, 15
  ), nrow = 3, byrow = TRUE)

  # Brute-force reference min / max on the profit matrix
  perms <- list(
    c(1L, 2L, 3L),
    c(1L, 3L, 2L),
    c(2L, 1L, 3L),
    c(2L, 3L, 1L),
    c(3L, 1L, 2L),
    c(3L, 2L, 1L)
  )

  costs <- vapply(
    perms,
    function(p) sum(profit[cbind(1:3, p)]),
    numeric(1)
  )

  ref_min_cost <- min(costs)
  ref_max_cost <- max(costs)

  expect_true(ref_max_cost > ref_min_cost)

  # Call Module H via the public Gabow–Tarjan solver
  res_min <- call_module_h(profit, maximize = FALSE)
  res_max <- call_module_h(profit, maximize = TRUE)

  gt_min_cost <- res_min$cost
  gt_max_cost <- res_max$cost

  expect_equal(gt_min_cost, ref_min_cost)
  expect_equal(gt_max_cost, ref_max_cost)
  expect_true(gt_max_cost > gt_min_cost)

  # 1-feasible complementary slackness for both solutions
  # Minimization: directly on profit
  expect_true(check_complementary_slackness(
    profit,
    res_min$row_match,
    res_min$col_match,
    res_min$y_u,
    res_min$y_v
  ))

  # Maximization: inner GT runs on cost = -profit, with *unflipped* duals
  # Our wrapper returns duals with sign flipped back, so revert them here.
  expect_true(check_complementary_slackness(
    -profit,
    res_max$row_match,
    res_max$col_match,
    -res_max$y_u,
    -res_max$y_v
  ))
})

test_that("Module H handles 4x4 matrix", {
  cost <- matrix(c(
    10, 19,  8, 15,
    10, 18,  7, 17,
    13, 16,  9, 14,
    12, 19,  8, 18
  ), nrow = 4, byrow = TRUE)

  result <- call_module_h(cost)

  expect_equal(sum(result$row_match > 0, na.rm = TRUE), 4)
  expect_true(result$cost > 0)

  expect_true(check_complementary_slackness(
    cost, result$row_match, result$col_match, result$y_u, result$y_v
  ))
})

test_that("Module H handles 5x5 matrix", {
  cost <- matrix(c(
    7, 2, 1, 9, 4,
    9, 6, 9, 5, 5,
    3, 8, 3, 1, 8,
    7, 9, 4, 2, 2,
    8, 4, 7, 4, 8
  ), nrow = 5, byrow = TRUE)

  result <- call_module_h(cost)

  expect_equal(sum(result$row_match > 0, na.rm = TRUE), 5)
  expect_true(result$cost > 0)

  expect_true(check_complementary_slackness(
    cost, result$row_match, result$col_match, result$y_u, result$y_v
  ))
})

test_that("Module H handles negative costs", {
  cost <- matrix(c(
    -1,  5,  3,
     4, -2,  6,
     2,  1, -3
  ), nrow = 3, byrow = TRUE)

  result <- call_module_h(cost)

  expect_equal(sum(result$row_match > 0, na.rm = TRUE), 3)

  # Optimal is diagonal: -1 + -2 + -3 = -6
  expect_equal(result$cost, -6)

  expect_true(check_complementary_slackness(
    cost, result$row_match, result$col_match, result$y_u, result$y_v
  ))
})

test_that("Module H handles zero costs", {
  cost <- matrix(0, nrow = 3, ncol = 3)

  result <- call_module_h(cost)

  expect_equal(sum(result$row_match > 0, na.rm = TRUE), 3)
  expect_equal(result$cost, 0)

  expect_true(check_complementary_slackness(
    cost, result$row_match, result$col_match, result$y_u, result$y_v
  ))
})

test_that("Module H handles uniform costs", {
  cost <- matrix(5, nrow = 3, ncol = 3)

  result <- call_module_h(cost)

  expect_equal(sum(result$row_match > 0, na.rm = TRUE), 3)
  expect_equal(result$cost, 15)  # any perfect matching

  expect_true(check_complementary_slackness(
    cost, result$row_match, result$col_match, result$y_u, result$y_v
  ))
})

test_that("Module H handles large cost differences (wide dynamic range)", {
  cost <- matrix(c(
      1, 1000, 1000,
   1000,    1, 1000,
   1000, 1000,    1
  ), nrow = 3, byrow = TRUE)

  result <- call_module_h(cost)

  expect_equal(sum(result$row_match > 0, na.rm = TRUE), 3)
  expect_equal(result$cost, 3)
  expect_equal(result$row_match, c(1L, 2L, 3L))

  expect_true(check_complementary_slackness(
    cost, result$row_match, result$col_match, result$y_u, result$y_v
  ))
})

test_that("Module H handles rectangular matrices (4x3)", {
  cost <- matrix(c(
     1,  2,  3,
     4,  5,  6,
     7,  8,  9,
    10, 11, 12
  ), nrow = 4, byrow = TRUE)

  result <- call_module_h(cost)

  # 4 rows matched (3 real + 1 dummy)
  expect_equal(result$n_matched, 4)

  real_matches  <- sum(result$row_match <= 3, na.rm = TRUE)
  dummy_matches <- sum(result$row_match >  3, na.rm = TRUE)

  expect_equal(real_matches, 3)
  expect_equal(dummy_matches, 1)
})

test_that("Module H produces same result as Hungarian on small matrices", {
  set.seed(42)
  cost <- matrix(sample(1:20, 9, replace = TRUE), nrow = 3)

  result_gt <- call_module_h(cost)
  result_h  <- lapr:::lap_solve_hungarian(cost, maximize = FALSE)

  # Robustly extract Hungarian cost
  cost_h <- if (!is.null(result_h$cost)) {
    result_h$cost
  } else if (!is.null(result_h$total_cost)) {
    result_h$total_cost
  } else if (!is.null(result_h$assignment)) {
    assignment_cost(cost, result_h$assignment)
  } else {
    NA_real_
  }

  expect_false(is.na(cost_h))
  expect_equal(result_gt$cost, cost_h, tolerance = 1e-6)

  expect_true(check_complementary_slackness(
    cost, result_gt$row_match, result_gt$col_match, result_gt$y_u, result_gt$y_v
  ))
})

test_that("Module H matches JV on larger matrices", {
  set.seed(123)
  n <- 10
  cost <- matrix(sample(1:100, n * n, replace = TRUE), nrow = n)

  result_gt <- call_module_h(cost)
  result_jv <- lapr:::lap_solve_jv(cost, maximize = FALSE)

  cost_jv <- if (!is.null(result_jv$cost)) {
    result_jv$cost
  } else if (!is.null(result_jv$total_cost)) {
    result_jv$total_cost
  } else if (!is.null(result_jv$assignment)) {
    assignment_cost(cost, result_jv$assignment)
  } else {
    NA_real_
  }

  expect_false(is.na(cost_jv))
  expect_equal(result_gt$cost, cost_jv, tolerance = 1e-6)

  expect_true(check_complementary_slackness(
    cost, result_gt$row_match, result_gt$col_match, result_gt$y_u, result_gt$y_v
  ))
})
