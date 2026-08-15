# test-gabow_tarjan_solver.R
# High-level tests for lap_solve_gabow_tarjan()

library(testthat)

# check_complementary_slackness() and assignment_cost() are in helper-gabow_tarjan.R

# Helper: build col_match from row_match
build_col_match <- function(row_match, m) {
  col_match <- rep(NA_integer_, m)
  for (i in seq_along(row_match)) {
    j <- row_match[i]
    if (!is.na(j) && j >= 1 && j <= m) {
      col_match[j] <- i
    }
  }
  col_match
}

test_that("Gabow-Tarjan solves simple 3x3 matrix", {
  cost <- matrix(c(
    4, 1, 3,
    2, 0, 5,
    3, 2, 2
  ), nrow = 3, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 5)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles identity-like matrix", {
  cost <- matrix(c(
    1,   100, 100,
    100,   1, 100,
    100, 100,   1
  ), nrow = 3, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 3)
  expect_equal(res$match, c(1L, 2L, 3L))

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles maximization", {
  # Use a profit matrix where min and max assignments differ
  profit <- matrix(c(
    10,  5,  3,
     7, 12,  4,
     6,  8, 15
  ), nrow = 3, byrow = TRUE)

  # Brute-force reference min / max (no dependency on other solvers)
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

  # Sanity check: this matrix actually has different min/max
  expect_true(ref_max_cost > ref_min_cost)

  # Run Gabow–Tarjan in min and max modes
  res_gt_min <- lap_solve_gabow_tarjan(profit, maximize = FALSE)
  res_gt_max <- lap_solve_gabow_tarjan(profit, maximize = TRUE)

  # All rows should be matched
  expect_equal(res_gt_min$n_matched, 3)
  expect_equal(res_gt_max$n_matched, 3)

  gt_min_cost <- res_gt_min$total_cost
  gt_max_cost <- res_gt_max$total_cost

  # Costs must match the brute-force optimum for each objective
  expect_equal(gt_min_cost, ref_min_cost)
  expect_equal(gt_max_cost, ref_max_cost)

  # Maximization must not be worse than minimization on a profit matrix
  expect_true(gt_max_cost > gt_min_cost)

  # Optional: if you want complementary slackness here too, use row/col matches
  # and duals from the new interface. Comment out if you prefer not to check.
  #
  # build col_match from row_match for the CS check
  col_match_min <- rep(NA_integer_, ncol(profit))
  for (i in seq_len(nrow(profit))) {
    j <- res_gt_min$row_match[i]
    if (!is.na(j) && j >= 1 && j <= ncol(profit)) {
      col_match_min[j] <- i
    }
  }
  col_match_max <- rep(NA_integer_, ncol(profit))
  for (i in seq_len(nrow(profit))) {
    j <- res_gt_max$row_match[i]
    if (!is.na(j) && j >= 1 && j <= ncol(profit)) {
      col_match_max[j] <- i
    }
  }

  expect_true(check_complementary_slackness(
    profit,
    res_gt_min$row_match,
    col_match_min,
    res_gt_min$u,
    res_gt_min$v
  ))
})




test_that("Gabow-Tarjan handles 4x4 matrix", {
  cost <- matrix(c(
    10, 19,  8, 15,
    10, 18,  7, 17,
    13, 16,  9, 14,
    12, 19,  8, 18
  ), nrow = 4, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 4)
  expect_true(res$total_cost > 0)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles 5x5 matrix", {
  cost <- matrix(c(
    7, 2, 1, 9, 4,
    9, 6, 9, 5, 5,
    3, 8, 3, 1, 8,
    7, 9, 4, 2, 2,
    8, 4, 7, 4, 8
  ), nrow = 5, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 5)
  expect_true(res$total_cost > 0)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles negative costs", {
  cost <- matrix(c(
    -1, 5, 3,
     4, -2, 6,
     2, 1, -3
  ), nrow = 3, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, -6)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles zero costs", {
  cost <- matrix(0, nrow = 3, ncol = 3)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 0)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles uniform costs", {
  cost <- matrix(5, nrow = 3, ncol = 3)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 15)

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles large cost differences", {
  cost <- matrix(c(
      1, 1000, 1000,
   1000,    1, 1000,
   1000, 1000,    1
  ), nrow = 3, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  expect_equal(res$n_matched, 3)
  expect_equal(res$total_cost, 3)
  expect_equal(res$match, c(1L, 2L, 3L))

  col_match <- build_col_match(res$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res$match, col_match, res$row_duals, res$col_duals
  ))
})

test_that("Gabow-Tarjan handles rectangular matrices (4x3)", {
  cost <- matrix(c(
     1,  2,  3,
     4,  5,  6,
     7,  8,  9,
    10, 11, 12
  ), nrow = 4, byrow = TRUE)

  res <- lap_solve_gabow_tarjan(cost, maximize = FALSE)

  # Three columns can hold three rows, so one row comes back unmatched. This
  # used to report four, the fourth pointing at a padding column the caller's
  # matrix does not have.
  expect_equal(res$n_matched, 3)
  matched <- res$match[!is.na(res$match)]
  expect_true(all(matched >= 1L & matched <= ncol(cost)))
  expect_equal(length(unique(matched)), length(matched))
  expect_equal(res$total_cost, assignment(cost, method = "jv")$total_cost)
})

test_that("Gabow-Tarjan is optimal on wide problems (gcol33/couplr#31)", {
  # The 1-optimality bound holds for a matching that saturates both sides, so
  # a rectangular instance has to be padded to square. It was not, and 179 of
  # 200 random wide problems came back worse than jv.
  cost <- matrix(c(
    14,  9, 17,  3,  4, 12,
    14,  5, 14,  3, 12, 11,
    12, 13,  1, 19,  4, 17
  ), nrow = 3, byrow = TRUE)
  storage.mode(cost) <- "double"

  res <- assignment(cost, method = "gabow_tarjan")
  expect_equal(res$total_cost, 8)
  expect_true(verify_assignment(res, cost)$certified_optimal)

  skip_on_cran()
  set.seed(31)
  for (i in seq_len(40)) {
    n <- sample(3:7, 1)
    m <- n + sample(1:8, 1)
    cst <- matrix(round(stats::runif(n * m, 1, 100), 4), n, m)
    gt <- assignment(cst, method = "gabow_tarjan")
    expect_equal(gt$total_cost, assignment(cst, method = "jv")$total_cost,
                 info = paste0(n, " x ", m, ", seed 31 draw ", i))
  }
})

test_that("Gabow-Tarjan matches Hungarian on small matrices", {
  set.seed(42)
  cost <- matrix(sample(1:20, 9, replace = TRUE), nrow = 3)

  res_gt <- lap_solve_gabow_tarjan(cost, maximize = FALSE)
  res_h  <- couplr:::lap_solve_hungarian(cost, maximize = FALSE)

  cost_h <- if (!is.null(res_h$cost)) {
    res_h$cost
  } else if (!is.null(res_h$total_cost)) {
    res_h$total_cost
  } else if (!is.null(res_h$assignment)) {
    assignment_cost(cost, res_h$assignment)
  } else {
    NA_real_
  }

  expect_false(is.na(cost_h))
  expect_equal(res_gt$total_cost, cost_h, tolerance = 1e-6)

  col_match <- build_col_match(res_gt$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res_gt$match, col_match, res_gt$row_duals, res_gt$col_duals
  ))
})

test_that("Gabow-Tarjan matches JV on larger matrices", {
  set.seed(123)
  n <- 10
  cost <- matrix(sample(1:100, n * n, replace = TRUE), nrow = n)

  res_gt <- lap_solve_gabow_tarjan(cost, maximize = FALSE)
  res_jv <- couplr:::lap_solve_jv(cost, maximize = FALSE)

  cost_jv <- if (!is.null(res_jv$cost)) {
    res_jv$cost
  } else if (!is.null(res_jv$total_cost)) {
    res_jv$total_cost
  } else if (!is.null(res_jv$assignment)) {
    assignment_cost(cost, res_jv$assignment)
  } else {
    NA_real_
  }

  expect_false(is.na(cost_jv))
  expect_equal(res_gt$total_cost, cost_jv, tolerance = 1e-6)

  col_match <- build_col_match(res_gt$match, ncol(cost))

  expect_true(check_complementary_slackness(
    cost, res_gt$match, col_match, res_gt$row_duals, res_gt$col_duals
  ))
})
