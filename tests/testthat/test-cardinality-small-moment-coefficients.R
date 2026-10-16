test_that("small mean-difference coefficients preserve certified sample size", {
  for (exponent in c(-900, -40, -31, -10, 0, 10, 900)) {
    left <- data.frame(id = paste0("l", 1:8), z = 1:8,
      x = c(rep(2^exponent, 4), rep(0, 4)))
    right <- data.frame(id = paste0("r", 1:8), z = 1:8, x = 0)
    fit <- cardinality_match(left, right, vars = "z",
      moments = list(var = "x", stat = "mean_diff", max = 0),
      node_limit = 1000, time_limit = Inf)
    result <- glance(fit)
    expect_equal(result$n_pairs, 4L, info = paste("exponent", exponent))
    expect_equal(result$best_possible, 4L)
    expect_equal(result$gap, 0L)
    expect_equal(result$total_distance, 0)
    expect_true(result$certified)
    pairs <- join_matched(fit, left, right)
    expect_true(all(pairs$x_left == 0))
  }
})

test_that("nonzero distance optimization closes under equivalent cost scales", {
  for (exponent in c(-10, 0, 10, 30)) {
    unit <- 2^exponent
    left <- data.frame(id = paste0("l", 1:8),
      z = c(100:103, 0, 0, 4, 4), x = c(rep(2^-40, 4), rep(0, 4)))
    right <- data.frame(id = paste0("r", 1:8),
      z = c(0:3, 20, 30, 40, 50), x = 0)
    distance <- function(a, b) abs(outer(a[, 1], b[, 1], "-")) * unit
    fit <- cardinality_match(left, right, vars = "z", distance = distance,
      moments = list(var = "x", stat = "mean_diff", max = 0),
      node_limit = 1000, time_limit = Inf)
    result <- glance(fit)
    expect_equal(result$n_pairs, 4L)
    expect_equal(result$best_possible, 4L)
    expect_equal(result$gap, 0L)
    expect_equal(result$total_distance, 4 * unit)
    expect_true(result$certified)
  }
})
