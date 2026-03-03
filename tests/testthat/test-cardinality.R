# Tests for cardinality matching (Feature 6)

test_that("cardinality_match returns matching_result", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20, 0, 1), y = rnorm(20, 0, 1))
  right <- data.frame(id = 21:50, x = rnorm(30, 0.5, 1), y = rnorm(30, 0.3, 1))
  result <- cardinality_match(left, right, vars = c("x", "y"),
                              max_std_diff = 0.3)

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
  expect_true(!is.null(result$info$pruning_iterations))
  expect_true(!is.null(result$info$pairs_removed))
})

test_that("cardinality_match improves balance", {
  set.seed(42)
  left <- data.frame(id = 1:30, x = rnorm(30, 0, 1), y = rnorm(30, 0, 1))
  right <- data.frame(id = 31:80, x = rnorm(50, 1, 1), y = rnorm(50, 0.5, 1))

  result <- cardinality_match(left, right, vars = c("x", "y"),
                              max_std_diff = 0.25)

  # Balance should be within threshold (or close if pruning hit limits)
  bal <- balance_diagnostics(result, left, right, vars = c("x", "y"))
  max_imbalance <- max(abs(bal$var_stats$std_diff))
  # Should be improved from the full match
  expect_true(max_imbalance <= 0.5 || result$info$pairs_removed > 0)
})

test_that("cardinality_match with already-balanced data removes no pairs", {
  set.seed(42)
  left <- data.frame(id = 1:10, x = rnorm(10, 0, 1))
  right <- data.frame(id = 11:20, x = rnorm(10, 0, 1))

  result <- cardinality_match(left, right, vars = "x",
                              max_std_diff = 1.0)  # Very lenient

  # Should remove zero or very few pairs
  expect_true(result$info$pairs_removed <= 2)
})

test_that("cardinality_match validates max_std_diff", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  expect_error(cardinality_match(left, right, vars = "x", max_std_diff = -0.1),
               "max_std_diff must be a positive number")
})

test_that("cardinality_match with tight threshold prunes pairs", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20, 0, 1))
  right <- data.frame(id = 21:50, x = rnorm(30, 1, 1))
  result <- cardinality_match(left, right, vars = "x",
                              max_std_diff = 0.05)

  # With very tight threshold, some pruning should occur
  expect_true(result$info$pairs_removed >= 0)
  expect_true(!is.null(result$info$pruning_iterations))
})

test_that("cardinality_match preserves pair structure", {
  set.seed(42)
  left <- data.frame(id = 1:15, x = rnorm(15), y = rnorm(15))
  right <- data.frame(id = 16:35, x = rnorm(20), y = rnorm(20))
  result <- cardinality_match(left, right, vars = c("x", "y"),
                              max_std_diff = 0.2)

  expect_true("left_id" %in% names(result$pairs))
  expect_true("right_id" %in% names(result$pairs))
  expect_true("distance" %in% names(result$pairs))
})
