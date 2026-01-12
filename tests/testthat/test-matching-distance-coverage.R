# ==============================================================================
# Additional tests for matching distance functions to increase coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# compute_distance_matrix tests
# ------------------------------------------------------------------------------

test_that("compute_distance_matrix validates column count", {
  left <- matrix(1:6, nrow = 2, ncol = 3)
  right <- matrix(1:4, nrow = 2, ncol = 2)  # Different column count

  expect_error(
    couplr:::compute_distance_matrix(left, right),
    "same number of columns"
  )
})

test_that("compute_distance_matrix computes euclidean distance", {
  left <- matrix(c(0, 0, 3, 0), nrow = 2, ncol = 2, byrow = TRUE)
  right <- matrix(c(0, 4, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)

  result <- couplr:::compute_distance_matrix(left, right, distance = "euclidean")

  expect_equal(dim(result), c(2, 2))
  expect_equal(result[1, 1], 4)  # (0,0) to (0,4)
  expect_equal(result[2, 2], 3)  # (3,0) to (0,0)
})

test_that("compute_distance_matrix computes L2 distance (alias)", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 4), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "l2")

  expect_equal(result[1, 1], 5)  # 3-4-5 triangle
})

test_that("compute_distance_matrix computes manhattan distance", {
  left <- matrix(c(0, 0, 1, 1), nrow = 2, ncol = 2, byrow = TRUE)
  right <- matrix(c(2, 3, 0, 0), nrow = 2, ncol = 2, byrow = TRUE)

  result <- couplr:::compute_distance_matrix(left, right, distance = "manhattan")

  expect_equal(result[1, 1], 5)  # |0-2| + |0-3| = 5
  expect_equal(result[2, 1], 3)  # |1-2| + |1-3| = 3
  expect_equal(result[1, 2], 0)  # |0-0| + |0-0| = 0
})

test_that("compute_distance_matrix computes L1 distance (alias)", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 4), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "l1")

  expect_equal(result[1, 1], 7)  # |3| + |4|
})

test_that("compute_distance_matrix computes cityblock distance (alias)", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 4), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "cityblock")

  expect_equal(result[1, 1], 7)
})

test_that("compute_distance_matrix computes squared euclidean distance", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 4), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "squared_euclidean")

  expect_equal(result[1, 1], 25)  # 9 + 16
})

test_that("compute_distance_matrix computes sqeuclidean distance (alias)", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 4), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "sqeuclidean")

  expect_equal(result[1, 1], 25)
})

test_that("compute_distance_matrix computes sq distance (alias)", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 4), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "sq")

  expect_equal(result[1, 1], 25)
})

test_that("compute_distance_matrix computes chebyshev distance", {
  left <- matrix(c(0, 0, 5, 5), nrow = 2, ncol = 2, byrow = TRUE)
  right <- matrix(c(3, 10, 2, 2), nrow = 2, ncol = 2, byrow = TRUE)

  result <- couplr:::compute_distance_matrix(left, right, distance = "chebyshev")

  expect_equal(result[1, 1], 10)  # max(|0-3|, |0-10|) = 10
  expect_equal(result[2, 2], 3)   # max(|5-2|, |5-2|) = 3
})

test_that("compute_distance_matrix computes chebychev distance (alternate spelling)", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 10), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "chebychev")

  expect_equal(result[1, 1], 10)
})

test_that("compute_distance_matrix computes maximum distance (alias)", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 10), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "maximum")

  expect_equal(result[1, 1], 10)
})

test_that("compute_distance_matrix computes max distance (alias)", {
  left <- matrix(c(0, 0), nrow = 1)
  right <- matrix(c(3, 10), nrow = 1)

  result <- couplr:::compute_distance_matrix(left, right, distance = "max")

  expect_equal(result[1, 1], 10)
})

test_that("compute_distance_matrix computes mahalanobis distance", {
  set.seed(123)
  left <- matrix(rnorm(20), nrow = 5, ncol = 4)
  right <- matrix(rnorm(20), nrow = 5, ncol = 4)

  result <- couplr:::compute_distance_matrix(left, right, distance = "mahalanobis")

  expect_equal(dim(result), c(5, 5))
  expect_true(all(result >= 0))
  expect_true(all(is.finite(result)))
})

test_that("compute_distance_matrix computes maha distance (alias)", {
  set.seed(123)
  left <- matrix(rnorm(20), nrow = 5, ncol = 4)
  right <- matrix(rnorm(20), nrow = 5, ncol = 4)

  result <- couplr:::compute_distance_matrix(left, right, distance = "maha")

  expect_equal(dim(result), c(5, 5))
})

test_that("compute_distance_matrix errors on singular covariance", {
  # Create data with linearly dependent columns
  left <- matrix(c(1, 2, 3, 4, 2, 4, 6, 8), nrow = 2, ncol = 4, byrow = TRUE)
  right <- matrix(c(1, 2, 3, 4, 2, 4, 6, 8), nrow = 2, ncol = 4, byrow = TRUE)

  expect_error(
    couplr:::compute_distance_matrix(left, right, distance = "mahalanobis"),
    "singular"
  )
})

test_that("compute_distance_matrix rejects unknown metric", {
  left <- matrix(1:4, nrow = 2)
  right <- matrix(5:8, nrow = 2)

  expect_error(
    couplr:::compute_distance_matrix(left, right, distance = "unknown_metric"),
    "Unknown distance metric"
  )
})

test_that("compute_distance_matrix accepts custom function", {
  custom_dist <- function(left, right) {
    n <- nrow(left)
    m <- nrow(right)
    result <- matrix(1.0, n, m)  # Constant distance of 1
    result
  }

  left <- matrix(1:4, nrow = 2)
  right <- matrix(5:8, nrow = 2)

  result <- couplr:::compute_distance_matrix(left, right, distance = custom_dist)

  expect_equal(dim(result), c(2, 2))
  expect_true(all(result == 1.0))
})

# ------------------------------------------------------------------------------
# apply_scaling tests
# ------------------------------------------------------------------------------

test_that("apply_scaling returns originals when method is FALSE", {
  left <- matrix(1:6, nrow = 2)
  right <- matrix(7:12, nrow = 2)

  result <- couplr:::apply_scaling(left, right, method = FALSE)

  expect_equal(result$left, left)
  expect_equal(result$right, right)
  expect_null(result$params)
})

test_that("apply_scaling returns originals when method is 'none'", {
  left <- matrix(1:6, nrow = 2)
  right <- matrix(7:12, nrow = 2)

  result <- couplr:::apply_scaling(left, right, method = "none")

  expect_equal(result$left, left)
  expect_equal(result$right, right)
})

test_that("apply_scaling returns originals when method is NULL", {
  left <- matrix(1:6, nrow = 2)
  right <- matrix(7:12, nrow = 2)

  result <- couplr:::apply_scaling(left, right, method = NULL)

  expect_equal(result$left, left)
  expect_equal(result$right, right)
})

test_that("apply_scaling standardizes with method = TRUE", {
  left <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  right <- matrix(c(7, 8, 9, 10, 11, 12), nrow = 3, ncol = 2)

  result <- couplr:::apply_scaling(left, right, method = TRUE)

  # Check that columns have mean ~0 and sd ~1
  combined <- rbind(result$left, result$right)
  expect_true(all(abs(colMeans(combined)) < 1e-10))
  expect_equal(result$params$method, "standardize")
})

test_that("apply_scaling standardizes with method = 'standardize'", {
  left <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
  right <- matrix(c(7, 8, 9, 10, 11, 12), nrow = 3, ncol = 2)

  result <- couplr:::apply_scaling(left, right, method = "standardize")

  expect_equal(result$params$method, "standardize")
  expect_true("means" %in% names(result$params))
  expect_true("sds" %in% names(result$params))
})

test_that("apply_scaling standardizes with method = 'scale'", {
  left <- matrix(c(1, 2), nrow = 2, ncol = 1)
  right <- matrix(c(3, 4), nrow = 2, ncol = 1)

  result <- couplr:::apply_scaling(left, right, method = "scale")

  expect_equal(result$params$method, "standardize")
})

test_that("apply_scaling handles constant columns in standardize", {
  left <- matrix(c(5, 5, 1, 2), nrow = 2, ncol = 2)  # First col constant
  right <- matrix(c(5, 5, 3, 4), nrow = 2, ncol = 2)

  result <- couplr:::apply_scaling(left, right, method = "standardize")

  # Should handle without error (sd = 1 for constant cols)
  expect_true(is.numeric(result$left))
  expect_true(all(is.finite(result$left)))
})

test_that("apply_scaling applies range scaling", {
  left <- matrix(c(0, 10, 0, 20), nrow = 2, ncol = 2)
  right <- matrix(c(5, 5, 10, 10), nrow = 2, ncol = 2)

  result <- couplr:::apply_scaling(left, right, method = "range")

  expect_equal(result$params$method, "range")
  expect_true("mins" %in% names(result$params))
  expect_true("maxs" %in% names(result$params))

  # Values should be in [0, 1]
  combined <- rbind(result$left, result$right)
  expect_true(all(combined >= 0))
  expect_true(all(combined <= 1))
})

test_that("apply_scaling applies minmax scaling (alias)", {
  left <- matrix(c(0, 100), nrow = 2, ncol = 1)
  right <- matrix(c(50, 50), nrow = 2, ncol = 1)

  result <- couplr:::apply_scaling(left, right, method = "minmax")

  expect_equal(result$params$method, "range")
})

test_that("apply_scaling handles constant columns in range", {
  left <- matrix(c(5, 5, 0, 10), nrow = 2, ncol = 2)  # First col constant
  right <- matrix(c(5, 5, 5, 5), nrow = 2, ncol = 2)

  result <- couplr:::apply_scaling(left, right, method = "range")

  expect_true(all(is.finite(result$left)))
})

test_that("apply_scaling applies robust scaling", {
  left <- matrix(c(1, 2, 3, 4, 100, 5, 6, 7, 8, 9), nrow = 5, ncol = 2)  # With outlier
  right <- matrix(c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19), nrow = 5, ncol = 2)

  result <- couplr:::apply_scaling(left, right, method = "robust")

  expect_equal(result$params$method, "robust")
  expect_true("medians" %in% names(result$params))
  expect_true("mads" %in% names(result$params))
})

test_that("apply_scaling handles constant columns in robust", {
  left <- matrix(c(5, 5, 5, 1, 2, 3), nrow = 3, ncol = 2)  # First col constant
  right <- matrix(c(5, 5, 5, 4, 5, 6), nrow = 3, ncol = 2)

  result <- couplr:::apply_scaling(left, right, method = "robust")

  expect_true(all(is.finite(result$left)))
})

test_that("apply_scaling rejects unknown method", {
  left <- matrix(1:4, nrow = 2)
  right <- matrix(5:8, nrow = 2)

  expect_error(
    couplr:::apply_scaling(left, right, method = "unknown_method"),
    "Unknown scaling method"
  )
})

test_that("apply_scaling removes scale attributes", {
  left <- matrix(c(1, 2, 3, 4), nrow = 2)
  right <- matrix(c(5, 6, 7, 8), nrow = 2)

  result <- couplr:::apply_scaling(left, right, method = "standardize")

  expect_null(attr(result$left, "scaled:center"))
  expect_null(attr(result$left, "scaled:scale"))
  expect_null(attr(result$right, "scaled:center"))
  expect_null(attr(result$right, "scaled:scale"))
})

# ------------------------------------------------------------------------------
# apply_weights tests
# ------------------------------------------------------------------------------

test_that("apply_weights returns original when weights is NULL", {
  mat <- matrix(1:6, nrow = 2)

  result <- couplr:::apply_weights(mat, NULL)

  expect_equal(result, mat)
})

test_that("apply_weights returns original when all weights are 1", {
  mat <- matrix(1:6, nrow = 2)

  result <- couplr:::apply_weights(mat, c(1, 1, 1))

  expect_equal(result, mat)
})

test_that("apply_weights applies sqrt of weights", {
  mat <- matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2)
  weights <- c(4, 9)  # sqrt(4) = 2, sqrt(9) = 3

  result <- couplr:::apply_weights(mat, weights)

  expect_equal(result[, 1], c(2, 2))  # 1 * sqrt(4)
  expect_equal(result[, 2], c(3, 3))  # 1 * sqrt(9)
})

test_that("apply_weights validates weight length", {
  mat <- matrix(1:6, nrow = 2, ncol = 3)
  weights <- c(1, 2)  # Wrong length

  expect_error(
    couplr:::apply_weights(mat, weights),
    "must match number of columns"
  )
})

# ------------------------------------------------------------------------------
# build_cost_matrix tests
# ------------------------------------------------------------------------------

test_that("build_cost_matrix integrates all components", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3), y = c(10, 20, 30))
  right <- data.frame(id = 4:6, x = c(1.5, 2.5, 3.5), y = c(15, 25, 35))

  result <- couplr:::build_cost_matrix(left, right, vars = c("x", "y"),
                                        distance = "euclidean", scale = "standardize")

  expect_equal(dim(result), c(3, 3))
  expect_equal(attr(result, "distance"), "euclidean")
  expect_true(!is.null(attr(result, "scaling")))
})

test_that("build_cost_matrix works without scaling", {
  left <- data.frame(x = c(0, 3), y = c(0, 0))
  right <- data.frame(x = c(0, 0), y = c(4, 0))

  result <- couplr:::build_cost_matrix(left, right, vars = c("x", "y"),
                                        distance = "euclidean", scale = FALSE)

  expect_equal(result[1, 1], 4)  # (0,0) to (0,4)
  expect_null(attr(result, "scaling"))
})

test_that("build_cost_matrix applies weights", {
  left <- data.frame(x = c(0, 1), y = c(0, 1))
  right <- data.frame(x = c(1, 0), y = c(1, 0))

  # Weight x twice as much as y
  result <- couplr:::build_cost_matrix(left, right, vars = c("x", "y"),
                                        weights = c(x = 4, y = 1),
                                        distance = "euclidean", scale = FALSE)

  # With weights, x differences are multiplied by sqrt(4) = 2
  expect_true(is.numeric(result))
})
