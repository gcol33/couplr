# ==============================================================================
# Tests for Mahalanobis Distance Fixes
# ==============================================================================

test_that("Mahalanobis distance works with well-conditioned data", {
  set.seed(42)
  left <- data.frame(
    id = 1:10,
    x = rnorm(10, 0, 1),
    y = rnorm(10, 0, 1)
  )
  right <- data.frame(
    id = 11:20,
    x = rnorm(10, 0.5, 1),
    y = rnorm(10, 0.5, 1)
  )

  result <- match_couples(left, right, vars = c("x", "y"),
                          distance = "mahalanobis")
  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})


test_that("Mahalanobis detects near-singular covariance", {
  # Create data with perfectly collinear variables
  left_mat <- matrix(c(1:10, 2 * (1:10)), ncol = 2)
  right_mat <- matrix(c(11:20, 2 * (11:20)), ncol = 2)

  expect_error(
    compute_distance_matrix(left_mat, right_mat, distance = "mahalanobis"),
    "singular or near-singular"
  )
})


test_that("Mahalanobis accepts custom sigma", {
  set.seed(42)
  left_mat <- matrix(rnorm(20), ncol = 2)
  right_mat <- matrix(rnorm(20), ncol = 2)

  # Identity matrix should give same result as euclidean
  sigma <- diag(2)
  maha_dist <- compute_distance_matrix(left_mat, right_mat,
                                       distance = "mahalanobis",
                                       sigma = sigma)
  eucl_dist <- compute_distance_matrix(left_mat, right_mat,
                                       distance = "euclidean")
  expect_equal(maha_dist, eucl_dist, tolerance = 1e-10)
})


test_that("Mahalanobis sigma validation catches wrong dimensions", {
  left_mat <- matrix(rnorm(20), ncol = 2)
  right_mat <- matrix(rnorm(20), ncol = 2)
  bad_sigma <- diag(3)

  expect_error(
    compute_distance_matrix(left_mat, right_mat,
                            distance = "mahalanobis", sigma = bad_sigma),
    "sigma must be a 2 x 2 matrix"
  )
})


test_that("Mahalanobis vectorized matches original loop result", {
  set.seed(123)
  n <- 5
  left_mat <- matrix(rnorm(n * 3), ncol = 3)
  right_mat <- matrix(rnorm(n * 3), ncol = 3)

  # Compute with our function
  result <- compute_distance_matrix(left_mat, right_mat,
                                    distance = "mahalanobis")

  # Compute reference manually
  combined <- rbind(left_mat, right_mat)
  inv_cov <- solve(cov(combined))
  expected <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      diff <- left_mat[i, ] - right_mat[j, ]
      expected[i, j] <- sqrt(as.numeric(t(diff) %*% inv_cov %*% diff))
    }
  }

  expect_equal(result, expected, tolerance = 1e-10)
})


test_that("sigma parameter propagates through match_couples", {
  set.seed(42)
  left <- data.frame(
    id = 1:8,
    x = rnorm(8),
    y = rnorm(8)
  )
  right <- data.frame(
    id = 9:16,
    x = rnorm(8),
    y = rnorm(8)
  )

  # Custom sigma (diagonal = no cross-correlation)
  sigma <- diag(c(2, 0.5))
  result <- match_couples(left, right, vars = c("x", "y"),
                          distance = "mahalanobis", sigma = sigma)
  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
})


test_that("sigma parameter propagates through greedy_couples", {
  set.seed(42)
  left <- data.frame(
    id = 1:8,
    x = rnorm(8),
    y = rnorm(8)
  )
  right <- data.frame(
    id = 9:16,
    x = rnorm(8),
    y = rnorm(8)
  )

  sigma <- diag(2)
  result <- greedy_couples(left, right, vars = c("x", "y"),
                           distance = "mahalanobis", sigma = sigma)
  expect_s3_class(result, "matching_result")
})
