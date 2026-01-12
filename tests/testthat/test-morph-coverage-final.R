# ==============================================================================
# Final coverage tests for morph functions
# ==============================================================================

# ------------------------------------------------------------------------------
# morph_utils.R helpers
# ------------------------------------------------------------------------------

test_that(".has_namespace works", {
  # Test for packages that exist
  expect_true(couplr:::.has_namespace("base"))
  # Test for package that doesn't exist
  expect_false(couplr:::.has_namespace("nonexistent_package_xyz123"))
})

test_that(".gif_delay_from_fps converts correctly", {
  # 10 fps = 0.1 seconds = 10 hundredths
  expect_equal(couplr:::.gif_delay_from_fps(10), 10)
  # 20 fps = 0.05 seconds = 5 hundredths
  expect_equal(couplr:::.gif_delay_from_fps(20), 5)
  # 1 fps = 1 second = 100 hundredths
  expect_equal(couplr:::.gif_delay_from_fps(1), 100)
})

test_that(".clamp_rgb handles various inputs", {
  # Test with array
  arr <- array(c(-10, 0, 127, 255, 300, 128), dim = c(2, 3))
  result <- couplr:::.clamp_rgb(arr)
  expect_equal(dim(result), c(2, 3))
  expect_true(all(result >= 0 & result <= 255))
})

test_that(".to_planar_rgb and .from_planar_rgb are inverse", {
  H <- 4
  W <- 6
  original <- array(runif(H * W * 3) * 255, dim = c(H, W, 3))
  planar <- couplr:::.to_planar_rgb(original)
  reconstructed <- couplr:::.from_planar_rgb(planar, H, W)
  expect_equal(dim(reconstructed), c(H, W, 3))
  # Values should be close (may have integer conversion)
  expect_true(all(abs(reconstructed - original) < 2))
})

# ------------------------------------------------------------------------------
# cpp wrappers
# ------------------------------------------------------------------------------

test_that(".cpp_downscale works", {
  H <- 16
  W <- 16
  planar <- runif(H * W * 3) * 255
  result <- couplr:::.cpp_downscale(planar, H, W, 8, 8)
  expect_length(result, 8 * 8 * 3)
})

test_that(".cpp_upscale_assignment works", {
  H <- 8
  W <- 8
  Hs <- 4
  Ws <- 4
  # Simple identity assignment at scaled level
  assignment <- as.integer(0:(Hs * Ws - 1))
  result <- couplr:::.cpp_upscale_assignment(assignment, H, W, Hs, Ws)
  expect_length(result, H * W)
})

# ------------------------------------------------------------------------------
# downscale helpers
# ------------------------------------------------------------------------------

test_that(".downscale_both handles multiple steps", {
  H <- 32
  W <- 32
  A_planar <- runif(H * W * 3) * 255
  B_planar <- runif(H * W * 3) * 255

  result <- couplr:::.downscale_both(A_planar, B_planar, H, W, steps = 2)
  expect_true("Hs" %in% names(result))
  expect_true("Ws" %in% names(result))
  expect_true(result$Hs < H)
  expect_true(result$Ws < W)
})

# ------------------------------------------------------------------------------
# assignment helpers
# ------------------------------------------------------------------------------

test_that(".assemble_assignment handles mixed assignments", {
  i_idx <- c(1, 2, 4)
  j_idx <- c(3, 1, 2)
  result <- couplr:::.assemble_assignment(N = 5, i_idx, j_idx)
  expect_length(result, 5)
  expect_equal(result[1], 3L)
  expect_equal(result[2], 1L)
  expect_equal(result[3], -1L)  # Unassigned
  expect_equal(result[4], 2L)
})

test_that(".fill_unassigned_identity fills correctly", {
  assign <- c(3L, -1L, 1L, -1L, 2L)
  result <- couplr:::.fill_unassigned_identity(assign)
  expect_equal(result[2], 2L)  # Filled with identity
  expect_equal(result[4], 4L)  # Filled with identity
  # Original assignments preserved
  expect_equal(result[1], 3L)
  expect_equal(result[3], 1L)
  expect_equal(result[5], 2L)
})

# ------------------------------------------------------------------------------
# matching_diagnostics edge cases
# ------------------------------------------------------------------------------

test_that("balance_table returns tibble", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")
  tbl <- balance_table(balance)
  expect_s3_class(tbl, "tbl_df")
})

test_that("summary.balance_result works", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x")
  balance <- balance_diagnostics(result, left, right, vars = "x")
  summ <- summary(balance)
  expect_type(summ, "list")
})

# ------------------------------------------------------------------------------
# Additional matching core edge cases
# ------------------------------------------------------------------------------

test_that("match_couples with return_diagnostics=TRUE", {
  set.seed(123)
  left <- data.frame(id = 1:5, x = rnorm(5))
  right <- data.frame(id = 6:10, x = rnorm(5))
  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)
  expect_s3_class(result, "matching_result")
})

test_that("greedy_couples with pq strategy", {
  set.seed(123)
  left <- data.frame(id = 1:20, x = rnorm(20))
  right <- data.frame(id = 21:50, x = rnorm(30))
  result <- greedy_couples(left, right, vars = "x", strategy = "pq")
  expect_s3_class(result, "matching_result")
})

test_that("compute_distances with different distance metrics", {
  left <- data.frame(id = 1:5, x = 1:5)
  right <- data.frame(id = 6:10, x = 6:10)

  for (dist in c("euclidean", "manhattan")) {
    result <- compute_distances(left, right, vars = "x", distance = dist)
    expect_s3_class(result, "distance_object")
  }
})

# ------------------------------------------------------------------------------
# lap_solve edge cases
# ------------------------------------------------------------------------------

test_that("assignment with orlin method", {
  cost <- matrix(runif(25), 5, 5)
  result <- assignment(cost, method = "orlin")
  expect_equal(result$status, "optimal")
})

test_that("assignment with network_simplex method", {
  cost <- matrix(runif(25), 5, 5)
  result <- assignment(cost, method = "network_simplex")
  expect_equal(result$status, "optimal")
})

test_that("assignment with push_relabel method", {
  cost <- matrix(runif(25), 5, 5)
  result <- assignment(cost, method = "push_relabel")
  expect_equal(result$status, "optimal")
})

test_that("assignment with ramshaw_tarjan method", {
  cost <- matrix(runif(6 * 8), 6, 8)  # Rectangular
  result <- assignment(cost, method = "ramshaw_tarjan")
  expect_equal(result$status, "optimal")
})

test_that("assignment with cycle_cancel method", {
  cost <- matrix(runif(16), 4, 4)
  result <- assignment(cost, method = "cycle_cancel")
  expect_equal(result$status, "optimal")
})

test_that("assignment with gabow_tarjan method", {
  cost <- matrix(runif(16), 4, 4)
  result <- assignment(cost, method = "gabow_tarjan")
  expect_equal(result$status, "optimal")
})

test_that("assignment with csflow method", {
  cost <- matrix(runif(16), 4, 4)
  result <- assignment(cost, method = "csflow")
  expect_equal(result$status, "optimal")
})

test_that("assignment with csa method", {
  cost <- matrix(runif(25), 5, 5)
  result <- assignment(cost, method = "csa")
  expect_equal(result$status, "optimal")
})
