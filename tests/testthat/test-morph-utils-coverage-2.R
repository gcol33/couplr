# ==============================================================================
# Additional tests for morph_utils.R coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# Internal helper tests
# ------------------------------------------------------------------------------

test_that(".has_namespace works", {
  expect_true(couplr:::.has_namespace("base"))
  expect_false(couplr:::.has_namespace("nonexistent_package_xyz"))
})

test_that(".gif_delay_from_fps handles various inputs", {
  # Normal FPS
  expect_equal(couplr:::.gif_delay_from_fps(10), 10L)
  expect_equal(couplr:::.gif_delay_from_fps(20), 5L)

  # Edge cases
  expect_equal(couplr:::.gif_delay_from_fps(0), 10L)  # Default for invalid
  expect_equal(couplr:::.gif_delay_from_fps(-5), 10L)
  expect_equal(couplr:::.gif_delay_from_fps(NA), 10L)
  expect_equal(couplr:::.gif_delay_from_fps(Inf), 10L)
  expect_equal(couplr:::.gif_delay_from_fps(NaN), 10L)
})

test_that(".clamp_rgb clamps values correctly", {
  x <- c(-10, 50, 300)
  result <- couplr:::.clamp_rgb(x)
  expect_equal(result, c(0L, 50L, 255L))

  # With dimensions
  m <- matrix(c(-5, 128, 500, 255), 2, 2)
  result <- couplr:::.clamp_rgb(m)
  expect_equal(dim(result), c(2, 2))
  expect_true(all(result >= 0 & result <= 255))
})

# ------------------------------------------------------------------------------
# Array conversion tests
# ------------------------------------------------------------------------------

test_that(".to_planar_rgb and .from_planar_rgb are inverses", {
  skip_if_not_installed("magick")

  # Create a simple 3x4 RGB array
  H <- 3
  W <- 4
  arr <- array(sample(0:255, H * W * 3, replace = TRUE), dim = c(H, W, 3))
  storage.mode(arr) <- "integer"

  # Convert to planar and back
  planar <- couplr:::.to_planar_rgb(arr)
  expect_equal(length(planar), H * W * 3)

  recovered <- couplr:::.from_planar_rgb(planar, H, W)
  expect_equal(dim(recovered), c(H, W, 3))
})

test_that(".from_planar_rgb errors on wrong length", {
  expect_error(
    couplr:::.from_planar_rgb(1:10, 2, 3),
    "planar data has wrong length"
  )
})

test_that(".to_array_rgb works with magick images", {
  skip_if_not_installed("magick")

  img <- magick::image_blank(4, 3, color = "red")
  arr <- couplr:::.to_array_rgb(img)

  expect_equal(dim(arr), c(3, 4, 3))  # H x W x 3
  expect_true(is.integer(arr))
})

# ------------------------------------------------------------------------------
# Downscale/upscale tests
# ------------------------------------------------------------------------------

test_that(".downscale_both with no steps returns original", {
  H <- 4
  W <- 4
  N <- H * W
  A <- rep(128, N * 3)
  B <- rep(64, N * 3)

  result <- couplr:::.downscale_both(A, B, H, W, steps = 0)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
  expect_equal(result$A_s, A)
  expect_equal(result$B_s, B)
})

test_that(".downscale_both with NULL steps returns original", {
  H <- 4
  W <- 4
  N <- H * W
  A <- rep(128, N * 3)
  B <- rep(64, N * 3)

  result <- couplr:::.downscale_both(A, B, H, W, steps = NULL)

  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})

# ------------------------------------------------------------------------------
# LAP assign wrapper tests
# ------------------------------------------------------------------------------

test_that(".lap_assign works with cost matrix", {
  C <- matrix(c(1, 5, 5, 1), 2, 2)

  result <- couplr:::.lap_assign(C, method = "jv", maximize = FALSE)

  expect_equal(length(result), 2)
  expect_true(all(result >= 0))  # 0-based
})

# ------------------------------------------------------------------------------
# Palette pipeline tests
# ------------------------------------------------------------------------------

test_that(".palette_pairs_identity handles empty matches", {
  # Create info with no matching colors
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0), nrow = 1, ncol = 3),
    colorsB_rgb = matrix(c(0, 255, 0), nrow = 1, ncol = 3),
    countsA = 10,
    countsB = 10
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
})

test_that(".palette_pairs_identity handles matching colors", {
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0, 0, 255, 0), nrow = 2, ncol = 3, byrow = TRUE),
    colorsB_rgb = matrix(c(255, 0, 0, 0, 0, 255), nrow = 2, ncol = 3, byrow = TRUE),
    countsA = c(10, 5),
    countsB = c(8, 6)
  )

  result <- couplr:::.palette_pairs_identity(info)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)  # Only red matches
})

test_that(".assemble_assignment creates correct assignment", {
  N <- 5
  i_idx <- c(1, 3, 5)
  j_idx <- c(2, 4, 1)

  result <- couplr:::.assemble_assignment(N, i_idx, j_idx)

  expect_equal(length(result), 5)
  expect_equal(result[1], 2L)
  expect_equal(result[3], 4L)
  expect_equal(result[5], 1L)
  expect_equal(result[2], -1L)  # Unassigned
})

test_that(".fill_unassigned_identity fills gaps", {
  assign <- c(2L, -1L, 4L, -1L, 1L)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result[1], 2L)
  expect_equal(result[2], 2L)  # Filled with identity
  expect_equal(result[4], 4L)  # Filled with identity
})

# ------------------------------------------------------------------------------
# NULL coalesce operator tests
# ------------------------------------------------------------------------------

test_that("%||% operator works", {
  expect_equal(couplr:::`%||%`(NULL, 5), 5)
  expect_equal(couplr:::`%||%`(3, 5), 3)
  expect_equal(couplr:::`%||%`(0, 5), 0)
})

# ------------------------------------------------------------------------------
# prepare_cost_matrix (from zzz.R)
# ------------------------------------------------------------------------------

test_that("prepare_cost_matrix works with numeric matrix", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- couplr:::prepare_cost_matrix(cost, maximize = FALSE)

  expect_true(is.list(result))
})

test_that("prepare_cost_matrix with maximize", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- couplr:::prepare_cost_matrix(cost, maximize = TRUE)

  expect_true(is.list(result))
})

test_that("prepare_cost_matrix errors on non-numeric", {
  cost <- matrix(c("a", "b", "c", "d"), 2, 2)

  expect_error(
    couplr:::prepare_cost_matrix(cost),
    "must be a numeric matrix"
  )
})

test_that("prepare_cost_matrix errors on NaN", {
  cost <- matrix(c(1, NaN, 3, 4), 2, 2)

  expect_error(
    couplr:::prepare_cost_matrix(cost),
    "NaN not allowed"
  )
})
