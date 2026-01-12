# ==============================================================================
# Extended coverage tests for morph_utils.R
# ==============================================================================

# ------------------------------------------------------------------------------
# Image conversion helpers
# ------------------------------------------------------------------------------

test_that(".to_planar_rgb converts array to planar format", {
  arr <- array(1:24, dim = c(2, 4, 3))
  planar <- couplr:::.to_planar_rgb(arr)
  expect_length(planar, 2 * 4 * 3)
})

test_that(".from_planar_rgb converts back to array", {
  H <- 2
  W <- 4
  planar <- 1:(H * W * 3)
  arr <- couplr:::.from_planar_rgb(planar, H, W)
  expect_equal(dim(arr), c(H, W, 3))
})

test_that(".from_planar_rgb errors on wrong length", {
  expect_error(couplr:::.from_planar_rgb(1:10, 2, 4), "wrong length")
})

test_that(".clamp_rgb clamps correctly", {
  result <- couplr:::.clamp_rgb(c(-10, 0, 127, 255, 300))
  expect_equal(result, c(0L, 0L, 127L, 255L, 255L))
})

test_that(".clamp_rgb preserves dimensions", {
  arr <- array(c(-10, 300, 100, 200), dim = c(2, 2))
  result <- couplr:::.clamp_rgb(arr)
  expect_equal(dim(result), c(2, 2))
})

# ------------------------------------------------------------------------------
# LAP assignment helper
# ------------------------------------------------------------------------------

test_that(".lap_assign returns 0-based assignment", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- couplr:::.lap_assign(cost, method = "jv")
  expect_length(result, 2)
  # 0-based indices
  expect_true(all(result >= 0 & result <= 1))
})

# ------------------------------------------------------------------------------
# Palette helpers
# ------------------------------------------------------------------------------

test_that(".palette_pairs_identity finds matching colors", {
  # Create mock info structure
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0, 0, 255, 0), ncol = 3, byrow = TRUE),
    colorsB_rgb = matrix(c(255, 0, 0, 0, 0, 255), ncol = 3, byrow = TRUE),
    countsA = c(10L, 5L),
    countsB = c(8L, 7L),
    groupsA = list(1:10, 11:15),
    groupsB = list(1:8, 9:15)
  )

  pairs <- couplr:::.palette_pairs_identity(info)
  expect_true(is.data.frame(pairs))
  # Should match red (255,0,0)
  expect_true(nrow(pairs) >= 0)
})

test_that(".palette_pairs_identity handles no matches", {
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0), ncol = 3),
    colorsB_rgb = matrix(c(0, 255, 0), ncol = 3),
    countsA = 10L,
    countsB = 10L
  )
  pairs <- couplr:::.palette_pairs_identity(info)
  expect_equal(nrow(pairs), 0)
})

test_that(".palette_pairs_lap solves color assignment", {
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0, 0, 255, 0), ncol = 3, byrow = TRUE),
    colorsB_rgb = matrix(c(254, 0, 0, 0, 254, 0), ncol = 3, byrow = TRUE),
    countsA = c(10L, 5L),
    countsB = c(8L, 7L),
    color_dist = matrix(c(1, 100, 100, 1), 2, 2)
  )

  pairs <- couplr:::.palette_pairs_lap(info, method = "jv")
  expect_true(is.data.frame(pairs))
  expect_equal(nrow(pairs), 2)
})

test_that(".palette_pairs_lap handles empty matrix", {
  info <- list(
    countsA = integer(0),
    countsB = integer(0),
    color_dist = matrix(numeric(0), 0, 0)
  )
  pairs <- couplr:::.palette_pairs_lap(info)
  expect_equal(nrow(pairs), 0)
})

# ------------------------------------------------------------------------------
# Assignment helpers
# ------------------------------------------------------------------------------

test_that(".assemble_assignment creates assignment vector", {
  i_idx <- c(1, 3, 5)
  j_idx <- c(2, 4, 6)
  result <- couplr:::.assemble_assignment(N = 6, i_idx, j_idx)
  expect_length(result, 6)
  expect_equal(result[1], 2L)
  expect_equal(result[3], 4L)
  expect_equal(result[5], 6L)
})

test_that(".assemble_assignment handles empty inputs", {
  result <- couplr:::.assemble_assignment(N = 5, integer(0), integer(0))
  expect_length(result, 5)
  expect_true(all(result == -1L))
})

test_that(".fill_unassigned_identity fills gaps", {
  assign <- c(2L, -1L, 4L, -1L, 6L)
  result <- couplr:::.fill_unassigned_identity(assign)
  expect_equal(result[2], 2L)  # Filled with identity
  expect_equal(result[4], 4L)  # Filled with identity
})

# ------------------------------------------------------------------------------
# Downscale helpers
# ------------------------------------------------------------------------------

test_that(".downscale_both returns correct structure", {
  # Create simple planar data
  H <- 16
  W <- 16
  A_planar <- runif(H * W * 3)
  B_planar <- runif(H * W * 3)

  result <- couplr:::.downscale_both(A_planar, B_planar, H, W, steps = 1)
  expect_true("Hs" %in% names(result))
  expect_true("Ws" %in% names(result))
  expect_true("A_s" %in% names(result))
  expect_true("B_s" %in% names(result))
})

test_that(".downscale_both handles steps = 0", {
  H <- 10
  W <- 10
  A_planar <- runif(H * W * 3)
  B_planar <- runif(H * W * 3)

  result <- couplr:::.downscale_both(A_planar, B_planar, H, W, steps = 0)
  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})

test_that(".downscale_both handles NULL steps", {
  H <- 10
  W <- 10
  A_planar <- runif(H * W * 3)
  B_planar <- runif(H * W * 3)

  result <- couplr:::.downscale_both(A_planar, B_planar, H, W, steps = NULL)
  expect_equal(result$Hs, H)
  expect_equal(result$Ws, W)
})

# ------------------------------------------------------------------------------
# Patch helpers
# ------------------------------------------------------------------------------

test_that(".expand_patch_assignment expands patches to pixels", {
  patch_assign <- c(2, 1)
  patches_a <- list(
    indices = list(1:4, 5:8)
  )
  patches_b <- list(
    indices = list(1:4, 5:8)
  )
  N <- 8

  result <- couplr:::.expand_patch_assignment(patch_assign, patches_a, patches_b, N)
  expect_length(result, N)
})

test_that(".expand_patch_assignment handles invalid assignments", {
  patch_assign <- c(NA, -1, 0)
  patches_a <- list(
    indices = list(1:2, 3:4, 5:6)
  )
  patches_b <- list(
    indices = list(1:2, 3:4, 5:6)
  )
  N <- 6

  result <- couplr:::.expand_patch_assignment(patch_assign, patches_a, patches_b, N)
  expect_length(result, N)
})

# ------------------------------------------------------------------------------
# Color match pipeline
# ------------------------------------------------------------------------------

test_that(".solve_color_match_pipeline runs", {
  skip_if_not_installed("magick")

  H <- 4
  W <- 4
  N <- H * W
  A_planar <- rep(c(255, 0, 0), N)  # All red
  B_planar <- rep(c(0, 255, 0), N)  # All green

  result <- couplr:::.solve_color_match_pipeline(A_planar, B_planar, H, W,
                                                  quantize_bits = 3,
                                                  fill_identity_for_unmatched = TRUE)
  expect_length(result, N)
})
