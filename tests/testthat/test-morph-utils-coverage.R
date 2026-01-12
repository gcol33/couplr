# ==============================================================================
# Additional tests for morph utility functions to increase coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# .gif_delay_from_fps edge cases
# ------------------------------------------------------------------------------

test_that(".gif_delay_from_fps handles edge cases", {
  # NaN and Inf should trigger default 10 fps (delay = 10 centiseconds)
  expect_equal(couplr:::.gif_delay_from_fps(NaN), 10L)
  expect_equal(couplr:::.gif_delay_from_fps(Inf), 10L)
  # Zero or negative should also default
  expect_equal(couplr:::.gif_delay_from_fps(0), 10L)
  expect_equal(couplr:::.gif_delay_from_fps(-5), 10L)
})

test_that(".gif_delay_from_fps handles fractional fps", {
  # 25.5 fps should round to 26
  expect_equal(couplr:::.gif_delay_from_fps(25.5), 4L)  # 100/26 = 3.8 -> 4
  # 15.4 fps should round to 15
  expect_equal(couplr:::.gif_delay_from_fps(15.4), 7L)  # 100/15 = 6.67 -> 7
})

# ------------------------------------------------------------------------------
# .to_array_rgb edge cases
# ------------------------------------------------------------------------------

test_that(".to_array_rgb handles different magick color formats", {
  skip_if_not_installed("magick")

  # Blue image
  img <- magick::image_blank(2, 2, color = "blue")
  result <- couplr:::.to_array_rgb(img)
  expect_equal(dim(result), c(2, 2, 3))
  expect_true(all(result[,,3] == 255))  # Blue channel
  expect_true(all(result[,,1] == 0))    # Red channel

  # White image
  img <- magick::image_blank(2, 2, color = "white")
  result <- couplr:::.to_array_rgb(img)
  expect_true(all(result == 255))

  # Black image
  img <- magick::image_blank(2, 2, color = "black")
  result <- couplr:::.to_array_rgb(img)
  expect_true(all(result == 0))
})

test_that(".to_array_rgb handles hex colors", {
  skip_if_not_installed("magick")

  img <- magick::image_blank(2, 2, color = "#FF8800")
  result <- couplr:::.to_array_rgb(img)
  expect_equal(dim(result), c(2, 2, 3))
  expect_true(all(result[,,1] == 255))  # Red
  expect_true(all(result[,,2] == 136))  # Green (0x88 = 136)
  expect_true(all(result[,,3] == 0))    # Blue
})

# ------------------------------------------------------------------------------
# .clamp_rgb additional edge cases
# ------------------------------------------------------------------------------

test_that(".clamp_rgb handles 3D arrays", {
  arr <- array(c(-100, 0, 128, 300, 50, 200, -10, 260), dim = c(2, 2, 2))
  result <- couplr:::.clamp_rgb(arr)

  expect_equal(dim(result), c(2, 2, 2))
  expect_true(all(result >= 0L))
  expect_true(all(result <= 255L))
})

test_that(".clamp_rgb handles single value", {
  expect_equal(couplr:::.clamp_rgb(300), 255L)
  expect_equal(couplr:::.clamp_rgb(-100), 0L)
  expect_equal(couplr:::.clamp_rgb(128), 128L)
})

# ------------------------------------------------------------------------------
# .palette_pairs_lap tests
# ------------------------------------------------------------------------------

test_that(".palette_pairs_lap matches colors by LAP", {
  info <- list(
    colorsA_rgb = matrix(c(255, 0, 0,
                           0, 255, 0), nrow = 2, ncol = 3, byrow = TRUE),
    colorsB_rgb = matrix(c(250, 5, 0,  # Close to red
                           5, 250, 5), nrow = 2, ncol = 3, byrow = TRUE),  # Close to green
    countsA = c(10, 5),
    countsB = c(8, 7),
    color_dist = matrix(c(0.1, 0.9,
                          0.9, 0.1), nrow = 2, ncol = 2)
  )

  result <- couplr:::.palette_pairs_lap(info, method = "hungarian")

  expect_equal(nrow(result), 2)
  expect_true(all(c("ia", "ib", "k") %in% names(result)))
})

test_that(".palette_pairs_lap handles empty matrices", {
  info <- list(
    countsA = integer(0),
    countsB = integer(0),
    color_dist = matrix(nrow = 0, ncol = 0)
  )

  result <- couplr:::.palette_pairs_lap(info)
  expect_equal(nrow(result), 0)
})

# ------------------------------------------------------------------------------
# .patch_cost_matrix tests
# ------------------------------------------------------------------------------

test_that(".patch_cost_matrix computes cost with color and spatial", {
  patches_a <- list(
    colors = matrix(c(255, 0, 0, 0, 255, 0), nrow = 2, ncol = 3, byrow = TRUE),
    centers = matrix(c(0, 0, 10, 10), nrow = 2, ncol = 2, byrow = TRUE)
  )
  patches_b <- list(
    colors = matrix(c(250, 5, 5, 5, 250, 5), nrow = 2, ncol = 3, byrow = TRUE),
    centers = matrix(c(1, 1, 11, 11), nrow = 2, ncol = 2, byrow = TRUE)
  )

  result <- couplr:::.patch_cost_matrix(patches_a, patches_b, alpha = 1, beta = 0.1, H = 20, W = 20)

  expect_equal(dim(result), c(2, 2))
  expect_true(is.numeric(result))
  expect_true(all(is.finite(result)))
})

test_that(".patch_cost_matrix handles missing H and W", {
  # Use 2x2 patches to get a proper matrix (1x1 returns scalar)
  patches_a <- list(
    colors = matrix(c(255, 0, 0, 0, 255, 0), nrow = 2, ncol = 3, byrow = TRUE),
    centers = matrix(c(5, 5, 15, 15), nrow = 2, ncol = 2, byrow = TRUE)
  )
  patches_b <- list(
    colors = matrix(c(250, 10, 10, 10, 250, 10), nrow = 2, ncol = 3, byrow = TRUE),
    centers = matrix(c(6, 6, 16, 16), nrow = 2, ncol = 2, byrow = TRUE)
  )

  result <- couplr:::.patch_cost_matrix(patches_a, patches_b, alpha = 1, beta = 0.1)

  expect_equal(dim(result), c(2, 2))
  expect_true(is.numeric(result))
})

# ------------------------------------------------------------------------------
# .expand_patch_assignment tests
# ------------------------------------------------------------------------------

test_that(".expand_patch_assignment expands patches to pixels", {
  patch_assign <- list(2L, 1L)  # Patch 1 -> B patch 2, Patch 2 -> B patch 1
  patches_a <- list(
    indices = list(c(1L, 2L), c(3L, 4L))
  )
  patches_b <- list(
    indices = list(c(5L, 6L), c(7L, 8L))
  )
  N <- 4

  result <- couplr:::.expand_patch_assignment(patch_assign, patches_a, patches_b, N)

  expect_equal(length(result), 4)
  expect_equal(result[1], 7L)  # A index 1 -> B index from patch 2
  expect_equal(result[2], 8L)
  expect_equal(result[3], 5L)  # A index 3 -> B index from patch 1
  expect_equal(result[4], 6L)
})

test_that(".expand_patch_assignment handles missing assignments", {
  patch_assign <- list(NA, 1L)
  patches_a <- list(
    indices = list(c(1L, 2L), c(3L, 4L))
  )
  patches_b <- list(
    indices = list(c(5L, 6L))
  )
  N <- 4

  result <- couplr:::.expand_patch_assignment(patch_assign, patches_a, patches_b, N)

  expect_equal(result[1], -1L)  # Not assigned
  expect_equal(result[2], -1L)
  expect_equal(result[3], 5L)   # Assigned
  expect_equal(result[4], 6L)
})

# ------------------------------------------------------------------------------
# .build_spatial_assignments_for_pairs tests
# ------------------------------------------------------------------------------

test_that(".build_spatial_assignments_for_pairs handles empty pairs", {
  info <- list(groupsA = list(), groupsB = list())
  pairs <- data.frame(ia = integer(), ib = integer(), k = integer())

  result <- couplr:::.build_spatial_assignments_for_pairs(info, pairs, H = 4, W = 4)

  expect_equal(length(result$i_idx), 0)
  expect_equal(length(result$j_idx), 0)
})

# ------------------------------------------------------------------------------
# .lap_assign edge cases
# ------------------------------------------------------------------------------

test_that(".lap_assign handles larger matrices", {
  cost <- matrix(runif(25), 5, 5)

  result <- couplr:::.lap_assign(cost, method = "hungarian")

  expect_equal(length(result), 5)
  expect_true(all(result >= 0))
  expect_true(all(result <= 4))
  expect_equal(length(unique(result)), 5)  # Bijection
})

test_that(".lap_assign handles rectangular matrices", {
  cost <- matrix(runif(12), 3, 4)

  result <- couplr:::.lap_assign(cost, method = "jv")

  expect_equal(length(result), 3)
  expect_true(all(result >= 0))
  expect_true(all(result <= 3))
})

# ------------------------------------------------------------------------------
# .exact_cost_and_solve tests
# ------------------------------------------------------------------------------

test_that(".exact_cost_and_solve computes and solves assignment", {
  H <- 2
  W <- 2
  N <- H * W
  A_planar <- rep(c(255, 0, 0), each = N)  # All red
  B_planar <- c(rep(c(255, 0, 0), each = 1), rep(c(0, 255, 0), each = 1),
                rep(c(0, 0, 255), each = 1), rep(c(128, 128, 128), each = 1))
  # Reshape B_planar to be planar format
  B_planar <- c(255, 0, 0, 128, 0, 255, 0, 128, 0, 0, 255, 128)

  result <- couplr:::.exact_cost_and_solve(A_planar, B_planar, H, W, alpha = 1, beta = 0)

  expect_equal(length(result), N)
  expect_true(all(result >= 1))
  expect_true(all(result <= N))
})

# ------------------------------------------------------------------------------
# .assemble_assignment edge cases
# ------------------------------------------------------------------------------

test_that(".assemble_assignment handles mismatched lengths", {
  N <- 5
  i_idx <- c(1L, 2L, 3L)
  j_idx <- c(4L, 5L)  # Shorter than i_idx

  result <- couplr:::.assemble_assignment(N, i_idx, j_idx)

  expect_equal(length(result), N)
  # Only first 2 should be assigned
  expect_true(result[1] > 0)
  expect_true(result[2] > 0)
  expect_equal(result[3], -1L)
})

test_that(".assemble_assignment handles large indices", {
  N <- 10
  i_idx <- c(1L, 5L, 10L)
  j_idx <- c(2L, 6L, 9L)

  result <- couplr:::.assemble_assignment(N, i_idx, j_idx)

  expect_equal(result[1], 2L)
  expect_equal(result[5], 6L)
  expect_equal(result[10], 9L)
})

# ------------------------------------------------------------------------------
# .fill_unassigned_identity edge cases
# ------------------------------------------------------------------------------

test_that(".fill_unassigned_identity handles all unassigned", {
  assign <- rep(-1L, 5)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result, 1:5)
})

test_that(".fill_unassigned_identity handles alternating pattern", {
  assign <- c(10L, -1L, 10L, -1L, 10L)

  result <- couplr:::.fill_unassigned_identity(assign)

  expect_equal(result[1], 10L)
  expect_equal(result[2], 2L)  # Filled with own index
  expect_equal(result[3], 10L)
  expect_equal(result[4], 4L)  # Filled with own index
  expect_equal(result[5], 10L)
})
