# ==============================================================================
# Final push tests to maximize morph coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# morph_pixel.R: mp4 output path (lines 394-414)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate saves mp4 when av available", {
  skip_if_not_installed("magick")
  skip_if_not_installed("av")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  outfile <- tempfile(fileext = ".mp4")
  on.exit(unlink(outfile), add = TRUE)

  result <- pixel_morph_animate(
    imgA, imgB,
    format = "mp4",
    outfile = outfile,
    n_frames = 3,
    show = FALSE
  )

  expect_true(file.exists(outfile))
})

# ------------------------------------------------------------------------------
# morph_pixel.R: size warnings for exact mode (lines 224-234)
# ------------------------------------------------------------------------------

# Large exact mode warning tests skipped - they require large LAP computations
# that are too slow for automated testing. The warnings are tested via
# visual inspection during development.

# ------------------------------------------------------------------------------
# morph_pixel.R: size warnings for large tiles (lines 237-249)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate warns for large tiles", {
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  expect_warning(
    pixel_morph_animate(
      imgA, imgB,
      mode = "exact",
      patch_size = 25,  # 25x25 = 625 pixels per tile > 400
      n_frames = 2,
      show = FALSE
    ),
    "Tile is large for LAP"
  )
})

# ------------------------------------------------------------------------------
# morph_pixel.R: color_walk large image warning (lines 252-263)
# ------------------------------------------------------------------------------

# Large image color_walk test skipped - too slow for automated testing
# The warning threshold is 250000 pixels; we skip to avoid timeout

# ------------------------------------------------------------------------------
# morph_utils.R: .to_array_rgb error paths (lines 28, 55)
# These are hard to trigger through normal usage since magick always
# returns properly formatted data
# ------------------------------------------------------------------------------

# The error at line 28 requires magick to return non-3D data - very rare
# The error at line 55 requires unexpected dim ordering - also rare

# ------------------------------------------------------------------------------
# morph_utils.R: color walk pipeline - remaining A pixels path (lines 454-476)
# ------------------------------------------------------------------------------

test_that("morph_utils color walk handles remaining pixels", {
  H <- 6
  W <- 6
  N <- H * W

  # Create images where color groups don't perfectly align
  set.seed(999)
  A_planar <- sample(0:255, N * 3, replace = TRUE)
  B_planar <- sample(0:255, N * 3, replace = TRUE)

  result <- couplr:::.solve_color_walk_pipeline(
    A_planar, B_planar, H, W,
    quantize_bits = 2,  # Very few colors = large groups
    method = "jv",
    maximize = FALSE
  )

  expect_equal(length(result), N)
  # Should be a valid assignment
  expect_true(all(result >= 1 & result <= N))
})

# ------------------------------------------------------------------------------
# morph_pixel.R: .solve_color_walk_pipeline remaining pixels (lines 1211-1218)
# ------------------------------------------------------------------------------

test_that("morph_pixel color walk handles sparse groups", {
  H <- 8
  W <- 8
  N <- H * W

  # Create images with very different colors to force remaining pixel handling
  set.seed(42)
  A_planar <- c(
    sample(c(0, 128, 255), N, replace = TRUE),  # R
    sample(c(0, 128, 255), N, replace = TRUE),  # G
    sample(c(0, 128, 255), N, replace = TRUE)   # B
  )
  B_planar <- c(
    sample(c(32, 160, 224), N, replace = TRUE),  # Different R range
    sample(c(32, 160, 224), N, replace = TRUE),  # Different G range
    sample(c(32, 160, 224), N, replace = TRUE)   # Different B range
  )

  result <- couplr:::.solve_color_walk_pipeline(
    A_planar, B_planar, H, W,
    quantize_bits = 4,
    method = "jv",
    maximize = FALSE
  )

  expect_equal(length(result), N)
})

# ------------------------------------------------------------------------------
# morph_tiling.R: .recursive_tiling_solver rest width/height (lines 1128-1162)
# ------------------------------------------------------------------------------

test_that(".recursive_tiling_solver handles images with odd dimensions", {
  # 7x5 has rest_w=1 and rest_h=1 when divided by 2
  H <- 5
  W <- 7
  N <- H * W

  set.seed(123)
  A_planar <- runif(N * 3) * 255
  B_planar <- runif(N * 3) * 255

  result <- couplr:::.recursive_tiling_solver(
    A_planar, B_planar, H, W,
    patch_size = 2,
    alpha = 1, beta = 0.1
  )

  expect_equal(length(result), N)
  # Should cover all pixels
  expect_true(all(result >= 1 & result <= N))
})

test_that(".recursive_tiling_solver handles both rest_w and rest_h > 0", {
  # 9x9 has rest_w=1 and rest_h=1 for 2x2 tiling
  H <- 9
  W <- 9
  N <- H * W

  set.seed(456)
  A_planar <- runif(N * 3) * 255
  B_planar <- runif(N * 3) * 255

  result <- couplr:::.recursive_tiling_solver(
    A_planar, B_planar, H, W,
    patch_size = 2,
    alpha = 0.5, beta = 0.5
  )

  expect_equal(length(result), N)
})

# ------------------------------------------------------------------------------
# morph_utils.R: .patch_cost_matrix with multiple patches (lines 261-275)
# ------------------------------------------------------------------------------

test_that(".patch_cost_matrix handles multiple patches", {
  patches_a <- list(
    colors = matrix(c(
      100, 100, 100,
      200, 50, 50,
      50, 200, 50
    ), nrow = 3, ncol = 3, byrow = TRUE),
    centers = matrix(c(
      0, 0,
      10, 0,
      5, 10
    ), nrow = 3, ncol = 2, byrow = TRUE)
  )
  patches_b <- list(
    colors = matrix(c(
      105, 95, 100,
      190, 60, 45,
      55, 210, 55
    ), nrow = 3, ncol = 3, byrow = TRUE),
    centers = matrix(c(
      2, 1,
      8, 2,
      6, 9
    ), nrow = 3, ncol = 2, byrow = TRUE)
  )

  result <- couplr:::.patch_cost_matrix(patches_a, patches_b, alpha = 1, beta = 0.2, H = 20, W = 20)

  expect_equal(dim(result), c(3, 3))
  expect_true(all(is.finite(result)))
})

# ------------------------------------------------------------------------------
# morph_utils.R: .expand_patch_assignment with multiple patches (lines 280-291)
# ------------------------------------------------------------------------------

test_that(".expand_patch_assignment handles multiple patches", {
  patch_assign <- list(2L, 1L, 3L)  # Patch 1->B2, Patch 2->B1, Patch 3->B3
  patches_a <- list(
    indices = list(c(1L, 2L), c(3L, 4L), c(5L, 6L))
  )
  patches_b <- list(
    indices = list(c(7L, 8L), c(9L, 10L), c(11L, 12L))
  )

  result <- couplr:::.expand_patch_assignment(patch_assign, patches_a, patches_b, N = 6)

  expect_equal(length(result), 6)
  # Patch 1 maps to B patch 2
  expect_equal(result[1], 9L)
  expect_equal(result[2], 10L)
  # Patch 2 maps to B patch 1
  expect_equal(result[3], 7L)
  expect_equal(result[4], 8L)
})

# ------------------------------------------------------------------------------
# morph_tiling.R: .generate_square_tiles boundary conditions
# ------------------------------------------------------------------------------

test_that(".generate_square_tiles handles exact division", {
  # 6x6 with P=2 divides exactly
  tiles <- couplr:::.generate_square_tiles(W = 6, H = 6, P = 2)

  # Should have 9 tiles of size 2
  expect_equal(length(tiles), 9)
  expect_true(all(sapply(tiles, function(t) t$size) == 2))
})

test_that(".generate_square_tiles handles P > both dimensions", {
  tiles <- couplr:::.generate_square_tiles(W = 2, H = 3, P = 5)

  # Should fill with smaller tiles
  covered <- matrix(FALSE, 3, 2)
  for (tile in tiles) {
    for (dx in 0:(tile$size - 1)) {
      for (dy in 0:(tile$size - 1)) {
        covered[tile$y0 + dy + 1, tile$x0 + dx + 1] <- TRUE
      }
    }
  }
  expect_true(all(covered))
})

# ------------------------------------------------------------------------------
# morph_pixel.R: pixel_morph with downscale (lines 707-718)
# ------------------------------------------------------------------------------

test_that("pixel_morph works with downscale_steps", {
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  result <- pixel_morph(
    imgA, imgB,
    downscale_steps = 1,
    n_frames = 2,
    show = FALSE
  )

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# morph_pixel.R: fractional upscale path (lines 365-371, 757-763)
# ------------------------------------------------------------------------------

test_that("pixel_morph applies fractional upscale correctly", {
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  result <- pixel_morph(
    imgA, imgB,
    upscale = 1.5,  # Non-integer
    n_frames = 2,
    show = FALSE
  )

  info <- magick::image_info(result)
  # 40 * 1.5 = 60
  expect_equal(info$width, 60)
  expect_equal(info$height, 60)
})

# ------------------------------------------------------------------------------
# morph_pixel.R: GIF format save (lines 419-420)
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate saves GIF correctly", {
  skip_if_not_installed("magick")

  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  skip_if(imgA == "" || imgB == "")

  outfile <- tempfile(fileext = ".gif")
  on.exit(unlink(outfile), add = TRUE)

  result <- pixel_morph_animate(
    imgA, imgB,
    format = "gif",
    outfile = outfile,
    n_frames = 2,
    show = FALSE
  )

  expect_true(file.exists(outfile))
  expect_true(file.size(outfile) > 0)
})
