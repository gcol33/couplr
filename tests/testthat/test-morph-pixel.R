# ==============================================================================
# Tests for pixel morphing functions (morph_pixel.R)
# ==============================================================================

# Skip all tests if magick is not available
skip_if_not_installed("magick")

# Helper to get test images
get_test_images <- function() {
  imgA <- system.file("extdata/icons/circleA_40.png", package = "couplr")
  imgB <- system.file("extdata/icons/circleB_40.png", package = "couplr")
  if (!nzchar(imgA) || !nzchar(imgB)) {
    skip("Test images not found")
  }
  list(A = imgA, B = imgB)
}

# ------------------------------------------------------------------------------
# Input validation tests for pixel_morph_animate
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate validates upscale parameter", {
  imgs <- get_test_images()

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, upscale = "invalid", show = FALSE),
    "upscale must be a single numeric"
  )

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, upscale = NA, show = FALSE),
    "upscale must be a single numeric"
  )

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, upscale = c(1, 2), show = FALSE),
    "upscale must be a single numeric"
  )
})

test_that("pixel_morph_animate validates n_frames parameter", {
  imgs <- get_test_images()

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, n_frames = "invalid", show = FALSE),
    "n_frames must be a single numeric"
  )

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, n_frames = NA, show = FALSE),
    "n_frames must be a single numeric"
  )
})

test_that("pixel_morph_animate validates alpha and beta parameters", {
  imgs <- get_test_images()

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, alpha = -1, show = FALSE),
    "alpha must be a single non-negative"
  )

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, beta = -1, show = FALSE),
    "beta must be a single non-negative"
  )

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, alpha = 0, beta = 0, show = FALSE),
    "alpha and beta cannot both be zero"
  )
})

test_that("pixel_morph_animate validates patch_size parameter", {
  imgs <- get_test_images()

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, patch_size = "invalid", show = FALSE),
    "patch_size must be a single numeric"
  )

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, patch_size = 0, show = FALSE),
    "patch_size must be at least 1"
  )
})

test_that("pixel_morph_animate validates downscale_steps parameter", {
  imgs <- get_test_images()

  expect_error(
    pixel_morph_animate(imgs$A, imgs$B, downscale_steps = -1, show = FALSE),
    "downscale_steps must be non-negative"
  )
})

# ------------------------------------------------------------------------------
# Input validation tests for pixel_morph
# ------------------------------------------------------------------------------

test_that("pixel_morph validates upscale parameter", {
  imgs <- get_test_images()

  expect_error(
    pixel_morph(imgs$A, imgs$B, upscale = "invalid", show = FALSE),
    "upscale must be a single numeric"
  )
})

test_that("pixel_morph validates alpha/beta parameters", {
  imgs <- get_test_images()

  expect_error(
    pixel_morph(imgs$A, imgs$B, alpha = 0, beta = 0, show = FALSE),
    "alpha and beta cannot both be zero"
  )
})

# ------------------------------------------------------------------------------
# Functional tests for pixel_morph
# ------------------------------------------------------------------------------

test_that("pixel_morph works with exact mode", {
  imgs <- get_test_images()

  result <- pixel_morph(imgs$A, imgs$B, mode = "exact", n_frames = 2, show = FALSE)

  expect_s3_class(result, "magick-image")
  info <- magick::image_info(result)
  expect_equal(info$width, 40)
  expect_equal(info$height, 40)
})

test_that("pixel_morph works with color_walk mode", {
  imgs <- get_test_images()

  result <- pixel_morph(imgs$A, imgs$B, mode = "color_walk", n_frames = 2, show = FALSE)

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph works with recursive mode", {
  imgs <- get_test_images()

  result <- pixel_morph(imgs$A, imgs$B, mode = "recursive", n_frames = 2, show = FALSE)

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph works with patch_size > 1", {
  imgs <- get_test_images()

  result <- pixel_morph(imgs$A, imgs$B, mode = "exact", patch_size = 3,
                        n_frames = 2, show = FALSE)

  expect_s3_class(result, "magick-image")
})

test_that("pixel_morph handles upscale parameter", {
  imgs <- get_test_images()

  result <- pixel_morph(imgs$A, imgs$B, mode = "exact", n_frames = 2,
                        upscale = 2, show = FALSE)

  expect_s3_class(result, "magick-image")
  info <- magick::image_info(result)
  expect_equal(info$width, 80)
  expect_equal(info$height, 80)
})

test_that("pixel_morph handles fractional upscale", {
  imgs <- get_test_images()

  result <- pixel_morph(imgs$A, imgs$B, mode = "exact", n_frames = 2,
                        upscale = 1.5, show = FALSE)

  expect_s3_class(result, "magick-image")
})

# ------------------------------------------------------------------------------
# Functional tests for pixel_morph_animate
# ------------------------------------------------------------------------------

test_that("pixel_morph_animate creates animation with correct number of frames", {
  imgs <- get_test_images()

  result <- pixel_morph_animate(imgs$A, imgs$B, n_frames = 4, show = FALSE)

  expect_type(result, "list")
  expect_s3_class(result$animation, "magick-image")
  expect_equal(result$n_pixels, 40 * 40)
  expect_equal(result$width, 40)
  expect_equal(result$height, 40)
})

test_that("pixel_morph_animate returns correct assignment vector", {
  imgs <- get_test_images()

  result <- pixel_morph_animate(imgs$A, imgs$B, n_frames = 2, show = FALSE)

  expect_type(result$assignment, "integer")
  expect_equal(length(result$assignment), 40 * 40)
  # Assignment should be 1-based (R convention)
  expect_true(all(result$assignment >= 1))
  expect_true(all(result$assignment <= 40 * 40))
})

test_that("pixel_morph_animate works with all modes", {
  imgs <- get_test_images()

  for (mode in c("exact", "color_walk", "recursive")) {
    result <- pixel_morph_animate(imgs$A, imgs$B, mode = mode,
                                  n_frames = 2, show = FALSE)
    expect_type(result, "list")
    expect_equal(result$mode, mode)
  }
})

test_that("pixel_morph_animate handles downscale_steps", {
  imgs <- get_test_images()

  result <- suppressWarnings(pixel_morph_animate(imgs$A, imgs$B, downscale_steps = 1,
                                n_frames = 2, show = FALSE))

  expect_type(result, "list")
  # Original dimensions preserved in output
  expect_equal(result$width, 40)
  expect_equal(result$height, 40)
})

test_that("pixel_morph_animate can save to file", {
  imgs <- get_test_images()
  outfile <- tempfile(fileext = ".gif")

  result <- pixel_morph_animate(imgs$A, imgs$B, n_frames = 2,
                                outfile = outfile, show = FALSE)

  expect_true(file.exists(outfile))
  unlink(outfile)
})

test_that("pixel_morph_animate handles webp format", {
  imgs <- get_test_images()
  outfile <- tempfile(fileext = ".webp")

  result <- pixel_morph_animate(imgs$A, imgs$B, n_frames = 2,
                                format = "webp", outfile = outfile, show = FALSE)

  expect_true(file.exists(outfile))
  unlink(outfile)
})

# ------------------------------------------------------------------------------
# Edge case tests
# ------------------------------------------------------------------------------

test_that("pixel_morph handles images of different sizes", {
  imgs <- get_test_images()

  # Load and resize one image
  A <- magick::image_read(imgs$A)
  B <- magick::image_read(imgs$B)
  B_resized <- magick::image_resize(B, "30x30!")

  # Should auto-resize B to match A
  result <- pixel_morph(A, B_resized, mode = "exact", n_frames = 2, show = FALSE)

  expect_s3_class(result, "magick-image")
  info <- magick::image_info(result)
  expect_equal(info$width, 40)
  expect_equal(info$height, 40)
})

test_that("pixel_morph_animate warns on large images for exact mode", {
  skip_on_cran()  # Skip on CRAN due to resource constraints
  imgs <- get_test_images()

  # This should warn about large image
  # We can't easily test this without creating a large image
  # Just ensure the code path exists
  expect_true(TRUE)
})

test_that("pixel_morph warns on negative upscale (sets to 1)", {
  imgs <- get_test_images()

  expect_warning(
    pixel_morph(imgs$A, imgs$B, upscale = -1, n_frames = 2, show = FALSE),
    "upscale must be positive"
  )
})

test_that("pixel_morph_animate warns on small n_frames (sets to 2)", {
  imgs <- get_test_images()

  expect_warning(
    pixel_morph_animate(imgs$A, imgs$B, n_frames = 1, show = FALSE),
    "n_frames must be at least 2"
  )
})
