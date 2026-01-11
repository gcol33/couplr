# ==============================================================================
# Extended tests for matching_utils.R
# ==============================================================================

# ------------------------------------------------------------------------------
# validate_matching_inputs tests
# ------------------------------------------------------------------------------

test_that("validate_matching_inputs checks left is data frame", {
  expect_error(
    couplr:::validate_matching_inputs("not a df", data.frame(x = 1)),
    "left must be a data frame"
  )
})

test_that("validate_matching_inputs checks right is data frame", {
  expect_error(
    couplr:::validate_matching_inputs(data.frame(x = 1), "not a df"),
    "right must be a data frame"
  )
})

test_that("validate_matching_inputs checks left not empty", {
  expect_error(
    couplr:::validate_matching_inputs(data.frame(), data.frame(x = 1)),
    "left must have at least one row"
  )
})

test_that("validate_matching_inputs checks right not empty", {
  expect_error(
    couplr:::validate_matching_inputs(data.frame(x = 1), data.frame()),
    "right must have at least one row"
  )
})

test_that("validate_matching_inputs checks variables exist in left", {
  expect_error(
    couplr:::validate_matching_inputs(
      data.frame(x = 1),
      data.frame(x = 1, y = 2),
      vars = c("x", "y")
    ),
    "left is missing.*y"
  )
})

test_that("validate_matching_inputs checks variables exist in right", {
  expect_error(
    couplr:::validate_matching_inputs(
      data.frame(x = 1, y = 2),
      data.frame(x = 1),
      vars = c("x", "y")
    ),
    "right is missing.*y"
  )
})

test_that("validate_matching_inputs checks left variables are numeric", {
  expect_error(
    couplr:::validate_matching_inputs(
      data.frame(x = "a"),
      data.frame(x = 1),
      vars = "x"
    ),
    "Variable 'x' in left must be numeric"
  )
})

test_that("validate_matching_inputs checks right variables are numeric", {
  expect_error(
    couplr:::validate_matching_inputs(
      data.frame(x = 1),
      data.frame(x = "a"),
      vars = "x"
    ),
    "Variable 'x' in right must be numeric"
  )
})

test_that("validate_matching_inputs returns TRUE for valid inputs", {
  expect_true(
    couplr:::validate_matching_inputs(
      data.frame(x = 1:3, y = 4:6),
      data.frame(x = 7:9, y = 10:12),
      vars = c("x", "y")
    )
  )
})

# ------------------------------------------------------------------------------
# extract_ids tests
# ------------------------------------------------------------------------------

test_that("extract_ids uses id column when present", {
  df <- data.frame(id = c("a", "b", "c"), x = 1:3)

  result <- couplr:::extract_ids(df)

  expect_equal(result, c("a", "b", "c"))
})

test_that("extract_ids uses meaningful row names", {
  df <- data.frame(x = 1:3)
  rownames(df) <- c("row_a", "row_b", "row_c")

  result <- couplr:::extract_ids(df)

  expect_equal(result, c("row_a", "row_b", "row_c"))
})

test_that("extract_ids creates sequential IDs when no id column or meaningful rownames", {
  df <- data.frame(x = 1:3)

  result <- couplr:::extract_ids(df, prefix = "unit")

  expect_equal(result, c("unit_1", "unit_2", "unit_3"))
})

test_that("extract_ids uses default prefix", {
  df <- data.frame(x = 1:3)

  result <- couplr:::extract_ids(df)

  expect_equal(result, c("id_1", "id_2", "id_3"))
})

# ------------------------------------------------------------------------------
# extract_matching_vars tests
# ------------------------------------------------------------------------------

test_that("extract_matching_vars returns matrix", {
  df <- data.frame(x = 1:3, y = 4:6)

  result <- couplr:::extract_matching_vars(df, c("x", "y"))

  expect_true(is.matrix(result))
  expect_equal(dim(result), c(3, 2))
})

test_that("extract_matching_vars errors on NA values", {
  df <- data.frame(x = c(1, NA, 3), y = 4:6)

  expect_error(
    couplr:::extract_matching_vars(df, c("x", "y")),
    "Missing values.*NA"
  )
})

test_that("extract_matching_vars errors on NaN values", {
  # NaN is converted to NA by as.matrix, so it triggers NA check
  df <- data.frame(x = c(1, NaN, 3), y = 4:6)

  expect_error(
    couplr:::extract_matching_vars(df, c("x", "y")),
    "NA|NaN"  # May match either error
  )
})

test_that("extract_matching_vars errors on Inf values", {
  df <- data.frame(x = c(1, Inf, 3), y = 4:6)

  expect_error(
    couplr:::extract_matching_vars(df, c("x", "y")),
    "Infinite values"
  )
})

# ------------------------------------------------------------------------------
# get_block_id_column tests
# ------------------------------------------------------------------------------

test_that("get_block_id_column finds block_id", {
  df <- data.frame(block_id = c("A", "B"), x = 1:2)

  expect_equal(couplr:::get_block_id_column(df), "block_id")
})

test_that("get_block_id_column finds blockid", {
  df <- data.frame(blockid = c("A", "B"), x = 1:2)

  expect_equal(couplr:::get_block_id_column(df), "blockid")
})

test_that("get_block_id_column finds block", {
  df <- data.frame(block = c("A", "B"), x = 1:2)

  expect_equal(couplr:::get_block_id_column(df), "block")
})

test_that("get_block_id_column finds stratum", {
  df <- data.frame(stratum = c("A", "B"), x = 1:2)

  expect_equal(couplr:::get_block_id_column(df), "stratum")
})

test_that("get_block_id_column finds stratum_id", {
  df <- data.frame(stratum_id = c("A", "B"), x = 1:2)

  expect_equal(couplr:::get_block_id_column(df), "stratum_id")
})

test_that("get_block_id_column returns NULL when no block column", {
  df <- data.frame(x = 1:2, y = 3:4)

  expect_null(couplr:::get_block_id_column(df))
})

test_that("get_block_id_column prefers block_id over block", {
  df <- data.frame(block_id = c("A", "B"), block = c("C", "D"), x = 1:2)

  expect_equal(couplr:::get_block_id_column(df), "block_id")
})

# ------------------------------------------------------------------------------
# has_blocks tests
# ------------------------------------------------------------------------------

test_that("has_blocks returns TRUE when block column exists", {
  df <- data.frame(block_id = c("A", "B"), x = 1:2)

  expect_true(couplr:::has_blocks(df))
})

test_that("has_blocks returns FALSE when no block column", {
  df <- data.frame(x = 1:2, y = 3:4)

  expect_false(couplr:::has_blocks(df))
})

# ------------------------------------------------------------------------------
# validate_weights tests
# ------------------------------------------------------------------------------

test_that("validate_weights returns equal weights when NULL", {
  result <- couplr:::validate_weights(NULL, c("x", "y", "z"))

  expect_equal(result, c(1, 1, 1))
})

test_that("validate_weights accepts numeric vector of correct length", {
  result <- couplr:::validate_weights(c(1, 2, 3), c("x", "y", "z"))

  expect_equal(result, c(1, 2, 3))
})

test_that("validate_weights errors on wrong length numeric vector", {
  expect_error(
    couplr:::validate_weights(c(1, 2), c("x", "y", "z")),
    "weights must have length 3"
  )
})

test_that("validate_weights errors on negative weights", {
  expect_error(
    couplr:::validate_weights(c(1, -2, 3), c("x", "y", "z")),
    "weights must be non-negative"
  )
})

test_that("validate_weights accepts named list for partial weights", {
  weights <- list(x = 2, z = 3)

  result <- couplr:::validate_weights(weights, c("x", "y", "z"))

  expect_equal(result, c(2, 1, 3))
})

test_that("validate_weights accepts list", {
  weights <- list(x = 2, z = 3)

  result <- couplr:::validate_weights(weights, c("x", "y", "z"))

  expect_equal(result, c(2, 1, 3))
})

test_that("validate_weights errors on unknown variable in weights", {
  weights <- list(x = 2, unknown = 3)

  expect_error(
    couplr:::validate_weights(weights, c("x", "y", "z")),
    "unknown variable.*unknown"
  )
})

test_that("validate_weights errors on invalid type", {
  expect_error(
    couplr:::validate_weights("invalid", c("x", "y")),
    "weights must be a numeric vector or named list"
  )
})

# ------------------------------------------------------------------------------
# validate_calipers tests
# ------------------------------------------------------------------------------

test_that("validate_calipers returns NULL when NULL", {
  expect_null(couplr:::validate_calipers(NULL, c("x", "y")))
})

test_that("validate_calipers accepts named numeric vector", {
  calipers <- c(x = 0.5, y = 1.0)

  result <- couplr:::validate_calipers(calipers, c("x", "y", "z"))

  expect_equal(result, calipers)
})

test_that("validate_calipers accepts list", {
  calipers <- list(x = 0.5, y = 1.0)

  result <- couplr:::validate_calipers(calipers, c("x", "y", "z"))

  expect_equal(result$x, 0.5)
  expect_equal(result$y, 1.0)
})

test_that("validate_calipers errors on invalid type", {
  expect_error(
    couplr:::validate_calipers("invalid", c("x", "y")),
    "calipers must be a named numeric vector or list"
  )
})

test_that("validate_calipers errors when not named", {
  expect_error(
    couplr:::validate_calipers(c(0.5, 1.0), c("x", "y")),
    "calipers must be named"
  )
})

test_that("validate_calipers errors on unknown variable", {
  calipers <- c(x = 0.5, unknown = 1.0)

  expect_error(
    couplr:::validate_calipers(calipers, c("x", "y")),
    "unknown variables.*unknown"
  )
})

test_that("validate_calipers errors on non-positive values", {
  calipers <- c(x = 0.5, y = 0)

  expect_error(
    couplr:::validate_calipers(calipers, c("x", "y")),
    "caliper values must be positive"
  )
})

test_that("validate_calipers errors on negative values", {
  calipers <- c(x = 0.5, y = -1.0)

  expect_error(
    couplr:::validate_calipers(calipers, c("x", "y")),
    "caliper values must be positive"
  )
})
