# ==============================================================================
# More tests for matching_core.R coverage
# ==============================================================================

# Create test data
set.seed(42)
left_df <- tibble::tibble(
  id = 1:20,
  age = rnorm(20, 40, 10),
  income = rnorm(20, 50000, 15000),
  education = sample(1:5, 20, replace = TRUE),
  score = runif(20, 0, 100)
)

right_df <- tibble::tibble(
  id = 101:120,
  age = rnorm(20, 42, 12),
  income = rnorm(20, 52000, 18000),
  education = sample(1:5, 20, replace = TRUE),
  score = runif(20, 0, 100)
)

# ------------------------------------------------------------------------------
# match_couples method variations
# ------------------------------------------------------------------------------

test_that("match_couples works with all LAP methods", {
  methods <- c("jv", "hungarian", "auction", "ssp", "csflow")

  for (m in methods) {
    result <- match_couples(
      left_df[1:5, ], right_df[1:5, ],
      vars = c("age", "income"),
      method = m
    )
    expect_s3_class(result, "matching_result")
    expect_true(nrow(result$pairs) > 0)
  }
})

test_that("match_couples handles single variable", {
  result <- match_couples(
    left_df, right_df,
    vars = "age"
  )

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), nrow(left_df))
})

test_that("match_couples handles many variables", {
  result <- match_couples(
    left_df, right_df,
    vars = c("age", "income", "education", "score")
  )

  expect_s3_class(result, "matching_result")
})

test_that("match_couples handles unequal group sizes (more left)", {
  result <- match_couples(
    left_df,
    right_df[1:10, ],
    vars = c("age", "income")
  )

  expect_s3_class(result, "matching_result")
  # Should match min(20, 10) = 10 pairs
  expect_equal(nrow(result$pairs), 10)
})

test_that("match_couples handles unequal group sizes (more right)", {
  result <- match_couples(
    left_df[1:8, ],
    right_df,
    vars = c("age", "income")
  )

  expect_s3_class(result, "matching_result")
  expect_equal(nrow(result$pairs), 8)
})

test_that("match_couples with distance metric euclidean", {
  result <- match_couples(
    left_df, right_df,
    vars = c("age", "income"),
    distance = "euclidean"
  )

  expect_s3_class(result, "matching_result")
})

test_that("match_couples with distance metric manhattan", {
  result <- match_couples(
    left_df, right_df,
    vars = c("age", "income"),
    distance = "manhattan"
  )

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# greedy_couples variations
# ------------------------------------------------------------------------------

test_that("greedy_couples with all strategies", {
  strategies <- c("sorted", "row_best", "pq")

  for (s in strategies) {
    result <- greedy_couples(
      left_df, right_df,
      vars = c("age", "income"),
      strategy = s
    )
    expect_s3_class(result, "matching_result")
    expect_true(nrow(result$pairs) > 0)
  }
})

test_that("greedy_couples with scaling", {
  result <- greedy_couples(
    left_df, right_df,
    vars = c("age", "income"),
    scale = "robust"
  )

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# Edge cases and error handling
# ------------------------------------------------------------------------------

test_that("match_couples with very different scales", {
  # Create data with wildly different scales
  left_scale <- tibble::tibble(
    id = 1:10,
    tiny = rnorm(10, 0, 0.001),
    huge = rnorm(10, 1e6, 1e5)
  )
  right_scale <- tibble::tibble(
    id = 1:10,
    tiny = rnorm(10, 0, 0.001),
    huge = rnorm(10, 1e6, 1e5)
  )

  result <- match_couples(
    left_scale, right_scale,
    vars = c("tiny", "huge"),
    auto_scale = TRUE
  )

  expect_s3_class(result, "matching_result")
})

test_that("match_couples with integer columns", {
  left_int <- tibble::tibble(
    id = 1:10,
    count1 = sample(1:100, 10),
    count2 = sample(1:50, 10)
  )
  right_int <- tibble::tibble(
    id = 1:10,
    count1 = sample(1:100, 10),
    count2 = sample(1:50, 10)
  )

  result <- match_couples(
    left_int, right_int,
    vars = c("count1", "count2")
  )

  expect_s3_class(result, "matching_result")
})

test_that("match_couples with identical data returns zero distances", {
  identical_left <- tibble::tibble(id = 1:5, val = c(1, 2, 3, 4, 5))
  identical_right <- tibble::tibble(id = 1:5, val = c(1, 2, 3, 4, 5))

  result <- suppressWarnings(match_couples(
    identical_left, identical_right,
    vars = "val"
  ))

  expect_s3_class(result, "matching_result")
  # All distances should be 0
  expect_true(all(result$pairs$distance == 0))
})

# ------------------------------------------------------------------------------
# Block matching
# ------------------------------------------------------------------------------

test_that("match_couples with block_id", {
  left_blocked <- left_df
  left_blocked$block <- rep(1:4, each = 5)

  right_blocked <- right_df
  right_blocked$block <- rep(1:4, each = 5)

  result <- match_couples(
    left_blocked, right_blocked,
    vars = c("age", "income"),
    block_id = "block"
  )

  expect_s3_class(result, "matching_result")
})

# ------------------------------------------------------------------------------
# Summary and print methods
# ------------------------------------------------------------------------------

test_that("summary method for matching_result works", {
  result <- match_couples(
    left_df[1:10, ], right_df[1:10, ],
    vars = c("age", "income")
  )

  summ <- summary(result)
  expect_true(is.list(summ))
})

test_that("print method for matching_result works", {
  result <- match_couples(
    left_df[1:5, ], right_df[1:5, ],
    vars = c("age", "income")
  )

  output <- capture.output(print(result))
  expect_true(length(output) > 0)
})
