# Tests for summary.couplr_result on matching results

test_that("summary reports matched units on each side", {
  left <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  right <- data.frame(id = 6:10, x = c(1.1, 2.2, 3.1, 4.2, 5.1))
  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)

  s <- summary(result)
  expect_equal(s$glance$n_matched_left, 5L)
  expect_equal(s$glance$n_left, 5L)
  out <- capture.output(print(s))
  expect_true(any(grepl("Left units: *5 matched of 5", out)))
})

test_that("summary includes pair distance quantiles", {
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:20, x = rnorm(10))
  result <- match_couples(left, right, vars = "x", return_diagnostics = TRUE)

  s <- summary(result)
  expect_length(s$distances, 6)
  expect_equal(unname(s$distances[1]), min(result$pairs$distance))
  expect_equal(unname(s$distances[6]), max(result$pairs$distance))
  out <- capture.output(print(s))
  expect_true(any(grepl("Pair distances", out)))
})

test_that("summary handles empty matches", {
  left <- data.frame(id = 1:3, x = c(100, 200, 300))
  right <- data.frame(id = 4:6, x = c(1, 2, 3))
  # greedy matching doesn't error on no valid pairs (strict_no_pairs = FALSE)
  expect_warning(
    result <- match_couples(left, right, vars = "x", max_distance = 0.001,
                            method = "greedy"),
    "No valid pairs"
  )

  s <- summary(result)
  expect_null(s$distances)
  expect_equal(s$glance$n_matched_left, 0L)
  expect_output(print(s), "Unmatched left: 3")
})

test_that("summary names the design of a replacement matching", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 2.1, 3.1))
  result <- match_couples(left, right, vars = "x",
                          replace = TRUE, return_diagnostics = TRUE)

  expect_equal(summary(result)$glance$design, "one-to-one with replacement")
  ratio <- match_couples(left, data.frame(id = 4:9, x = c(1, 1.2, 2, 2.2, 3, 3.2)),
                         vars = "x", ratio = 2L)
  expect_equal(glance(ratio)$design, "2-to-one")
})
