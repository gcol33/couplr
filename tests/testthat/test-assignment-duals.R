# tests/testthat/test-assignment-duals.R
# Tests for assignment_duals() - dual variable extraction

test_that("assignment_duals returns all required components", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_true("match" %in% names(result))
  expect_true("total_cost" %in% names(result))
  expect_true("u" %in% names(result))
  expect_true("v" %in% names(result))
  expect_true("status" %in% names(result))

  expect_equal(length(result$match), 3)
  expect_equal(length(result$u), 3)
  expect_equal(length(result$v), 3)
})

test_that("assignment_duals satisfies complementary slackness", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # For assigned pairs, u[i] + v[j] should equal cost[i,j]
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-9)
  }
})

test_that("assignment_duals satisfies strong duality", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # sum(u) + sum(v) = total_cost
  expect_equal(sum(result$u) + sum(result$v), result$total_cost, tolerance = 1e-9)
})

test_that("assignment_duals dual feasibility (u + v <= cost)", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # For all (i,j): u[i] + v[j] <= cost[i,j]
  for (i in 1:3) {
    for (j in 1:3) {
      expect_true(result$u[i] + result$v[j] <= cost[i, j] + 1e-9)
    }
  }
})

test_that("assignment_duals matches assignment() result", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)

  result_duals <- assignment_duals(cost)
  result_jv <- assignment(cost, method = "jv")

  expect_equal(result_duals$match, result_jv$match)
  expect_equal(result_duals$total_cost, result_jv$total_cost)
})

test_that("assignment_duals handles rectangular 3x5", {
  cost <- matrix(c(
    1, 5, 9, 2, 6,
    3, 7, 1, 4, 8,
    5, 2, 6, 3, 7
  ), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_equal(length(result$match), 3)
  expect_equal(length(result$u), 3)
  expect_equal(length(result$v), 5)

  # Verify complementary slackness
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-9)
  }

  # Verify strong duality
  expect_equal(sum(result$u) + sum(result$v), result$total_cost, tolerance = 1e-9)
})

test_that("assignment_duals handles tall rectangular (transposed)", {
  cost <- matrix(c(
    1, 5, 9,
    3, 7, 1,
    5, 2, 6,
    4, 8, 2,
    6, 3, 5
  ), nrow = 5, byrow = TRUE)
  result <- assignment_duals(cost)

  # After transposition: u has length 5, v has length 3
  expect_equal(length(result$u), 5)
  expect_equal(length(result$v), 3)

  # Verify strong duality
  expect_equal(sum(result$u) + sum(result$v), result$total_cost, tolerance = 1e-9)
})

test_that("assignment_duals handles maximization", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost, maximize = TRUE)

  expect_equal(result$status, "optimal")

  # For maximization, the duals are negated
  # So complementary slackness should still hold
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-9)
  }
})

test_that("assignment_duals handles 1x1", {
  cost <- matrix(42, nrow = 1)
  result <- assignment_duals(cost)

  expect_equal(result$match, 1)
  expect_equal(result$total_cost, 42)
  expect_equal(length(result$u), 1)
  expect_equal(length(result$v), 1)
  expect_equal(result$u[1] + result$v[1], 42)
})

test_that("assignment_duals handles negative costs", {
  cost <- matrix(c(-4, -2, -5, -3, -3, -6, -7, -5, -4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # Verify complementary slackness
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-9)
  }
})

test_that("assignment_duals handles NA/Inf entries", {
  cost <- matrix(c(
    1, NA, 3,
    5, 6, Inf,
    9, 10, 11
  ), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_equal(result$status, "optimal")

  # Check assigned pairs are finite
  for (i in 1:3) {
    j <- result$match[i]
    expect_true(is.finite(cost[i, j]))
  }
})

test_that("assignment_duals larger matrix", {
  set.seed(123)
  n <- 10
  cost <- matrix(runif(n * n, 1, 100), nrow = n)
  result <- assignment_duals(cost)

  expect_equal(length(result$match), n)
  expect_equal(length(result$u), n)
  expect_equal(length(result$v), n)

  # Verify strong duality
  expect_equal(sum(result$u) + sum(result$v), result$total_cost, tolerance = 1e-6)

  # Verify complementary slackness
  for (i in 1:n) {
    j <- result$match[i]
    expect_equal(result$u[i] + result$v[j], cost[i, j], tolerance = 1e-6)
  }
})

test_that("assignment_duals reduced costs are non-negative", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # Reduced cost = cost - u - v >= 0 for all (i,j)
  for (i in 1:3) {
    for (j in 1:3) {
      reduced_cost <- cost[i, j] - result$u[i] - result$v[j]
      expect_true(reduced_cost >= -1e-9)
    }
  }
})

test_that("assignment_duals print method works", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_output(print(result), "Assignment Result with Duals")
  expect_output(print(result), "Dual variables")
})

test_that("assignment_duals class is correct", {
  cost <- matrix(c(1, 2, 3, 4), nrow = 2)
  result <- assignment_duals(cost)

  expect_s3_class(result, "assignment_duals_result")
})

test_that("assignment_duals zero cost diagonal", {
  cost <- matrix(c(0, 1, 2, 3, 0, 4, 5, 6, 0), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  expect_equal(result$total_cost, 0)
  expect_equal(result$match, c(1, 2, 3))
})

test_that("assignment_duals sensitivity analysis example", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), nrow = 3, byrow = TRUE)
  result <- assignment_duals(cost)

  # Compute reduced costs (sensitivity to cost changes)
  reduced <- matrix(NA, 3, 3)
  for (i in 1:3) {
    for (j in 1:3) {
      reduced[i, j] <- cost[i, j] - result$u[i] - result$v[j]
    }
  }

  # Assigned pairs have zero reduced cost
  for (i in 1:3) {
    j <- result$match[i]
    expect_equal(reduced[i, j], 0, tolerance = 1e-9)
  }

  # Non-assigned pairs have positive reduced cost
  for (i in 1:3) {
    for (j in 1:3) {
      if (result$match[i] != j) {
        expect_true(reduced[i, j] > -1e-9)
      }
    }
  }
})
