# ==============================================================================
# Tests for cycle_cancel solver coverage
# ==============================================================================

test_that("cycle_cancel handles 1x1 matrix", {
  cost <- matrix(5, 1, 1)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$match, 1L)
  expect_equal(result$total_cost, 5)
  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles 2x2 diagonal optimal", {
  cost <- matrix(c(1, 100, 100, 1), 2, 2)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$total_cost, 2)
  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles 2x2 anti-diagonal optimal", {
  cost <- matrix(c(100, 1, 1, 100), 2, 2)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$total_cost, 2)
  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles rectangular matrix (more cols)", {
  cost <- matrix(1:6, 2, 3)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(length(result$match), 2)
  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles rectangular matrix (more rows)", {
  cost <- matrix(1:6, 3, 2)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(length(result$match), 3)
})

test_that("cycle_cancel handles negative costs", {
  cost <- matrix(c(-5, -1, -2, -10), 2, 2)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
  # Optimal should be -5 + -10 = -15
  expect_equal(result$total_cost, -15)
})

test_that("cycle_cancel handles large cost range", {
  cost <- matrix(c(1, 1000000, 1000000, 1), 2, 2)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$total_cost, 2)
})

test_that("cycle_cancel handles tie costs", {
  cost <- matrix(1, 3, 3)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$total_cost, 3)
})

test_that("cycle_cancel handles 4x4 matrix", {
  cost <- matrix(c(
    1, 5, 3, 4,
    2, 6, 1, 5,
    4, 2, 7, 3,
    6, 3, 2, 1
  ), 4, 4, byrow = TRUE)

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 4)
})

test_that("cycle_cancel handles forbidden assignments", {
  cost <- matrix(c(1, Inf, Inf, 1), 2, 2)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$total_cost, 2)
})

test_that("cycle_cancel handles 5x5 matrix", {
  set.seed(42)
  cost <- matrix(runif(25), 5, 5)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 5)
})

test_that("cycle_cancel handles sparse forbidden pattern", {
  # Only diagonal and anti-diagonal allowed
  cost <- matrix(Inf, 4, 4)
  for (i in 1:4) {
    cost[i, i] <- i * 2
    cost[i, 5 - i] <- i + 3
  }

  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel matches jv on random matrices", {
  set.seed(123)
  for (n in c(2, 4, 6)) {
    cost <- matrix(runif(n * n), n, n)

    result_cc <- assignment(cost, method = "cycle_cancel")
    result_jv <- assignment(cost, method = "jv")

    expect_equal(result_cc$total_cost, result_jv$total_cost,
                 tolerance = 1e-10,
                 info = sprintf("n=%d", n))
  }
})
