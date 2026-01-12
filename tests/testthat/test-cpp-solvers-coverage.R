# ==============================================================================
# Tests to increase C++ solver coverage
# ==============================================================================

# ------------------------------------------------------------------------------
# Gabow-Tarjan solver tests
# ------------------------------------------------------------------------------

test_that("gabow_tarjan solver works on small matrices", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "gabow_tarjan")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 3)
})

test_that("gabow_tarjan solver handles rectangular matrices", {
  cost <- matrix(runif(12), 3, 4)
  result <- assignment(cost, method = "gabow_tarjan")

  expect_equal(result$status, "optimal")
})

test_that("gabow_tarjan solver handles maximize", {
  cost <- matrix(c(1, 10, 10, 1), 2, 2)
  result <- assignment(cost, method = "gabow_tarjan", maximize = TRUE)

  expect_equal(result$status, "optimal")
  expect_true(result$total_cost >= 10)
})

test_that("gabow_tarjan handles sparse matrix with NA", {
  cost <- matrix(c(1, NA, 3, NA, 5, NA, NA, 8, 9), 3, 3)
  result <- assignment(cost, method = "gabow_tarjan")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Network Simplex solver tests
# ------------------------------------------------------------------------------

test_that("network_simplex solver works on small matrices", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
})

test_that("network_simplex handles rectangular matrices", {
  cost <- matrix(runif(15), 3, 5)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
})

test_that("network_simplex handles larger matrices", {
  set.seed(123)
  cost <- matrix(runif(100), 10, 10)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 10)
})

test_that("network_simplex with some forbidden entries", {
  cost <- matrix(c(1, NA, 3, 4, NA, 6, 7, 8, NA), 3, 3)
  result <- assignment(cost, method = "network_simplex")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Cycle Canceling solver tests
# ------------------------------------------------------------------------------

test_that("cycle_cancel solver works on small matrices", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles maximize", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- assignment(cost, method = "cycle_cancel", maximize = TRUE)

  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel handles rectangular matrices", {
  cost <- matrix(runif(20), 4, 5)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
})

test_that("cycle_cancel with sparse matrix", {
  cost <- matrix(c(1, NA, NA, 4, 5, NA, 7, NA, 9), 3, 3)
  result <- assignment(cost, method = "cycle_cancel")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Orlin-Ahuja (csa) solver tests
# ------------------------------------------------------------------------------

test_that("csa solver works on small matrices", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
})

test_that("csa solver handles maximize", {
  cost <- matrix(c(1, 10, 10, 1), 2, 2)
  result <- assignment(cost, method = "csa", maximize = TRUE)

  expect_equal(result$status, "optimal")
})

test_that("csa handles larger matrices efficiently", {
  set.seed(456)
  cost <- matrix(runif(400), 20, 20)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
  expect_equal(length(result$match), 20)
})

test_that("csa handles rectangular matrices", {
  cost <- matrix(runif(24), 4, 6)
  result <- assignment(cost, method = "csa")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Push-Relabel solver tests
# ------------------------------------------------------------------------------

test_that("push_relabel solver works on small matrices", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "push_relabel")

  expect_equal(result$status, "optimal")
})

test_that("push_relabel handles maximize", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- assignment(cost, method = "push_relabel", maximize = TRUE)

  expect_equal(result$status, "optimal")
})

test_that("push_relabel handles rectangular matrices", {
  cost <- matrix(runif(18), 3, 6)
  result <- assignment(cost, method = "push_relabel")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Ramshaw-Tarjan solver tests
# ------------------------------------------------------------------------------

test_that("ramshaw_tarjan solver works on small matrices", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(result$status, "optimal")
})

test_that("ramshaw_tarjan handles rectangular matrices", {
  cost <- matrix(runif(15), 3, 5)
  result <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(result$status, "optimal")
})

test_that("ramshaw_tarjan handles highly rectangular matrices", {
  cost <- matrix(runif(20), 2, 10)
  result <- assignment(cost, method = "ramshaw_tarjan")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# SSP and SAP solver tests
# ------------------------------------------------------------------------------

test_that("ssp solver works", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "ssp")

  expect_equal(result$status, "optimal")
})

test_that("sap solver works", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "sap")

  expect_equal(result$status, "optimal")
})

test_that("ssp handles rectangular", {
  cost <- matrix(runif(12), 3, 4)
  result <- assignment(cost, method = "ssp")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# SSAP Bucket solver tests
# ------------------------------------------------------------------------------

test_that("ssap_bucket solver works", {
  # Integer costs work best for bucket-based algorithm
  cost <- matrix(as.integer(c(1, 2, 3, 4, 5, 6, 7, 8, 9)), 3, 3)
  result <- assignment(cost, method = "ssap_bucket")

  expect_equal(result$status, "optimal")
})

test_that("ssap_bucket handles larger matrices", {
  set.seed(789)
  cost <- matrix(sample(1:100, 64, replace = TRUE), 8, 8)
  result <- assignment(cost, method = "ssap_bucket")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# CSFlow solver tests
# ------------------------------------------------------------------------------

test_that("csflow solver works", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  result <- assignment(cost, method = "csflow")

  expect_equal(result$status, "optimal")
})

test_that("csflow handles maximize", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  result <- assignment(cost, method = "csflow", maximize = TRUE)

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# HK01 solver tests
# ------------------------------------------------------------------------------

test_that("hk01 solver works on binary costs", {
  cost <- matrix(c(0, 1, 1, 0, 1, 1, 0, 1, 1), 3, 3)
  result <- assignment(cost, method = "hk01")

  expect_equal(result$status, "optimal")
})

test_that("hk01 handles larger binary matrices", {
  set.seed(111)
  cost <- matrix(sample(0:1, 100, replace = TRUE), 10, 10)
  result <- assignment(cost, method = "hk01")

  expect_equal(result$status, "optimal")
})

# ------------------------------------------------------------------------------
# Lapmod solver tests (sparse matrices)
# ------------------------------------------------------------------------------

test_that("lapmod solver works on sparse matrices", {
  # Create sparse matrix (>50% NA)
  cost <- matrix(NA, 5, 5)
  cost[1, 1] <- 1
  cost[1, 2] <- 2
  cost[2, 2] <- 1
  cost[2, 3] <- 3
  cost[3, 3] <- 1
  cost[3, 4] <- 2
  cost[4, 4] <- 1
  cost[4, 5] <- 3
  cost[5, 5] <- 1

  result <- assignment(cost, method = "lapmod")

  expect_equal(result$status, "optimal")
})

test_that("lapmod handles medium-sized sparse matrices", {
  set.seed(222)
  n <- 15
  cost <- matrix(NA, n, n)
  # Fill about 30% of entries
  for (i in 1:n) {
    idx <- sample(1:n, 4)
    cost[i, idx] <- runif(4, 1, 10)
  }

  result <- assignment(cost, method = "lapmod")

  expect_true(result$status %in% c("optimal", "infeasible"))
})

# ------------------------------------------------------------------------------
# Bottleneck assignment tests
# ------------------------------------------------------------------------------

test_that("bottleneck_assignment works", {
  cost <- matrix(c(1, 5, 3, 2, 4, 6, 7, 1, 2), 3, 3)
  result <- bottleneck_assignment(cost)

  expect_s3_class(result, "bottleneck_result")
  expect_true(result$bottleneck >= 0)
})

test_that("bottleneck_assignment maximize works", {
  cost <- matrix(c(1, 5, 3, 2, 4, 6, 7, 1, 2), 3, 3)
  result <- bottleneck_assignment(cost, maximize = TRUE)

  expect_s3_class(result, "bottleneck_result")
})

test_that("bottleneck_assignment on larger matrices", {
  set.seed(333)
  cost <- matrix(runif(64), 8, 8)
  result <- bottleneck_assignment(cost)

  expect_s3_class(result, "bottleneck_result")
})

# ------------------------------------------------------------------------------
# Line metric solver tests
# ------------------------------------------------------------------------------

test_that("lap_solve_line_metric with L1 cost works", {
  x <- c(1, 3, 5, 7)
  y <- c(2, 4, 6, 8)

  result <- lap_solve_line_metric(x, y, cost = "L1")

  expect_equal(length(result$match), 4)
})

test_that("lap_solve_line_metric with L2 cost works", {
  x <- c(1, 3, 5)
  y <- c(2, 4, 6, 8)

  result <- lap_solve_line_metric(x, y, cost = "L2")

  expect_equal(length(result$match), 3)
})

test_that("lap_solve_line_metric handles unsorted inputs", {
  x <- c(5, 1, 3)
  y <- c(6, 2, 4, 8)

  result <- lap_solve_line_metric(x, y)

  expect_equal(length(result$match), 3)
})

# ------------------------------------------------------------------------------
# Sinkhorn solver tests
# ------------------------------------------------------------------------------

test_that("sinkhorn works on small matrices", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- sinkhorn(cost, lambda = 1)

  expect_true(is.matrix(result$transport_plan))
  expect_true(all(result$transport_plan >= 0))
})

test_that("sinkhorn with different lambda values", {
  cost <- matrix(runif(9), 3, 3)

  result1 <- sinkhorn(cost, lambda = 0.1)
  result2 <- sinkhorn(cost, lambda = 10)

  expect_true(all(result1$transport_plan >= 0))
  expect_true(all(result2$transport_plan >= 0))
})

test_that("sinkhorn_to_assignment works", {
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  sink_result <- sinkhorn(cost, lambda = 10)

  assignment <- sinkhorn_to_assignment(sink_result)

  expect_equal(length(assignment), 2)
})

# ------------------------------------------------------------------------------
# K-best solutions tests
# ------------------------------------------------------------------------------

test_that("lap_solve_kbest returns multiple solutions", {
  cost <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)

  result <- lap_solve_kbest(cost, k = 3)

  expect_s3_class(result, "lap_solve_kbest_result")
  expect_true(nrow(result) >= 1)
})

test_that("lap_solve_kbest with murty method", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result_murty <- lap_solve_kbest(cost, k = 2, method = "murty")

  expect_s3_class(result_murty, "lap_solve_kbest_result")
})

# ------------------------------------------------------------------------------
# Edge cases that test error paths
# ------------------------------------------------------------------------------

test_that("solvers handle 1x1 matrices", {
  cost <- matrix(5, 1, 1)

  for (method in c("hungarian", "jv", "auction", "ssp", "csflow")) {
    result <- assignment(cost, method = method)
    expect_equal(result$total_cost, 5)
  }
})

test_that("solvers handle 2x2 identity cost", {
  cost <- matrix(c(0, 1, 1, 0), 2, 2)

  for (method in c("hungarian", "jv", "gabow_tarjan", "csa")) {
    result <- assignment(cost, method = method)
    expect_equal(result$total_cost, 0)
  }
})

test_that("solvers agree on random matrices", {
  set.seed(444)
  cost <- matrix(runif(25), 5, 5)

  methods <- c("hungarian", "jv", "csa", "network_simplex")
  results <- lapply(methods, function(m) assignment(cost, method = m))

  costs <- sapply(results, function(r) r$total_cost)
  # All methods should find the same optimal cost
  expect_true(max(costs) - min(costs) < 1e-6)
})
