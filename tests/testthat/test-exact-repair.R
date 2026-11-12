# A complete matching is made exactly optimal before it is certified. The
# instances below hold two matchings whose totals differ by 2^-60, which a
# double cannot see once a cost of 1 is in the sum.

tiny <- 2^-60

test_that("an exactly improving cycle is applied", {
  # (1,1),(2,2) totals 1 + 2^-60; (1,2),(2,1) totals 1.
  cost <- matrix(c(1, 0, 1, tiny), 2, 2)
  start <- list(match = c(1L, 2L))
  expect_false(verify_assignment(start, cost)$exact_certificate)

  fixed <- couplr:::.exact_assignment(start, cost, maximize = FALSE)
  expect_identical(fixed$match, c(2L, 1L))
  expect_identical(fixed$total_cost, 1)
  cert <- verify_assignment(fixed, cost)
  expect_true(cert$exact_certificate)
  expect_identical(cert$arithmetic, "exact")
})

test_that("an exactly improving path to a free column is applied", {
  # Column 3 is free; moving row 2 onto it lowers the total by 2^-60.
  cost <- matrix(c(1, 10, 1, tiny, 10, 0), 2, 3)
  start <- list(match = c(1L, 2L))
  expect_false(verify_assignment(start, cost)$exact_certificate)

  fixed <- couplr:::.exact_assignment(start, cost, maximize = FALSE)
  expect_true(verify_assignment(fixed, cost)$exact_certificate)
  expect_identical(fixed$total_cost, 1)

  # The same instance with more rows than columns.
  fixed_t <- couplr:::.exact_assignment(list(match = c(1L, 2L, 0L)), t(cost),
                                        maximize = FALSE)
  expect_true(verify_assignment(fixed_t, t(cost))$exact_certificate)
})

test_that("the repair works on maximization", {
  cost <- -matrix(c(1, 0, 1, tiny), 2, 2)
  fixed <- couplr:::.exact_assignment(list(match = c(1L, 2L)), cost,
                                      maximize = TRUE)
  expect_identical(fixed$match, c(2L, 1L))
  expect_identical(fixed$total_cost, -1)
  expect_true(verify_assignment(fixed, cost, maximize = TRUE)$exact_certificate)
})

test_that("an exactly optimal matching is returned unchanged", {
  set.seed(3)
  cost <- matrix(sample(1:20, 36, TRUE), 6, 6)
  res <- assignment(cost)
  expect_identical(couplr:::.exact_assignment(res, cost, FALSE), res)
})

test_that("any complete matching is improved to the optimum, dense and lazy", {
  set.seed(11)
  left <- data.frame(id = 1:7, x = rnorm(7), y = rnorm(7))
  right <- data.frame(id = 8:17, x = rnorm(10), y = rnorm(10))
  dense <- compute_distance_matrix(as.matrix(left[, c("x", "y")]),
                                   as.matrix(right[, c("x", "y")]),
                                   distance = "euclidean")
  spec <- build_cost_matrix(left, right, vars = c("x", "y"), memory_mode = "lazy")
  best <- assignment(dense, method = "jv")$total_cost
  start <- list(match = sample(10L, 7L))

  for (cost in list(dense, spec)) {
    fixed <- couplr:::.exact_assignment(start, cost, maximize = FALSE)
    expect_equal(fixed$total_cost, best, tolerance = 1e-12)
    expect_true(verify_assignment(fixed, cost)$exact_certificate)
  }
})

test_that("certified solves carry an exact certificate on sub-rounding ties", {
  # Cells mix integers with multiples of 2^-60, so matchings tie in doubles and
  # differ exactly. Which solver stops on the worse one depends on the solver
  # and the platform; the certified answer is exact either way.
  set.seed(1)
  methods <- c("jv", "hungarian", "auction", "csa", "network_simplex")
  for (rep in 1:40) {
    n <- sample(3:6, 1)
    m <- n + sample(0:2, 1)
    big <- matrix(sample(1:3, n * m, TRUE), n, m)
    small <- matrix(sample(0:3, n * m, TRUE) * tiny, n, m)
    cost <- ifelse(matrix(runif(n * m) < 0.5, n, m), big, small)
    for (meth in methods) {
      res <- assignment(cost, method = meth, certify = TRUE)
      if (!identical(res$status, "optimal")) next
      cert <- certificate(res)
      expect_true(cert$exact_certificate, info = paste(meth, rep))
      expect_equal(res$total_cost, sum(cost[cbind(seq_len(n), res$match)]))
    }
    duals <- assignment_duals(cost, certify = TRUE)
    expect_true(duals$certificate$exact_certificate, info = paste("duals", rep))
  }
})
