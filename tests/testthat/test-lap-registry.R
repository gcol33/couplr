# assignment() looks its method up in the solver registry; lap_solvers() lists
# it and register_lap_solver() adds to it.

test_that("lap_solvers() lists every built-in solver once", {
  s <- lap_solvers()
  expect_s3_class(s, "tbl_df")
  expect_false(anyDuplicated(s$method) > 0)
  expect_true(all(c("jv", "hungarian", "auction", "network_simplex",
                    "bruteforce", "csa") %in% s$method))
  expect_identical(sort(s$method[s$potentials]), c("hungarian", "jv"))
  expect_identical(sort(s$method[s$lazy]), c("auction", "jv"))
  expect_true(all(s$builtin))
})

test_that("every built-in solver reaches the optimum through the registry", {
  set.seed(12)
  cost <- matrix(sample(1:50, 36, replace = TRUE), 6, 6)
  best <- assignment(cost, method = "bruteforce")$total_cost
  for (m in setdiff(lap_solvers()$method, "hk01")) {
    expect_equal(assignment(cost, method = m)$total_cost, best, info = m)
  }
})

test_that("an unknown method names the registry", {
  expect_error(assignment(diag(3), method = "nope"), "lap_solvers")
  expect_equal(assignment(diag(3) + 1, method = "ssp")$method_used, "sap")
})

test_that("a registered solver is certified by default", {
  greedy <- function(cost, maximize, ...) {
    match <- integer(nrow(cost))
    free <- rep(TRUE, ncol(cost))
    for (i in seq_len(nrow(cost))) {
      j <- which(free)[which.min(cost[i, free])]
      match[i] <- j
      free[j] <- FALSE
    }
    match
  }
  register_lap_solver("test_greedy", greedy, algorithm = "Row-wise greedy")
  on.exit(rm("test_greedy", envir = couplr:::.lap_registry), add = TRUE)
  expect_true("test_greedy" %in% lap_solvers()$method)
  expect_false(lap_solvers()$builtin[lap_solvers()$method == "test_greedy"])

  # Greedy is optimal here ...
  easy <- matrix(c(1, 9, 9, 9, 1, 9, 9, 9, 1), 3)
  expect_true(certificate(assignment(easy, method = "test_greedy"))$certified_optimal)
  # ... and not here, which the certificate reports.
  hard <- matrix(c(1, 2, 3, 2, 4, 6, 3, 6, 10), 3)
  res <- assignment(hard, method = "test_greedy")
  expect_false(certificate(res)$certified_optimal)
  expect_gt(certificate(res)$max_suboptimality, 0)

  expect_error(register_lap_solver("test_greedy", greedy), "already registered")
  expect_error(register_lap_solver("jv", greedy), "built-in")
  expect_error(register_lap_solver("auto", greedy), "other than")
})

test_that("a solver returning a malformed match is refused", {
  register_lap_solver("test_bad", function(cost, maximize, ...) 1L)
  on.exit(rm("test_bad", envir = couplr:::.lap_registry), add = TRUE)
  expect_error(assignment(diag(3), method = "test_bad"), "one column index per row")
})

test_that("registered solvers return plain integer-valued match vectors", {
  output <- c(1, 2)
  register_lap_solver("test_indices", function(cost, maximize) output)
  on.exit(rm("test_indices", envir = couplr:::.lap_registry), add = TRUE)
  cost <- matrix(c(1, 3, 3, 1), 2)
  valid <- assignment(cost, method = "test_indices")
  expect_identical(valid$match, c(1L, 2L))
  expect_true(certificate(valid)$certified_optimal)

  invalid <- list(c(1.5, 2), c("1", "2"), factor(c("1", "2")),
                  matrix(1:2, 2), c(1 + 0i, 2 + 0i), c(1, Inf),
                  c(NA_real_, 2), c(NaN, 2), c(-1, 2), c(1, 3),
                  list(matching = c(1L, 2L)))
  for (candidate in invalid) {
    output <- candidate
    expect_error(assignment(cost, method = "test_indices"),
                 'Solver "test_indices" must return one column index per row',
                 fixed = TRUE)
  }
})

test_that("a registered solver with potentials passes them to the certificate", {
  via_jv <- function(cost, maximize, ...) {
    d <- assignment_duals(cost, maximize = maximize)
    list(match = d$match, u = d$u, v = d$v)
  }
  register_lap_solver("test_duals", via_jv, potentials = TRUE)
  on.exit(rm("test_duals", envir = couplr:::.lap_registry), add = TRUE)
  set.seed(3)
  cost <- matrix(runif(20), 4)
  res <- assignment(cost, method = "test_duals")
  expect_identical(certificate(res)$exact_duals_source, "solver")
  expect_true(certificate(res)$certified_optimal)
})
