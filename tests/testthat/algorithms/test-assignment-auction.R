test_that("auction == jv on small square & rectangular with NA", {
  set.seed(1)
  for (n in 2:8) {
    m <- n + 2
    M <- matrix(runif(n*m), n, m)
    M[sample.int(length(M), size = floor(0.2*length(M)))] <- NA
    for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

    a1 <- assignment(M, method = "jv")
    a2 <- assignment(M, method = "auction")
    expect_equal(a1$total_cost, a2$total_cost, tolerance = 1e-8)

    a1 <- assignment(M, maximize = TRUE, method = "jv")
    a2 <- assignment(M, maximize = TRUE, method = "auction")
    expect_equal(a1$total_cost, a2$total_cost, tolerance = 1e-8)
  }
})

test_that("auction(errors) when a row is entirely forbidden", {
  M <- matrix(1, 3, 5)
  M[2,] <- NA
  expect_error(assignment(M, method = "auction"), "Infeasible")
})

test_that("auction with explicit epsilon matches JV", {
  set.seed(2)
  for (n in 3:6) {
    m <- n + 1
    M <- matrix(runif(n*m), n, m)
    M[sample.int(length(M), size = floor(0.15*length(M)))] <- NA
    for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

    a_jv <- assignment(M, method = "jv")
    for (eps in c(1e-6, 1e-9, 1e-12)) {
      a_auc <- assignment(M, method = "auction", auction_eps = eps)
      expect_equal(a_auc$total_cost, a_jv$total_cost, tolerance = 1e-8)
    }

    a_jv <- assignment(M, maximize = TRUE, method = "jv")
    for (eps in c(1e-6, 1e-9, 1e-12)) {
      a_auc <- assignment(M, method = "auction", maximize = TRUE, auction_eps = eps)
      expect_equal(a_auc$total_cost, a_jv$total_cost, tolerance = 1e-8)
    }
  }
})

test_that("auction_scaled(pow2) == JV on small square & rectangular with NA", {
  set.seed(3)
  for (n in 2:7) {
    m <- n + 3
    M <- matrix(runif(n*m), n, m)
    M[sample.int(length(M), size = floor(0.25*length(M)))] <- NA
    for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

    # direct call to the scaled interface
    s_min <- lap_solve_auction_scaled(M, maximize = FALSE, schedule = "pow2")
    s_max <- lap_solve_auction_scaled(M, maximize = TRUE,  schedule = "pow2")

    a_min <- assignment(M, method = "jv")
    a_max <- assignment(M, maximize = TRUE, method = "jv")

    expect_equal(sum(M[cbind(seq_len(n), s_min$match)]), a_min$total_cost, tolerance = 1e-8)
    expect_equal(sum(M[cbind(seq_len(n), s_max$match)]), a_max$total_cost, tolerance = 1e-8)
  }
})

test_that("auction_scaled errors on forbidden row", {
  M <- matrix(1, 4, 6)
  M[4,] <- NA
  expect_error(lap_solve_auction_scaled(M, maximize = FALSE, schedule = "pow2"), "Infeasible")
})
