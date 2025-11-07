test_that("ssp == jv on small squares", {
  set.seed(1)
  for (n in 2:7) {
    M <- matrix(runif(n*n), n)
    a <- assignment(M, method = "jv")
    s <- assignment(M, method = "sap")
    expect_equal(a$total_cost, s$total_cost, tolerance = 1e-10)
  }
})

test_that("ssp handles rectangular with many NA", {
  set.seed(2)
  n <- 8; m <- 20
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), size = floor(0.5*length(M)))] <- NA
  for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

  a <- assignment(M, method = "jv")
  s <- assignment(M, method = "sap")
  expect_equal(a$total_cost, s$total_cost, tolerance = 1e-8)
})

test_that("ssp errors when a row is entirely forbidden", {
  M <- matrix(1, 3, 5)
  M[3, ] <- NA
  expect_error(assignment(M, method = "sap"), "Infeasible")
})

test_that("maximize works with ssp", {
  set.seed(3)
  n <- 6; m <- 10
  M <- matrix(runif(n*m), n, m)
  M[sample.int(length(M), size = 10)] <- NA
  for (i in 1:n) if (all(is.na(M[i,]))) M[i, sample.int(m, 1)] <- runif(1)

  a_max <- assignment(M, maximize = TRUE,  method = "sap")
  a_min <- assignment(-M, maximize = FALSE, method = "sap")
  expect_equal(a_max$total_cost, -a_min$total_cost, tolerance = 1e-8)
})
