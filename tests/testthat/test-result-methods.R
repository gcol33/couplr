# The couplr_result family shares print(), summary(), glance(), tidy(),
# as.data.frame() and the accessors certificate(), potentials(), unmatched_units().

result_fixture <- function() {
  set.seed(31)
  left <- data.frame(id = paste0("L", 1:6), x = rnorm(6), y = rnorm(6))
  right <- data.frame(id = paste0("R", 1:10), x = rnorm(10), y = rnorm(10))
  list(left = left, right = right)
}

test_that("every matching design inherits the shared methods", {
  d <- result_fixture()
  fits <- list(
    match_couples(d$left, d$right, vars = c("x", "y")),
    full_match(d$left, d$right, vars = c("x", "y")),
    cem_match(d$left, d$right, vars = c("x", "y")),
    cardinality_match(d$left, d$right, vars = c("x", "y"))
  )
  for (fit in fits) {
    expect_s3_class(fit, "couplr_result")
    g <- glance(fit)
    expect_s3_class(g, "tbl_df")
    expect_equal(nrow(g), 1L)
    expect_equal(g$n_left, 6L)
    expect_equal(g$n_right, 10L)
    expect_true(is.data.frame(as.data.frame(fit)))
    expect_equal(nrow(tidy(fit)), nrow(as.data.frame(fit)))
    out <- capture.output(print(fit))
    expect_no_warning(full <- summary(fit))
    expect_lt(length(out), length(capture.output(print(full))))
  }
})

test_that("glance() reads the certificate, the search and the cardinality report", {
  d <- result_fixture()
  dense <- match_couples(d$left, d$right, vars = c("x", "y"), certify = TRUE)
  expect_true(glance(dense)$certified)
  expect_equal(glance(dense)$max_suboptimality, 0)

  imp <- match_couples(d$left, d$right, vars = c("x", "y"),
                       memory_mode = "implicit")
  g <- glance(imp)
  expect_true(all(c("n_rounds", "candidate_edges", "possible_edges") %in% names(g)))
  expect_equal(g$total_distance, glance(dense)$total_distance)

  card <- cardinality_match(d$left, d$right, vars = c("x", "y"))
  expect_true(all(c("best_possible", "gap") %in% names(glance(card))))
  expect_equal(glance(card)$design, "cardinality")
})

test_that("the accessors return the stored parts", {
  d <- result_fixture()
  m <- match_couples(d$left, d$right, vars = c("x", "y"), certify = TRUE,
                     max_distance = 1)
  expect_identical(certificate(m), m$certificate)
  expect_identical(potentials(m), m$potentials)
  expect_identical(unmatched_units(m), m$unmatched)
  expect_named(unmatched_units(m), c("left", "right"))

  ce <- cem_match(d$left, d$right, vars = c("x", "y"))
  um <- unmatched_units(ce)
  expect_equal(length(um$left) + glance(ce)$n_matched_left, 6L)

  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), 3)
  a <- assignment(cost, method = "jv", certify = TRUE)
  expect_s3_class(certificate(a), "assignment_certificate")
  expect_named(potentials(a), c("u", "v"))
  expect_length(unmatched_units(a), 0L)
  expect_error(certificate(1), "no method")
})

test_that("summary() lists the unmatched units", {
  d <- result_fixture()
  m <- suppressWarnings(match_couples(d$left, d$right, vars = c("x", "y"),
                                      max_distance = 0.5))
  out <- capture.output(print(summary(m)))
  expect_true(any(grepl("Unmatched left", out)))
})

test_that("the KS statistic matches stats::ks.test without its tie warning", {
  set.seed(5)
  x <- sample(1:5, 40, replace = TRUE)
  y <- sample(1:6, 50, replace = TRUE)
  ref <- suppressWarnings(stats::ks.test(x, y)$statistic)
  expect_no_warning(ks <- couplr:::.ks_statistic(x, y))
  expect_equal(ks, unname(ref))
  expect_equal(couplr:::.ks_statistic(c(1, 2), c(1, 2), c(1, 3), c(3, 1)), 0.5)
  expect_true(is.na(couplr:::.ks_statistic(numeric(0), 1)))
})

test_that("balance_diagnostics() raises no warning on tied covariates", {
  set.seed(6)
  left <- data.frame(id = 1:30, g = sample(1:3, 30, TRUE), x = rnorm(30))
  right <- data.frame(id = 31:80, g = sample(1:3, 50, TRUE), x = rnorm(50))
  m <- match_couples(left, right, vars = c("g", "x"))
  expect_no_warning(balance_diagnostics(m, left, right, vars = c("g", "x")))
})

test_that("the Mahalanobis precision comes from the Cholesky factor", {
  set.seed(7)
  A <- crossprod(matrix(rnorm(16), 4))
  expect_equal(couplr:::mahalanobis_precision(A), solve(A), tolerance = 1e-10)
  expect_error(couplr:::mahalanobis_precision(matrix(1, 2, 2)), "singular")
})

test_that("certificate print is shorter than its summary", {
  cost <- matrix(c(4, 2, 5, 3, 3, 6, 7, 5, 4), 3)
  cert <- verify_assignment(assignment(cost), cost)
  expect_lt(length(capture.output(print(cert))),
            length(capture.output(print(summary(cert)))))
  g <- glance(cert)
  expect_true(g$certified)
  expect_identical(g$arithmetic, cert$arithmetic)
})
