# Exact-arithmetic certification.
#
# The conditions verify_assignment() checks are comparisons of c_ij - u_i - v_j
# against zero. Deciding them in exact arithmetic removes the tolerance from the
# conclusion, and the certificate reports which arithmetic its conclusion is in.

test_that("integer costs certify in exact arithmetic", {
  set.seed(11)
  cost <- matrix(sample(1:200, 100, replace = TRUE), 10, 10)

  cert <- verify_assignment(assignment(cost), cost)

  expect_true(cert$certified_optimal)
  expect_identical(cert$arithmetic, "exact")
  expect_true(cert$exact_certificate)
  expect_true(cert$all_rows_matched)
  expect_equal(cert$n_exact_violations, 0)
  expect_equal(cert$n_exact_untight, 0)
})

test_that("a rectangular integer problem certifies exactly on both orientations", {
  set.seed(12)
  wide <- matrix(sample(1:200, 6 * 20, replace = TRUE), 6, 20)
  tall <- t(wide)

  cert_wide <- verify_assignment(assignment_duals(wide), wide)
  cert_tall <- verify_assignment(assignment_duals(tall), tall)

  expect_identical(cert_wide$arithmetic, "exact")
  expect_identical(cert_tall$arithmetic, "exact")
  expect_true(cert_wide$certified_optimal)
  expect_true(cert_tall$certified_optimal)
  expect_true(cert_tall$transposed)
})

test_that("maximize certifies exactly on integer costs", {
  set.seed(13)
  cost <- matrix(sample(1:200, 64, replace = TRUE), 8, 8)

  cert <- verify_assignment(assignment(cost, maximize = TRUE), cost,
                            maximize = TRUE)

  expect_true(cert$certified_optimal)
  expect_identical(cert$arithmetic, "exact")
})

test_that("arithmetic = 'double' keeps the tolerance conclusion", {
  set.seed(14)
  cost <- matrix(sample(1:200, 64, replace = TRUE), 8, 8)

  cert <- verify_assignment(assignment(cost), cost, arithmetic = "double")

  expect_true(cert$certified_optimal)
  expect_identical(cert$arithmetic, "double")
  # The exact question is not asked in this mode, so it is not answered either.
  expect_false(cert$exact_available)
  expect_false(cert$exact_certificate)
})

test_that("duals off by less than the tolerance fail exactly and pass at tol", {
  set.seed(15)
  cost <- matrix(sample(1:200, 64, replace = TRUE), 8, 8)
  duals <- assignment_duals(cost)
  nudged <- list(u = duals$u + 1e-12, v = duals$v)

  auto <- verify_assignment(duals, cost, duals = nudged)
  strict <- verify_assignment(duals, cost, duals = nudged, arithmetic = "exact")

  expect_true(auto$certified_optimal)
  expect_identical(auto$arithmetic, "double")
  expect_false(auto$exact_certificate)
  expect_gt(auto$n_exact_untight, 0)

  expect_false(strict$certified_optimal)
  expect_identical(strict$arithmetic, "exact")
})

test_that("duals that certify nothing fail in either arithmetic", {
  set.seed(16)
  cost <- matrix(sample(1:200, 64, replace = TRUE), 8, 8)
  duals <- assignment_duals(cost)
  bumped <- list(u = duals$u + 1, v = duals$v)

  auto <- verify_assignment(duals, cost, duals = bumped)
  strict <- verify_assignment(duals, cost, duals = bumped, arithmetic = "exact")

  expect_false(auto$certified_optimal)
  expect_false(strict$certified_optimal)
  expect_gt(auto$n_exact_violations, 0)
})

test_that("an unmatched row blocks the exact conclusion", {
  set.seed(17)
  cost <- matrix(sample(1:200, 64, replace = TRUE), 8, 8)
  duals <- assignment_duals(cost)
  dropped <- duals$match
  dropped[3] <- 0L

  cert <- verify_assignment(dropped, cost,
                            duals = list(u = duals$u, v = duals$v),
                            arithmetic = "exact")

  expect_false(cert$all_rows_matched)
  expect_false(cert$certified_optimal)
})

test_that("the exact sign test agrees with integer arithmetic on dyadic values", {
  # Costs and duals on a grid of 2^-20 are exactly representable, so the sign
  # of c - u - v can be computed independently in integers and compared
  # against what the certificate decided. A 1 x 1 problem isolates one pair.
  scale <- 2^20
  cases <- list(
    c(c = 7, u = 3, v = 4),        # tight
    c(c = 7, u = 3, v = 5),        # infeasible by one unit of the grid
    c(c = 7, u = 3, v = 3)         # feasible with slack
  )

  for (case in cases) {
    cost <- matrix(case[["c"]] / scale, 1, 1)
    duals <- list(u = case[["u"]] / scale, v = case[["v"]] / scale)
    exact_sign <- sign(case[["c"]] - case[["u"]] - case[["v"]])

    cert <- verify_assignment(1L, cost, duals = duals, arithmetic = "exact")

    expect_equal(cert$n_exact_violations, if (exact_sign < 0) 1 else 0)
    expect_equal(cert$n_exact_untight, if (exact_sign == 0) 0 else 1)
    expect_equal(cert$certified_optimal, exact_sign == 0)
  }
})

test_that("an exact certificate never certifies what the tolerance check refuses", {
  # The exact conditions imply the numerical ones at any non-negative
  # tolerance, which is what makes "auto" safe as a default.
  for (seed in 1:15) {
    set.seed(seed)
    cost <- matrix(sample(1:500, 12 * 18, replace = TRUE), 12, 18)
    solved <- assignment_duals(cost)

    strict <- verify_assignment(solved, cost, arithmetic = "exact")
    loose <- verify_assignment(solved, cost, arithmetic = "double")

    expect_true(!strict$certified_optimal || loose$certified_optimal)
  }
})

test_that("costs spread over many orders of magnitude fall back to the tolerance", {
  set.seed(18)
  cost <- matrix(exp(runif(400, -20, 20)), 20, 20)

  cert <- verify_assignment(assignment(cost), cost)

  expect_true(cert$certified_optimal)
  expect_identical(cert$arithmetic, "double")
  expect_gt(cert$n_exact_untight, 0)
})

test_that("the arithmetic argument is validated", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)
  expect_error(verify_assignment(assignment(cost), cost, arithmetic = "quad"),
               "should be one of")
})

test_that("the print method names the arithmetic the conclusion is in", {
  set.seed(19)
  cost <- matrix(sample(1:200, 36, replace = TRUE), 6, 6)

  exact_out <- capture.output(print(verify_assignment(assignment(cost), cost)))
  loose_out <- capture.output(
    print(verify_assignment(assignment(cost), cost, arithmetic = "double")))

  expect_true(any(grepl("exact, no tolerance", exact_out, fixed = TRUE)))
  expect_true(any(grepl("double, tolerance", loose_out, fixed = TRUE)))
})
