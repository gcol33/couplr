# The integer conversion cost scaling performs.
#
# `"csa"` runs on integers, so a real-valued matrix is converted before it is
# solved. The conversion scales the span of the costs after shifting the
# smallest to zero, which is what a complete assignment is invariant under, and
# refuses a span its resolution cannot order rather than returning the arbitrary
# choice a collapsed ordering produces.

test_that("csa agrees with the reference solver on ordinary continuous costs", {
  set.seed(101)
  for (rep in seq_len(10)) {
    cost <- matrix(stats::runif(2500), 50, 50)
    ref <- assignment(cost, method = "jv")
    got <- assignment(cost, method = "csa")
    expect_equal(got$total_cost, ref$total_cost, tolerance = 1e-9)
  }
})

test_that("csa is unaffected by an offset far from the origin", {
  # The span is one unit wide and sits at 1e9. Scaling the magnitude rather
  # than the span spends every digit of resolution on the offset and leaves the
  # variation below the rounding step, which is a wrong answer rather than a
  # slow one.
  set.seed(102)
  base <- matrix(stats::runif(2500), 50, 50)
  for (offset in c(0, 1e6, 1e9)) {
    cost <- base + offset
    ref <- assignment(cost, method = "jv")
    got <- assignment(cost, method = "csa")
    expect_equal(got$total_cost, ref$total_cost, tolerance = 1e-6,
                 info = paste("offset", offset))
  }
})

test_that("csa refuses a cost range its scaling cannot order", {
  # Lognormal costs at this width put the smallest entries a billionth of the
  # largest, so they round together at the bottom of the span. The solver
  # cannot order the cheapest pairs, which are the ones the optimum is made of.
  set.seed(103)
  cost <- matrix(exp(stats::rnorm(3600, 0, 6)), 60, 60)

  expect_error(assignment(cost, method = "csa"), "cost range is too wide")

  # The refusal names what to use instead, and that alternative solves it.
  expect_silent(ref <- assignment(cost, method = "jv"))
  expect_true(is.finite(ref$total_cost))
})

test_that("a refusal is preferred to a certificate-failing answer", {
  # The guard against a regression that trades the error back for the silent
  # wrong answer: whatever csa returns on a range it accepts has to certify.
  set.seed(104)
  for (sigma in c(0.5, 1, 2)) {
    cost <- matrix(exp(stats::rnorm(900, 0, sigma)), 30, 30)
    got <- tryCatch(assignment(cost, method = "csa"), error = function(e) NULL)
    if (is.null(got)) next
    cert <- verify_assignment(got, cost)
    expect_true(cert$certified_optimal, info = paste("sigma", sigma))
  }
})

test_that("integer costs are still solved without conversion", {
  set.seed(105)
  cost <- matrix(sample.int(500L, 900, replace = TRUE), 30, 30)
  storage.mode(cost) <- "double"

  ref <- assignment(cost, method = "jv")
  got <- assignment(cost, method = "csa")

  expect_equal(got$total_cost, ref$total_cost)
  expect_true(verify_assignment(got, cost)$certified_optimal)
})

test_that("csa handles a constant cost matrix", {
  cost <- matrix(3.5, 12, 12)
  got <- assignment(cost, method = "csa")
  expect_equal(got$total_cost, 12 * 3.5)
})
