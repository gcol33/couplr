# ==============================================================================
# Parametric parity tests for every registered animation trace
# ==============================================================================
# For each method returned by animated_methods(), check that:
#
#   - trace runs end-to-end on a battery of small cost matrices
#   - every frame's matching is a valid partial state
#   - no frame uses forbidden (NA/Inf) edges
#   - meta$total_cost matches the C++ production oracle within tolerance
#
# This file grows automatically as new trace_*.R files register more methods:
# nothing here mentions method names. If you add a new trace, it gets tested.
# ==============================================================================

# A small battery of cost matrices covering the cases each trace must handle.
# Kept small so the per-method test budget stays reasonable when many methods
# are registered.
.parity_test_matrices <- function() {
  set.seed(2026)
  list(
    list(name = "3x3 small integer",
         cost = matrix(sample.int(20, 9), nrow = 3)),
    list(name = "4x4 integer with ties",
         cost = matrix(sample.int(5, 16, replace = TRUE), nrow = 4)),
    list(name = "6x6 random integer",
         cost = matrix(sample.int(100, 36), nrow = 6)),
    list(name = "5x5 with forbidden edges",
         cost = local({
           m <- matrix(sample.int(50, 25), nrow = 5)
           m[1, 2] <- NA_real_
           m[3, 4] <- Inf
           m[5, 1] <- NA_real_
           m
         })),
    list(name = "4x4 doubles",
         cost = matrix(runif(16) * 10, nrow = 4))
  )
}

# Methods that don't accept arbitrary cost matrices. hk01 in particular only
# makes sense on 0/1 matrices and on the standard battery would be a no-op.
.parity_method_matrices <- function(method) {
  base <- .parity_test_matrices()
  if (identical(method, "hk01")) {
    set.seed(2027)
    return(list(
      list(name = "4x4 binary balanced",
           cost = matrix(sample(c(0, 1), 16, replace = TRUE, prob = c(0.6, 0.4)), nrow = 4)),
      list(name = "5x5 binary dense",
           cost = matrix(sample(c(0, 1), 25, replace = TRUE, prob = c(0.7, 0.3)), nrow = 5)),
      list(name = "6x6 binary mostly zero",
           cost = matrix(sample(c(0, 1), 36, replace = TRUE, prob = c(0.9, 0.1)), nrow = 6))
    ))
  }
  if (identical(method, "bruteforce")) {
    return(base[1:3])
  }
  base
}

test_that("every registered trace passes full per-frame parity checks", {
  skip_on_cran()
  methods <- animated_methods()
  expect_gt(length(methods), 0)

  for (method in methods) {
    mats <- .parity_method_matrices(method)
    for (case in mats) {
      label <- sprintf("method = %s, case = %s", method, case$name)

      trace_fn <- get_trace_fn(method)

      ok_min <- tryCatch({
        trace <- trace_fn(case$cost, maximize = FALSE)
        validate_trace_parity(trace, case$cost, maximize = FALSE, method = method)
        TRUE
      }, error = function(e) {
        fail(sprintf("[min] %s: %s", label, conditionMessage(e)))
        FALSE
      })

      if (!ok_min) next

      mat_max <- case$cost
      if (!any(is.finite(mat_max))) next

      ok_max <- tryCatch({
        trace <- trace_fn(mat_max, maximize = TRUE)
        validate_trace_parity(trace, mat_max, maximize = TRUE, method = method)
        TRUE
      }, error = function(e) {
        fail(sprintf("[max] %s: %s", label, conditionMessage(e)))
        FALSE
      })
      invisible(ok_max)
    }
  }
  succeed()
})
