# test-gabow_tarjan_complexity.R
# Empirical complexity tests for Gabow-Tarjan algorithm
# Expected: O(n³ log C) which should show roughly cubic growth in n

test_that("Gabow-Tarjan has O(n³ log C) complexity", {
  skip_on_cran()  # Skip on CRAN due to timing variability

  # Test sizes - need enough range to see cubic growth
  sizes <- c(20, 40, 60, 80, 100)
  n_reps <- 3  # Replications per size for stability

  timings <- data.frame(
    n = integer(),
    time_ms = numeric()
  )

  set.seed(42)

  for (n in sizes) {
    for (rep in seq_len(n_reps)) {
      # Random cost matrix with integer costs in [1, 1000]
      cost <- matrix(sample(1:1000, n * n, replace = TRUE), nrow = n, ncol = n)

      # Time the Gabow-Tarjan solver
      start <- Sys.time()
      result <- lap_solve(cost, method = "gabow_tarjan")
      elapsed <- as.numeric(Sys.time() - start, units = "secs") * 1000

      timings <- rbind(timings, data.frame(n = n, time_ms = elapsed))

      # Verify correctness
      expect_equal(nrow(result), n)
      expect_true(attr(result, "total_cost") > 0)
    }
  }

  # Aggregate by size
  agg <- aggregate(time_ms ~ n, data = timings, FUN = median)

  # Fit log-log regression: log(time) = a + b * log(n)
  # For O(n³), we expect b ≈ 3
  # For O(n⁴), we expect b ≈ 4
  fit <- lm(log(time_ms) ~ log(n), data = agg)
  exponent <- coef(fit)[2]

  # Print results for debugging
  cat("\n\nGabow-Tarjan Complexity Analysis:\n")
  cat("=================================\n")
  cat(sprintf("%-6s  %10s  %10s\n", "n", "time(ms)", "ratio"))
  for (i in seq_len(nrow(agg))) {
    ratio <- if (i == 1) NA else agg$time_ms[i] / agg$time_ms[i-1]
    cat(sprintf("%-6d  %10.2f  %10.2f\n", agg$n[i], agg$time_ms[i],
                if (is.na(ratio)) 0 else ratio))
  }
  cat(sprintf("\nFitted exponent: %.2f\n", exponent))
  cat(sprintf("Expected for O(n³): 3.0\n"))
  cat(sprintf("Expected for O(n⁴): 4.0\n"))

  # For n doubling (20→40, 40→80), expected ratio:
  # O(n³): 2³ = 8
  # O(n⁴): 2⁴ = 16
  # We check that exponent is closer to 3 than to 4
  expect_lt(exponent, 3.7,
            label = sprintf("Exponent %.2f should be < 3.7 (closer to cubic than quartic)", exponent))
  expect_gt(exponent, 2.0,
            label = sprintf("Exponent %.2f should be > 2.0 (at least quadratic)", exponent))
})

test_that("Gabow-Tarjan scales better than O(n⁴) on worst-case-like inputs", {
  skip_on_cran()

  # Create matrices that might trigger worst-case behavior:
  # All costs equal (many ties in slack values)
  sizes <- c(30, 50, 70)
  n_reps <- 3

  timings <- data.frame(n = integer(), time_ms = numeric())

  for (n in sizes) {
    for (rep in seq_len(n_reps)) {
      # Uniform costs - potential worst case for naive implementation
      cost <- matrix(1, nrow = n, ncol = n)

      start <- Sys.time()
      result <- lap_solve(cost, method = "gabow_tarjan")
      elapsed <- as.numeric(Sys.time() - start, units = "secs") * 1000

      timings <- rbind(timings, data.frame(n = n, time_ms = elapsed))
      expect_equal(nrow(result), n)
    }
  }

  agg <- aggregate(time_ms ~ n, data = timings, FUN = median)

  # Fit exponent
  if (nrow(agg) >= 2) {
    fit <- lm(log(time_ms) ~ log(n), data = agg)
    exponent <- coef(fit)[2]

    cat("\n\nUniform Cost Matrix (worst-case-like):\n")
    cat("======================================\n")
    cat(sprintf("Fitted exponent: %.2f\n", exponent))

    # Should still be cubic, not quartic
    expect_lt(exponent, 4.0,
              label = sprintf("Exponent %.2f should be < 4.0 on uniform costs", exponent))
  }
})

test_that("Gabow-Tarjan matches other solvers on timing order of magnitude", {
  skip_on_cran()

  n <- 80
  set.seed(123)
  cost <- matrix(sample(1:100, n * n, replace = TRUE), nrow = n, ncol = n)

  # Time different solvers
  time_gt <- system.time(res_gt <- lap_solve(cost, method = "gabow_tarjan"))["elapsed"]
  time_jv <- system.time(res_jv <- lap_solve(cost, method = "jv"))["elapsed"]
  time_hungarian <- system.time(res_hung <- lap_solve(cost, method = "hungarian"))["elapsed"]

  cat("\n\nSolver Comparison (n=80):\n")
  cat("=========================\n")
  cat(sprintf("Gabow-Tarjan: %.4f s\n", time_gt))
  cat(sprintf("JV:           %.4f s\n", time_jv))
  cat(sprintf("Hungarian:    %.4f s\n", time_hungarian))

  # All should produce same optimal cost
  expect_equal(attr(res_gt, "total_cost"), attr(res_jv, "total_cost"))
  expect_equal(attr(res_gt, "total_cost"), attr(res_hung, "total_cost"))

  # GT shouldn't be more than 200x slower than JV (would indicate O(n⁴))
  # This is a loose bound to accommodate CI variability; in practice should be much closer
  if (time_jv > 0.001) {  # Avoid division issues with very fast times
    ratio <- time_gt / time_jv
    cat(sprintf("GT/JV ratio:  %.1f\n", ratio))
    expect_lt(ratio, 200,
              label = "Gabow-Tarjan should not be >200x slower than JV")
  }
})
