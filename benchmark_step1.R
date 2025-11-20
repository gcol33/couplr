# Benchmark Script for Step 1 Performance Analysis
# Compare current vs optimized implementations

library(testthat)
library(microbenchmark)
library(couplr)

# Helper: Generate random cost matrix
generate_cost_matrix <- function(n, sparsity = 1.0, max_cost = 100) {
  cost <- matrix(runif(n * n, 0, max_cost), n, n)

  if (sparsity < 1.0) {
    # Make sparse by setting some edges to Inf
    n_remove <- floor((1 - sparsity) * n * n)
    remove_idx <- sample(1:(n*n), n_remove)
    cost[remove_idx] <- Inf
  }

  cost
}

# Benchmark Step 1 indirectly through full solver
benchmark_solver <- function(sizes = c(50, 100, 200, 500),
                              sparsity = 1.0,
                              n_reps = 10) {
  results <- data.frame(
    n = integer(),
    time_mean = numeric(),
    time_sd = numeric(),
    time_min = numeric(),
    time_max = numeric()
  )

  for (n in sizes) {
    cat(sprintf("\n=== Benchmarking n=%d ===\n", n))

    # Generate test matrix
    cost <- generate_cost_matrix(n, sparsity)

    # Run benchmark using Gabow-Tarjan method
    timing <- microbenchmark(
      lap_solve(cost, maximize = FALSE, method = "gabow_tarjan"),
      times = n_reps
    )

    summary_stats <- summary(timing)

    # Extract timing values and convert from microseconds to milliseconds
    mean_time <- summary_stats$mean / 1000
    sd_time <- sd(timing$time) / 1000  # Calculate SD from raw times
    min_time <- summary_stats$min / 1000
    max_time <- summary_stats$max / 1000

    results <- rbind(results, data.frame(
      n = n,
      time_mean = mean_time,
      time_sd = sd_time,
      time_min = min_time,
      time_max = max_time
    ))

    cat(sprintf("  Mean: %.2f ms (sd=%.2f)\n",
                mean_time,
                sd_time))
  }

  results
}

# Profile Step 1 specifically by examining iteration count
profile_step1_iterations <- function(n = 100, verbose = TRUE) {
  cost <- generate_cost_matrix(n)

  # This would require instrumentation in C++ code
  # For now, we estimate based on complexity

  # Expected iterations per scale: O(sqrt(n))
  expected_iterations_per_scale <- ceiling(sqrt(n))

  # Number of scales: O(log(nN))
  max_cost <- max(cost[is.finite(cost)])
  n_scales <- ceiling(log2((n + 1) * max_cost))

  # Total Step 1 calls: O(sqrt(n) * log(nN))
  total_step1_calls <- expected_iterations_per_scale * n_scales

  if (verbose) {
    cat(sprintf("n = %d\n", n))
    cat(sprintf("Expected iterations per scale: %d\n", expected_iterations_per_scale))
    cat(sprintf("Number of scales: %d\n", n_scales))
    cat(sprintf("Total Step 1 calls: %d\n", total_step1_calls))
  }

  list(
    n = n,
    iterations_per_scale = expected_iterations_per_scale,
    n_scales = n_scales,
    total_step1_calls = total_step1_calls
  )
}

# Complexity analysis
analyze_complexity <- function(results) {
  cat("\n=== Complexity Analysis ===\n")

  # Fit power law: time ~ n^k
  log_n <- log(results$n)
  log_time <- log(results$time_mean)

  fit <- lm(log_time ~ log_n)
  exponent <- coef(fit)[2]

  cat(sprintf("Empirical complexity: O(n^%.2f)\n", exponent))
  cat(sprintf("Expected (current): O(n^%.2f) for dense graphs\n", 2.5))
  cat(sprintf("Expected (optimized): O(n^%.2f) for dense graphs\n", 1.5))
  cat(sprintf("Paper target: O(n^%.2f) for dense graphs\n", 1.5))

  if (exponent > 2.3) {
    cat("\n⚠️  WARNING: Complexity is WORSE than expected!\n")
    cat("    This suggests bottlenecks beyond Step 1.\n")
  } else if (exponent > 2.0) {
    cat("\n📊 Complexity matches current O(n^2.5) prediction.\n")
    cat("    Step 1 optimizations will help significantly!\n")
  } else {
    cat("\n✅ Complexity is better than expected.\n")
    cat("    May already benefit from sparsity or other factors.\n")
  }

  list(
    exponent = exponent,
    r_squared = summary(fit)$r.squared
  )
}

# Main benchmark runner
run_full_benchmark <- function() {
  cat(strrep("=", 60), "\n")
  cat("STEP 1 PERFORMANCE BENCHMARK\n")
  cat(strrep("=", 60), "\n")

  # 1. Small scale benchmark (dense graphs)
  cat("\n### Dense Graphs ###\n")
  results_dense <- benchmark_solver(
    sizes = c(50, 100, 200, 300),
    sparsity = 1.0,
    n_reps = 5
  )

  # 2. Sparse graph benchmark
  cat("\n### Sparse Graphs (50% density) ###\n")
  results_sparse <- benchmark_solver(
    sizes = c(100, 200, 500, 1000),
    sparsity = 0.5,
    n_reps = 3
  )

  # 3. Complexity analysis
  cat("\n\n")
  complexity_dense <- analyze_complexity(results_dense)
  cat("\nSparse graph exponent: ")
  complexity_sparse <- analyze_complexity(results_sparse)

  # 4. Iteration profiling
  cat("\n### Iteration Profiling ###\n")
  for (n in c(100, 500, 1000)) {
    profile_step1_iterations(n, verbose = TRUE)
    cat("\n")
  }

  # Return results
  list(
    dense = results_dense,
    sparse = results_sparse,
    complexity_dense = complexity_dense,
    complexity_sparse = complexity_sparse
  )
}

# Run if executed directly
if (!interactive()) {
  results <- run_full_benchmark()

  # Save results
  saveRDS(results, "benchmark_step1_results.rds")
  cat("\nResults saved to: benchmark_step1_results.rds\n")
}

# Usage:
# source("benchmark_step1.R")
# results <- run_full_benchmark()
#
# # Plot results
# plot(results$dense$n, results$dense$time_mean,
#      type = "b", log = "xy",
#      xlab = "Problem size (n)", ylab = "Time (ms)",
#      main = "Gabow-Tarjan Performance (Dense Graphs)")
