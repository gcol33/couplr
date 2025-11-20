library(couplr)
library(microbenchmark)

cat("============================================================\n")
cat("COMPREHENSIVE LAP SOLVER BENCHMARK\n")
cat("============================================================\n\n")

# Test different problem sizes
sizes <- c(50, 100, 200, 500, 1000)

# Available methods
methods <- c("hungarian", "gabow_tarjan", "jv", "auction")

results <- list()

for (n in sizes) {
  cat(sprintf("\n=== Benchmarking n=%d ===\n", n))

  set.seed(123)
  cost <- matrix(runif(n*n, 0, 100), n, n)

  timings <- list()

  for (method in methods) {
    cat(sprintf("  Testing %s... ", method))

    tryCatch({
      # Warmup
      invisible(lap_solve(cost, method=method))

      # Benchmark
      bm <- microbenchmark(
        lap_solve(cost, method=method),
        times = 5,
        unit = "ms"
      )

      mean_time <- mean(bm$time) / 1e6  # Convert to milliseconds
      timings[[method]] <- mean_time

      cat(sprintf("%.2f ms\n", mean_time))

    }, error = function(e) {
      cat(sprintf("FAILED: %s\n", e$message))
      timings[[method]] <- NA
    })
  }

  results[[as.character(n)]] <- timings
}

cat("\n\n============================================================\n")
cat("SUMMARY TABLE\n")
cat("============================================================\n\n")

# Print table header
cat(sprintf("%-8s", "Size"))
for (method in methods) {
  cat(sprintf(" | %-12s", method))
}
cat("\n")
cat(paste(rep("-", 70), collapse=""))
cat("\n")

# Print results
for (n in sizes) {
  cat(sprintf("%-8d", n))
  timings <- results[[as.character(n)]]

  for (method in methods) {
    time <- timings[[method]]
    if (is.na(time)) {
      cat(sprintf(" | %-12s", "FAILED"))
    } else {
      cat(sprintf(" | %9.2f ms", time))
    }
  }
  cat("\n")
}

cat("\n\n============================================================\n")
cat("WINNER BY SIZE\n")
cat("============================================================\n\n")

for (n in sizes) {
  timings <- results[[as.character(n)]]
  valid_timings <- timings[!is.na(timings)]

  if (length(valid_timings) > 0) {
    winner <- names(which.min(valid_timings))
    winner_time <- min(unlist(valid_timings))

    cat(sprintf("n=%4d: %s (%.2f ms)\n", n, winner, winner_time))
  }
}

cat("\n\n============================================================\n")
cat("SPEEDUP ANALYSIS\n")
cat("============================================================\n\n")

cat("Gabow-Tarjan vs Hungarian:\n")
for (n in sizes) {
  timings <- results[[as.character(n)]]
  if (!is.na(timings$gabow_tarjan) && !is.na(timings$hungarian)) {
    speedup <- timings$hungarian / timings$gabow_tarjan
    cat(sprintf("  n=%4d: %.2fx faster\n", n, speedup))
  }
}

cat("\nGabow-Tarjan vs JV:\n")
for (n in sizes) {
  timings <- results[[as.character(n)]]
  if (!is.na(timings$gabow_tarjan) && !is.na(timings$jv)) {
    speedup <- timings$jv / timings$gabow_tarjan
    cat(sprintf("  n=%4d: %.2fx faster\n", n, speedup))
  }
}

cat("\n\n============================================================\n")
cat("RECOMMENDATION FOR AUTO-SELECT\n")
cat("============================================================\n\n")

# Analyze crossover points
cat("Based on benchmarks:\n\n")

for (i in 1:length(sizes)) {
  n <- sizes[i]
  timings <- results[[as.character(n)]]
  valid_timings <- timings[!is.na(timings)]

  if (length(valid_timings) > 0) {
    sorted <- sort(unlist(valid_timings))
    winner <- names(sorted)[1]
    second <- names(sorted)[2]

    cat(sprintf("n=%4d: Use %s (%.2f ms vs %s at %.2f ms)\n",
                n, winner, sorted[1], second, sorted[2]))
  }
}

cat("\n")

# Save results
saveRDS(results, "benchmark_all_solvers_results.rds")
cat("Results saved to: benchmark_all_solvers_results.rds\n")
