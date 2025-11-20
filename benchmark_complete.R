library(couplr)
library(microbenchmark)

cat("============================================================\n")
cat("COMPLETE LAP SOLVER BENCHMARK - ALL METHODS\n")
cat("============================================================\n\n")

# Test different problem sizes (up to 500)
sizes <- c(50, 100, 200, 500)

# ALL available methods (cost-scaling methods: csflow, ssp; hk01 only for binary)
methods <- c("hungarian", "gabow_tarjan", "jv", "auction", "auction_scaled", "sap", "csflow", "ssp")

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
  cat(sprintf(" | %-13s", method))
}
cat("\n")
cat(paste(rep("-", 100), collapse=""))
cat("\n")

# Print results
for (n in sizes) {
  cat(sprintf("%-8d", n))
  timings <- results[[as.character(n)]]

  for (method in methods) {
    time <- timings[[method]]
    if (is.na(time)) {
      cat(sprintf(" | %-13s", "FAILED"))
    } else {
      cat(sprintf(" | %10.2f ms", time))
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
    sorted <- sort(unlist(valid_timings))
    winner <- names(sorted)[1]
    winner_time <- sorted[1]
    second <- names(sorted)[2]
    second_time <- sorted[2]

    cat(sprintf("n=%4d: %s (%.2f ms) << %.1fx faster than %s (%.2f ms)\n",
                n, winner, winner_time, second_time/winner_time, second, second_time))
  }
}

cat("\n\n============================================================\n")
cat("RECOMMENDATION FOR AUTO-SELECT\n")
cat("============================================================\n\n")

# Analyze which method wins at each size
cat("Based on benchmarks:\n\n")

for (i in 1:length(sizes)) {
  n <- sizes[i]
  timings <- results[[as.character(n)]]
  valid_timings <- timings[!is.na(timings)]

  if (length(valid_timings) > 0) {
    sorted <- sort(unlist(valid_timings))
    winner <- names(sorted)[1]

    cat(sprintf("n=%4d: Use %s (%.2f ms)\n", n, winner, sorted[1]))
  }
}

cat("\n")

# Save results
saveRDS(results, "benchmark_complete_results.rds")
cat("Results saved to: benchmark_complete_results.rds\n")
