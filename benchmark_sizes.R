# Benchmark different problem sizes to determine greedy threshold

library(couplr)
library(bench)

set.seed(123)

# Test different sizes
sizes <- c(100, 500, 1000, 2000)

cat("============================================================\n")
cat("BENCHMARK: Finding optimal threshold for greedy matching\n")
cat("============================================================\n\n")

results_summary <- data.frame(
  size = integer(),
  optimal_median = numeric(),
  greedy_median = numeric(),
  speedup = numeric(),
  stringsAsFactors = FALSE
)

for (n in sizes) {
  cat("\n========== n =", n, "==========\n")

  left <- data.frame(
    id = 1:n,
    age = rnorm(n, 45, 12),
    income = rnorm(n, 50000, 15000),
    education = rnorm(n, 14, 3)
  )

  right <- data.frame(
    id = 1:n,
    age = rnorm(n, 42, 10),
    income = rnorm(n, 48000, 12000),
    education = rnorm(n, 13, 2.5)
  )

  vars <- c("age", "income", "education")

  result <- bench::mark(
    optimal = match_couples(left, right, vars = vars, method = "auto"),
    greedy  = greedy_couples(left, right, vars = vars, strategy = "row_best"),
    iterations = 3,
    check = FALSE
  )

  opt_time <- as.numeric(result$median[1])
  greedy_time <- as.numeric(result$median[2])
  speedup <- opt_time / greedy_time

  cat("Optimal median:", format(result$median[1]), "\n")
  cat("Greedy median: ", format(result$median[2]), "\n")
  cat("Speedup:       ", sprintf("%.2fx", speedup), "\n")

  results_summary <- rbind(results_summary, data.frame(
    size = n,
    optimal_median = opt_time,
    greedy_median = greedy_time,
    speedup = speedup
  ))
}

cat("\n\n============================================================\n")
cat("SUMMARY\n")
cat("============================================================\n\n")

print(results_summary)

cat("\n\nRECOMMENDATION:\n")
cat("Based on the results:\n")
for (i in 1:nrow(results_summary)) {
  cat(sprintf("- n=%d: optimal=%.2fs, greedy=%.2fs, speedup=%.2fx\n",
              results_summary$size[i],
              results_summary$optimal_median[i],
              results_summary$greedy_median[i],
              results_summary$speedup[i]))
}

# Save results
saveRDS(results_summary, "benchmark_sizes_results.rds")
cat("\nResults saved to: benchmark_sizes_results.rds\n")
