# Benchmark for README Performance Section
# Generate real benchmark numbers for n=1000 comparison

library(couplr)
library(bench)
library(dplyr)

set.seed(123)

# Generate realistic test data (n = 1000)
n <- 1000

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

cat("============================================================\n")
cat("README BENCHMARK: n = 1000 per group\n")
cat("============================================================\n\n")

cat("Running benchmark (this may take a few minutes)...\n\n")

# Run the actual benchmark
results <- bench::mark(
  optimal = match_couples(left, right, vars = vars, method = "auto"),
  greedy  = greedy_couples(left, right, vars = vars, strategy = "row_best"),
  iterations = 10,
  check = FALSE  # Don't check that results are identical (they won't be)
)

cat("============================================================\n")
cat("RESULTS\n")
cat("============================================================\n\n")

print(results)

cat("\n\n============================================================\n")
cat("FORMATTED FOR README\n")
cat("============================================================\n\n")

cat("```r\n")
cat("# Optimal matching\n")
cat("bench::mark(\n")
cat("  optimal = match_couples(left, right, vars, method = \"auto\"),\n")
cat("  greedy  = greedy_couples(left, right, vars, strategy = \"row_best\")\n")
cat(")\n")
cat("#> # A tibble: 2 × 6\n")
cat("#>   expression      min   median `itr/sec` mem_alloc `gc/sec`\n")
cat("#>   <bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>\n")

# Format the results
for (i in 1:nrow(results)) {
  expr <- as.character(results$expression[i])
  min_val <- format(results$min[i])
  median_val <- format(results$median[i])
  itr_sec <- sprintf("%.2f", results$`itr/sec`[i])
  mem_alloc <- format(results$mem_alloc[i])
  gc_sec <- sprintf("%.1f", results$`gc/sec`[i])

  cat(sprintf("#> %d %-10s %7s %8s %9s %10s %8s\n",
              i, expr, min_val, median_val, itr_sec, mem_alloc, gc_sec))
}
cat("```\n\n")

cat("Speedup: ", sprintf("%.1fx", results$`itr/sec`[2] / results$`itr/sec`[1]), "\n")
cat("Memory reduction: ",
    sprintf("%.1fx", as.numeric(results$mem_alloc[1]) / as.numeric(results$mem_alloc[2])),
    "\n")

# Save results
saveRDS(results, "benchmark_readme_results.rds")
cat("\nResults saved to: benchmark_readme_results.rds\n")
