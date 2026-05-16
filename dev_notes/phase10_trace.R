suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

# Try a few seeds known to violate.
set.seed(20L)
n <- sample(3:200, 1L)
m <- n
max_cost <- sample(c(1L, 10L, 100L, 1e3L, 1e5L, 1e7L, 1e9L), 1L)
cost <- matrix(sample.int(max_cost, n * m, replace = TRUE), nrow = n, ncol = m)

cat(sprintf("Instance: n=%d max_cost=%d\n", n, max_cost))
print(cost)

cat("\nJV result:\n")
jv <- couplr:::lap_solve_jv(cost, maximize = FALSE)
cat(sprintf("  total_cost=%s\n", format(jv$total_cost)))

cat("\nGT result:\n")
gt <- tryCatch(couplr:::lap_solve_gabow_tarjan(cost, maximize = FALSE),
               error = function(e) e)
if (inherits(gt, "error")) {
  cat(sprintf("  ERROR: %s\n", conditionMessage(gt)))
} else {
  cat(sprintf("  total_cost=%s\n", format(gt$total_cost)))
}
