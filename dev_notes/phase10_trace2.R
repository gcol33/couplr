suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

# Try seed 13 (n=5) — smallest that fuzz flagged.
set.seed(13L)
n <- sample(3:200, 1L)
m <- n
max_cost <- sample(c(1L, 10L, 100L, 1e3L, 1e5L, 1e7L, 1e9L), 1L)
cost <- matrix(sample.int(max_cost, n * m, replace = TRUE), nrow = n, ncol = m)

cat(sprintf("Instance: n=%d max_cost=%d\n", n, max_cost))
cat("\nCost:\n"); print(cost)
cat("\nRunning GT (any drop will print + abort):\n")
gt <- tryCatch(couplr:::lap_solve_gabow_tarjan(cost, maximize = FALSE),
               error = function(e) conditionMessage(e))
cat("\nGT message: "); cat(as.character(gt), "\n")
