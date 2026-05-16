suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

set.seed(42L)
n <- 50L
cost <- matrix(sample.int(1e9L, n * n, replace = TRUE), nrow = n, ncol = n)
cat(sprintf("n=%d max_cost=%g\n", n, max(cost)))
gt <- couplr:::lap_solve_gabow_tarjan(cost, maximize = FALSE)
cat(sprintf("GT cost: %s\n", format(gt$total_cost)))
