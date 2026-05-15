suppressPackageStartupMessages({ library(devtools) })
devtools::load_all(".", quiet = TRUE)
set.seed(100)
n <- 10
cost <- matrix(sample(1:1000, n * n, replace = TRUE), n, n)
res <- tryCatch(
  lap_solve(cost, method = "gabow_tarjan"),
  error = function(e) {
    cat("ERR: ", conditionMessage(e), "\n", sep = "")
    NULL
  }
)
cat("DONE\n")
