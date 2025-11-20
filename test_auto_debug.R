library(couplr)

cat("Debugging auto-selection:\n\n")

# Test n=10
set.seed(123)
cost10 <- matrix(runif(100, 0, 100), 10, 10)
cat("Cost matrix shape: ", nrow(cost10), "x", ncol(cost10), "\n")
cat("n <= 10 && m <= 10:", nrow(cost10) <= 10 && ncol(cost10) <= 10, "\n")

result10 <- lap_solve(cost10, method="auto")
cat("Method used:", attr(result10, "method_used"), "\n\n")

# Test n=60
cost60 <- matrix(runif(3600, 0, 100), 60, 60)
cat("Cost matrix shape: ", nrow(cost60), "x", ncol(cost60), "\n")
cat("n <= 50:", nrow(cost60) <= 50, "\n")
cat("n <= 75:", nrow(cost60) <= 75, "\n")

result60 <- lap_solve(cost60, method="auto")
cat("Method used:", attr(result60, "method_used"), "\n\n")

# Test n=100
cost100 <- matrix(runif(10000, 0, 100), 100, 100)
cat("Cost matrix shape: ", nrow(cost100), "x", ncol(cost100), "\n")
cat("n <= 75:", nrow(cost100) <= 75, "\n")
cat("n > 75:", nrow(cost100) > 75, "\n")

result100 <- lap_solve(cost100, method="auto")
cat("Method used:", attr(result100, "method_used"), "\n")
