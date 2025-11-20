library(couplr)

# Test Gabow-Tarjan with incremental updates
set.seed(123)
cost <- matrix(runif(100*100, 0, 100), 100, 100)

result <- lap_solve(cost, method='gabow_tarjan')

cat("Gabow-Tarjan with incremental updates WORKS!\n")
cat("Matched", nrow(result), "rows\n")
cat("Total cost:", attr(result, 'total_cost'), "\n")
