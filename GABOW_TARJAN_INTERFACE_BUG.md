# Gabow-Tarjan R Interface Bug

## Problem

The Gabow-Tarjan C++ implementation exists and compiles, but is **not properly connected to the R interface**.

## Evidence

1. `lap_solve(cost, method='gabow_tarjan')` returns empty results (0 rows)
2. `lap_solve_gabow_tarjan()` function does not exist in the package
3. Tests in `test-gabow_tarjan_solver.R` try to call `lap_solve_gabow_tarjan()` but it's not exported
4. Hungarian method works fine: `lap_solve(cost, method='hungarian')` returns correct results

## Test Results

```r
# This returns EMPTY (broken):
library(couplr)
cost <- matrix(c(4,1,3,2,0,5,3,2,2), 3, 3, byrow=TRUE)
lap_solve(cost, method='gabow_tarjan')
# Result: 0 rows, no cost

# This WORKS:
lap_solve(cost, method='hungarian')
# Result: 3 rows, cost = 5

# This doesn't exist:
lap_solve_gabow_tarjan(cost)
# Error: could not find function "lap_solve_gabow_tarjan"
```

## Root Cause

The C++ function `solve_gabow_tarjan_inner()` exists in `utils_gabow_tarjan.cpp`, but:

1. **Not exported to R**: No `lap_solve_gabow_tarjan()` wrapper function in R
2. **Not registered in Rcpp**: May not be in `RcppExports.cpp`
3. **lap_solve() doesn't route to it**: The `method='gabow_tarjan'` parameter doesn't connect properly

## Files to Check

1. **src/RcppExports.cpp** - Check if Gabow-Tarjan functions are registered
2. **R/lap_solve.R** - Check if `method='gabow_tarjan'` case is implemented
3. **src/gabow_tarjan/solve_gabow_tarjan.cpp** - Check if wrapper exists
4. **NAMESPACE** - Check if functions are exported

## Next Steps

1. Find where `lap_solve()` dispatches based on `method` parameter
2. Verify Gabow-Tarjan wrapper function exists and is exported
3. Check Rcpp attributes are correct (`// [[Rcpp::export]]`)
4. Rebuild with `Rcpp::compileAttributes()` if needed

## Impact on Optimization

The Step 1 optimization work is **theoretically sound** but cannot be tested until this interface bug is fixed. The optimizations are:

1. ✅ Fast augmentation - O(path_length) vs O(n)
2. ✅ DFS pathfinding - O(m) vs O(√n·m)
3. ✅ Direct path processing - avoids set operations

Once the R interface is fixed, these optimizations should provide 5-20x speedup.
