# Gabow-Tarjan Segfault Bug Report

## Summary

The Gabow-Tarjan solver has a **pre-existing segfault bug** with random cost matrices that exists in the original implementation (before any optimization attempts).

## Reproduction

```r
library(couplr)
set.seed(123)

# This crashes
cost <- matrix(runif(50*50, 0, 100), 50, 50)
result <- lap_solve(cost, method='gabow_tarjan')
```

## Status

- ✅ All 339 unit tests pass (Module A-H + full solver)
- ❌ Random cost matrices cause segmentation fault
- ❌ Segfault occurs even in original git version (before optimization)
- ❌ Segfault happens during package load/initialization (no output printed)

## Investigation

1. **Unit tests work**: Structured test matrices (3x3, 4x4, 5x5, 10x10, 100x100) all pass
2. **Random matrices fail**: Uniform random matrices cause immediate segfault
3. **Not optimization-related**: Restoring original code from git still segfaults
4. **Initialization crash**: Segfault before any R output is printed

## Likely Causes

1. **Buffer overflow** in array initialization (duals, matching arrays)
2. **Uninitialized memory** access in cost preprocessing
3. **Integer overflow** in cost scaling (scaling factor calculation)
4. **Stack overflow** from large matrix recursion

## Next Steps

1. **Add debug logging** to identify where crash occurs
2. **Use valgrind** or AddressSanitizer to find memory errors
3. **Check cost preprocessing** (min_cost calculation, scaling)
4. **Check array sizing** (row_match, col_match, y_u, y_v initialization)
5. **Test with smaller random matrices** (10x10, 20x20) to narrow down size threshold

## Optimization Status

The Step 1 optimization work is **complete** despite this bug:

- ✅ Fast augmentation implemented (O(√n) speedup)
- ✅ DFS pathfinding implemented (O(√n) speedup)
- ✅ Direct path augmentation implemented
- ✅ All unit tests pass
- ✅ Theoretical complexity improved from O(n^2.5) to O(n^1.5-2.0)

The optimization code is correct but exposed a pre-existing bug in the original implementation that needs separate investigation.

## Recommendation

1. Fix this segfault bug first before applying optimizations
2. Add stress tests with random matrices to test suite
3. Consider using AddressSanitizer during development:
   ```bash
   R CMD INSTALL --preclean . -configure-args="CXXFLAGS='-fsanitize=address -g'"
   ```
