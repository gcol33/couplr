# Setup Required to Complete Step 1

## Issue: Missing Rcpp Exports

The Step 1 implementation is complete, but the package needs to be rebuilt to register the C++ functions.

## What's Missing

The `greedy_matching()` function and its variants are implemented in C++ at:
- `src/solvers/greedy_matching.cpp`

However, they are **not yet registered** in `R/RcppExports.R`, which is causing test failures.

## Solution

Run the following R commands to regenerate the Rcpp exports:

```r
# From the package root directory
setwd("C:/Users/Gilles Colling/Documents/dev/couplr")

# Regenerate Rcpp exports
Rcpp::compileAttributes()

# Rebuild documentation
devtools::document()

# Rebuild and install the package
devtools::install()

# Run tests
devtools::test()
```

## Expected Result

After running `Rcpp::compileAttributes()`, the file `R/RcppExports.R` should contain:

```r
greedy_matching_sorted <- function(cost_matrix, maximize = FALSE) {
    .Call(`_couplr_greedy_matching_sorted`, cost_matrix, maximize)
}

greedy_matching_row_best <- function(cost_matrix, maximize = FALSE) {
    .Call(`_couplr_greedy_matching_row_best`, cost_matrix, maximize)
}

greedy_matching_pq <- function(cost_matrix, maximize = FALSE) {
    .Call(`_couplr_greedy_matching_pq`, cost_matrix, maximize)
}

greedy_matching <- function(cost_matrix, maximize = FALSE, strategy = "row_best") {
    .Call(`_couplr_greedy_matching`, cost_matrix, maximize, strategy)
}
```

## Test Failures Before Setup

Currently failing tests (9 failures):
- `greedy_couples works with simple input`
- `greedy_couples respects constraints`
- `greedy_couples works with blocking`
- `greedy strategies produce valid matchings`
- `greedy is faster than optimal (smoke test)`
- `greedy_couples with auto_scale works correctly`
- Plus 3 more related to greedy matching

Error message:
```
Error in greedy_matching(cost_matrix, maximize = FALSE, strategy = strategy):
  could not find function "greedy_matching"
```

## After Setup

All tests should pass (50 total tests including the 10 new preprocessing tests).

## Why This Happened

The C++ code was already implemented in the package, but:
1. The package hasn't been fully rebuilt since the greedy matching was added
2. `Rcpp::compileAttributes()` needs to be run to generate R bindings
3. This is a standard step in Rcpp package development

## Quick Fix Command

If you just want to fix it quickly:

```r
Rcpp::compileAttributes("C:/Users/Gilles Colling/Documents/dev/couplr")
```

This will regenerate `R/RcppExports.R` and `src/RcppExports.cpp` with all the needed bindings.
