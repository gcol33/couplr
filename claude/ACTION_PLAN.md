# Action Plan to Fix Remaining Test Failures

## Current Status (Verified by User)

```r
ls("package:couplr")[grep("greedy_matching|scale_match_cpp", ls("package:couplr"))]
# [1] "_couplr_scale_match_cpp" "scale_match_cpp"
```

✅ `scale_match_cpp` is exported and available
❌ `greedy_matching` is completely missing from namespace

## Root Causes Identified

### Issue 1: greedy_matching Not in Namespace

**Problem:**
- `greedy_couples()` calls `greedy_matching(cost_matrix, maximize = FALSE, strategy = strategy)` at line 541 of matching_core.R
- Error: `could not find function "greedy_matching"`
- C++ function exists in `src/solvers/greedy_matching.cpp:248` with `[[Rcpp::export]]`
- But NOT in `R/RcppExports.R`

**Why:**
- `Rcpp::compileAttributes()` has never been run for this file
- The function was added after the last `compileAttributes()` run

**Fix:**
Run `Rcpp::compileAttributes()` which will:
1. Scan all C++ files for `[[Rcpp::export]]` tags
2. Generate `R/RcppExports.R` with R wrapper functions
3. Generate `src/RcppExports.cpp` with C registration
4. Add `greedy_matching` to the namespace

### Issue 2: scale_match_cpp Tests Using Wrong Package Name ✅ FIXED

**Problem:**
- `scale_match_cpp` exists in namespace as `couplr:::scale_match_cpp`
- Tests fail with: `object 'scale_match_cpp' not found`

**Root Cause:**
- Tests were calling `lapr:::scale_match_cpp(...)` - referring to OLD package name
- Package was renamed from `lapr` to `couplr`
- All 21 test calls needed to be updated to `couplr:::scale_match_cpp(...)`

**Fix Applied:**
✅ **FIXED** - Replaced all 21 occurrences of `lapr:::` with `couplr:::` in test_gabow_tarjan_moduleG.R

```bash
# Command used:
sed -i 's/lapr:::/couplr:::/g' tests/testthat/test_gabow_tarjan_moduleG.R
```

**Verification:**
- Before: 21 occurrences of `lapr:::`
- After: 21 occurrences of `couplr:::`, 0 occurrences of `lapr:::`

**Example change:**
```r
# Before:
result <- lapr:::scale_match_cpp(cost)

# After:
result <- couplr:::scale_match_cpp(cost)
```

## Step-by-Step Fix

### Step 1: Manual Export Registration ✅ DONE

**Status:** Manually added `greedy_matching()` exports to RcppExports files since `Rcpp::compileAttributes()` was not accessible from the environment.

**Files modified:**
- ✅ `R/RcppExports.R` - Added R wrapper function
- ✅ `src/RcppExports.cpp` - Added C++ export wrapper and registration entry

**What was done:**
- Added `greedy_matching()` to `R/RcppExports.R`
- Added C++ wrapper `_couplr_greedy_matching()` to `src/RcppExports.cpp`
- Added registration entry to CallEntries array
- Makes greedy_matching callable from R after rebuild

### Step 2: Rebuild Package

```r
devtools::clean_dll()
devtools::load_all()
```

**What this does:**
- Cleans old compiled code
- Recompiles with new exports
- Loads fresh package with all functions

### Step 3: Verify Exports

```r
# Check that greedy_matching is now available:
ls("package:couplr")[grep("greedy", ls("package:couplr"))]
# Should show:
# [1] "_couplr_greedy_matching"           "greedy_matching"
# [2] "_couplr_greedy_matching_pq"        "greedy_matching_pq"
# [3] "_couplr_greedy_matching_row_best"  "greedy_matching_row_best"
# [4] "_couplr_greedy_matching_sorted"    "greedy_matching_sorted"
```

### Step 4: Fix scale_match_cpp Test Access ✅ DONE

✅ **Already fixed!** All test references updated from `lapr:::` to `couplr:::`

### Step 5: Run Tests

```r
devtools::test()
```

## Expected Results After Fix

### Before Fixes:
- ❌ 5 tests failing in `test-matching.R` (greedy_matching not found)
- ❌ ~16 tests failing in `test_gabow_tarjan_moduleG.R` (wrong package name `lapr:::`)

### After Fixes Applied:
- ✅ scale_match_cpp tests fixed - package name updated to `couplr:::`
- ⏳ greedy_matching tests - pending `Rcpp::compileAttributes()`

### After Running Rcpp::compileAttributes():
- ✅ All greedy_couples tests pass
- ✅ All Gabow-Tarjan module G tests pass
- ✅ 100% test pass rate

## Quick Commands (Copy-Paste)

```r
# Complete fix in one go:
setwd("C:/Users/Gilles Colling/Documents/dev/couplr")
Rcpp::compileAttributes()
devtools::clean_dll()
devtools::load_all()

# Verify greedy_matching is now available:
"greedy_matching" %in% ls("package:couplr")  # Should be TRUE

# Run tests:
devtools::test()
```

## Fixes Already Applied ✅

1. ✅ **scale_match_cpp package name** - Fixed all 21 test references from `lapr:::` to `couplr:::`
2. ✅ **test-assignment-ssap-bucket.R** - Fixed all calls to use `assignment(..., method = "ssap_bucket")`
3. ✅ **test-cycle-cancel.R** - Fixed all calls to use `assignment(..., method = "cycle_cancel")`
4. ✅ **Empty matrix validation** - Added to `assignment()` function
5. ✅ **Class name expectations** - Updated from `"lap_result"` to `"lap_solve_result"`

## Summary

The issue is **NOT** missing C++ code - both functions exist and are properly marked for export.

The issue is **missing R bindings** - `Rcpp::compileAttributes()` hasn't been run since these functions were added.

**One command fixes 95% of the issues:**
```r
Rcpp::compileAttributes()
```
