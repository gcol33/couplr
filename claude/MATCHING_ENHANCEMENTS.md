# Matching Layer Enhancement Plan

## Overview

This document outlines enhancements to make the matching layer more robust, user-friendly, and production-ready. These features address common pitfalls in matching workflows and reduce manual preprocessing burden.

---

## 1. Automatic Scaling and Preprocessing

### Current State
- ✅ Basic scaling implemented (`scale = "standardize"` or `"range"`)
- ✅ Manual weight specification supported
- ❌ No automatic variable preprocessing
- ❌ No detection of problematic variables

### Proposed Enhancement

**Add `auto_scale` parameter with smart preprocessing:**

```r
match_couples(
  left, right,
  vars = c("age", "income", "bp_systolic"),
  auto_scale = TRUE,  # New parameter
  scale_method = "auto"  # "auto", "standardize", "range", or FALSE
)
```

**Features:**
1. **Variable health checks:**
   - Detect constant columns (SD = 0) → exclude with warning
   - Detect nearly-constant (SD < 1e-6) → warn
   - Detect all-NA columns → exclude with warning
   - Detect high missingness (>50%) → warn

2. **Automatic scaling selection:**
   - For mixed variables: Use MAD (median absolute deviation) for robust scaling
   - For skewed distributions: Suggest log-transform or rank-based distance
   - For count data: Suggest Poisson or negative binomial distance

3. **Categorical variable handling:**
   - Binary (0/1): Use as-is or treat as factor
   - Ordered factors: Use ordinal distance
   - Unordered factors: One-hot encode or use Gower distance

**Implementation:**
- New file: `R/matching_preprocessing.R`
- Functions:
  - `check_variable_health()`
  - `suggest_scaling()`
  - `auto_encode_categorical()`
  - `preprocess_matching_vars()`

---

## 2. Balance Diagnostics

### Current State
- ✅ Basic info returned (n_matched, total_distance)
- ❌ No variable-level balance statistics
- ❌ No standardized difference metrics

### Proposed Enhancement

**Add balance diagnostic functions:**

```r
# After matching
result <- match_couples(left, right, vars = c("age", "income", "education"))

# Get balance diagnostics
balance <- balance_diagnostics(result, left, right, vars)
print(balance)
```

**Output:**
```
Balance Diagnostics for Matched Pairs
======================================

Variable-level summaries:
                  Mean Diff  Std Diff  Max Diff  % >0.5 SD
age                   1.23      0.15      5.67       2.3%
income              234.50      0.08    1200.00      0.8%
education             0.45      0.12      2.00       1.5%

Overall:
- Mean standardized difference: 0.12 (good: <0.25)
- Pairs with any large difference: 3.1%
- Unmatched left: 5 units
- Unmatched right: 12 units

Block summaries:
Block   N Pairs  Mean Dist  Balance Quality
A           45       2.34    Excellent
B           38       3.12    Good
C           22       5.67    Fair (check age)
```

**Features:**
1. **Standardized differences:** (mean_left - mean_right) / pooled_SD
2. **Distribution checks:** KS test for continuous, chi-square for categorical
3. **Propensity score overlap:** If provided
4. **Per-block diagnostics:** If blocking used
5. **Visual diagnostics:** Love plot function

**Implementation:**
- New file: `R/matching_diagnostics.R`
- Functions:
  - `balance_diagnostics()`
  - `standardized_difference()`
  - `plot_balance()` (Love plot)
  - `balance_table()`

---

## 3. Joined Matched Dataset Output

### Current State
- ✅ Returns pairs tibble with IDs
- ❌ User must manually join variables
- ❌ No ready-to-analyze format

### Proposed Enhancement

**Add `join_matched()` function:**

```r
result <- match_couples(left, right, vars = c("x", "y"))

# Get joined dataset
matched_data <- join_matched(
  result,
  left,
  right,
  left_vars = c("treatment", "age", "income"),
  right_vars = c("age", "income"),
  suffix = c("_treated", "_control")
)

head(matched_data)
```

**Output:**
```
# A tibble: 100 × 9
   pair_id left_id right_id distance age_treated age_control income_treated income_control treatment
     <int>   <int>    <int>    <dbl>       <dbl>       <dbl>          <dbl>          <dbl>     <int>
 1       1       1      105     0.45          25          24          45000          46000         1
 2       2       2      132     0.67          30          29          52000          51500         1
 ...
```

**Features:**
1. **Automatic variable joining** with clear suffixes
2. **Include matching metadata:** distance, block_id, pair_id
3. **Long format option:** For difference analysis
4. **Summary statistics:** Built-in difference calculations

**Alternative: `augment()` method (broom-style):**
```r
augmented <- augment(result, left, right)
```

**Implementation:**
- Add to `R/matching_core.R`
- Functions:
  - `join_matched()`
  - `augment.matching_result()`
  - Helper: `prepare_matched_data()`

---

## 4. Precomputed and Reusable Distances

### Current State
- ❌ Distances recomputed on every matching call
- ❌ No caching mechanism

### Proposed Enhancement

**Add distance object workflow:**

```r
# Step 1: Compute distances once
dist_obj <- compute_distances(
  left, right,
  vars = c("age", "income", "education"),
  distance = "euclidean",
  scale = "standardize"
)

# Step 2: Reuse for different matching strategies
result1 <- match_couples(dist_obj, max_distance = 5)
result2 <- match_couples(dist_obj, max_distance = 10)
result3 <- greedy_couples(dist_obj, strategy = "sorted")

# Update constraints without recomputing
dist_obj_constrained <- apply_calipers(dist_obj, calipers = list(age = 5))
result4 <- match_couples(dist_obj_constrained)
```

**Features:**
1. **Distance matrix caching** with metadata
2. **Lazy evaluation** for large datasets
3. **Constraint modification** without recomputation
4. **Memory-efficient storage** (sparse matrices for many forbidden pairs)

**Implementation:**
- New file: `R/matching_distance_cache.R`
- S3 class: `distance_object`
- Functions:
  - `compute_distances()` - Create distance object
  - `update_constraints()` - Modify constraints
  - `is_distance_object()` - Type check
- Modify `match_couples()` and `greedy_couples()` to accept distance objects

---

## 5. Near-Exact Matching for Categorical Variables

### Current State
- ✅ Exact blocking supported via `matchmaker()`
- ❌ No fuzzy categorical matching
- ❌ No hierarchical category matching

### Proposed Enhancement

**Add `near_exact` parameter:**

```r
match_couples(
  left, right,
  vars = c("age", "income"),
  near_exact = list(
    education = "ordered",      # Allow ±1 level
    region = c("A" = "B"),      # A can match B
    occupation = hierarchy      # Use tree structure
  )
)
```

**Features:**

1. **Ordered categories:** Allow matching within k levels
```r
near_exact = list(
  education = list(type = "ordered", max_diff = 1)
)
# "High School" can match "Some College" but not "Graduate Degree"
```

2. **Category groups:** Define equivalence classes
```r
near_exact = list(
  region = list(
    "Northeast" = c("Northeast", "Mid-Atlantic"),
    "South" = c("South", "Southeast")
  )
)
```

3. **Hierarchical categories:** Use tree structure
```r
occupation_tree <- list(
  "Healthcare" = c("Doctor", "Nurse", "Technician"),
  "Education" = c("Teacher", "Professor", "Administrator")
)
near_exact = list(occupation = occupation_tree)
```

**Implementation:**
- New file: `R/matching_categorical.R`
- Functions:
  - `define_near_exact()`
  - `categorical_distance()`
  - `check_category_compatibility()`
- Integrate with `apply_constraints()`

---

## 6. Warnings for Degenerate or Extreme Costs

### Current State
- ❌ No cost distribution checks
- ❌ Silent failures on problematic inputs

### Proposed Enhancement

**Add automatic diagnostics before solving:**

```r
match_couples(left, right, vars = c("x", "y"), check_costs = TRUE)
```

**Warnings generated:**

1. **Too many zero distances:**
```
Warning: 45% of distances are zero (likely duplicates)
  • 23 pairs of (left, right) have identical values
  • Consider adding more distinguishing variables
  • Use `remove_duplicates = TRUE` to handle automatically
```

2. **Extreme cost ratios:**
```
Warning: Cost distribution is highly skewed
  • 95th percentile: 2.5
  • 99th percentile: 450.0 (180x larger!)
  • Variable 'income' may need log-scaling
  • Consider `scale = "rank"` or check for outliers
```

3. **Many forbidden pairs:**
```
Warning: 87% of pairs are forbidden (distance = Inf)
  • Only 156 valid pairs remain for 120 left units
  • Constraints may be too strict
  • Consider relaxing: max_distance or calipers
```

4. **Constant distance:**
```
Warning: All distances are identical (3.45)
  • Matching variables are not informative
  • Check for constant or highly correlated variables
```

**Features:**
- **Pre-solve checks** with actionable messages
- **Severity levels:** Info, Warning, Error
- **Automatic fixes:** Optional `auto_fix = TRUE`
- **Diagnostics plot:** Show cost distribution

**Implementation:**
- New file: `R/matching_checks.R`
- Functions:
  - `check_cost_distribution()`
  - `diagnose_distance_matrix()`
  - `suggest_fixes()`
- Add to `match_couples()` and `greedy_couples()` at start

---

## Implementation Priority

### Phase 1 (Essential - Complete before CRAN)
1. ✅ **Balance diagnostics** - Most requested by users
2. ✅ **Joined matched dataset** - Reduces friction significantly
3. ✅ **Cost distribution warnings** - Prevents common errors

### Phase 2 (Important - Next release)
4. **Precomputed distances** - Performance improvement
5. **Automatic preprocessing** - Better defaults

### Phase 3 (Advanced - Future)
6. **Near-exact categorical matching** - Specialized use case

---

## Testing Strategy

For each enhancement:

1. **Unit tests:**
   - Edge cases (empty, single row, all NA)
   - Different variable types
   - With and without blocking

2. **Integration tests:**
   - Full workflows with real-ish data
   - Compatibility with existing functions

3. **Documentation:**
   - Clear examples in function docs
   - Vignette for each major feature
   - README updates

---

## API Compatibility

All enhancements maintain backward compatibility:
- New parameters have sensible defaults
- Existing workflows continue to work unchanged
- New functions don't shadow existing ones
- Old return structures extended, not replaced

---

## Example: Complete Workflow with Enhancements

```r
library(couplr)

# 1. Prepare data with automatic preprocessing
left <- data.frame(
  id = 1:100,
  treatment = 1,
  age = rnorm(100, 50, 10),
  income = rlnorm(100, 10, 1),
  education = sample(c("HS", "College", "Grad"), 100, replace = TRUE)
)

right <- data.frame(
  id = 101:250,
  treatment = 0,
  age = rnorm(150, 52, 10),
  income = rlnorm(150, 10, 1),
  education = sample(c("HS", "College", "Grad"), 150, replace = TRUE)
)

# 2. Create blocks with near-exact matching
blocks <- matchmaker(
  left, right,
  block_type = "group",
  block_by = "education",
  near_exact = list(education = "ordered")
)

# 3. Compute distances once
dist_obj <- compute_distances(
  blocks$left, blocks$right,
  vars = c("age", "income"),
  auto_scale = TRUE,  # Automatic preprocessing
  check_costs = TRUE  # Get warnings
)

# 4. Try different matching strategies
result_opt <- match_couples(dist_obj, max_distance = 0.5)
result_greedy <- greedy_couples(dist_obj, strategy = "sorted")

# 5. Get balance diagnostics
balance <- balance_diagnostics(result_opt, left, right,
                              vars = c("age", "income"))
print(balance)
plot_balance(balance)  # Love plot

# 6. Create analysis-ready dataset
matched_data <- join_matched(
  result_opt,
  left, right,
  left_vars = c("treatment", "age", "income"),
  right_vars = c("age", "income"),
  suffix = c("_treated", "_control")
)

# 7. Analyze
lm(outcome ~ treatment + age_treated + income_treated, data = matched_data)
```

---

## Summary

These enhancements transform the matching layer from a basic implementation into a production-grade tool that:
- **Reduces errors** through automatic checks and preprocessing
- **Saves time** by eliminating manual data wrangling
- **Improves transparency** with clear diagnostics and warnings
- **Enhances performance** through distance caching
- **Increases flexibility** with advanced categorical matching

All while maintaining full backward compatibility with existing code.
