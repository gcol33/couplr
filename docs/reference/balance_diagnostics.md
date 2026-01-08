# Balance Diagnostics for Matched Pairs

Computes comprehensive balance statistics comparing the distribution of
matching variables between left and right units in the matched sample.

## Usage

``` r
balance_diagnostics(
  result,
  left,
  right,
  vars = NULL,
  left_id = "id",
  right_id = "id"
)
```

## Arguments

- result:

  A matching result object from
  [`match_couples()`](https://gcol33.github.io/couplr/reference/match_couples.md)
  or
  [`greedy_couples()`](https://gcol33.github.io/couplr/reference/greedy_couples.md)

- left:

  Data frame of left units

- right:

  Data frame of right units

- vars:

  Character vector of variable names to check balance for. Defaults to
  the variables used in matching (if available in result).

- left_id:

  Character, name of ID column in left data (default: "id")

- right_id:

  Character, name of ID column in right data (default: "id")

## Value

An S3 object of class `balance_diagnostics` containing:

- var_stats:

  Tibble with per-variable balance statistics

- overall:

  List with overall balance metrics

- pairs:

  Tibble of matched pairs with variables

- n_matched:

  Number of matched pairs

- n_unmatched_left:

  Number of unmatched left units

- n_unmatched_right:

  Number of unmatched right units

- method:

  Matching method used

- has_blocks:

  Whether blocking was used

- block_stats:

  Per-block statistics (if blocking used)

## Details

This function computes several balance metrics:

Standardized Difference: The difference in means divided by the pooled
standard deviation. Values less than 0.1 indicate excellent balance,
0.1-0.25 good balance.

Variance Ratio: The ratio of standard deviations (left/right). Values
close to 1 are ideal.

KS Statistic: Kolmogorov-Smirnov test statistic comparing distributions.
Lower values indicate more similar distributions.

Overall Metrics include mean absolute standardized difference across all
variables, proportion of variables with large imbalance (\|std diff\| \>
0.25), and maximum standardized difference.

## Examples

``` r
# Create sample data
set.seed(123)
left <- data.frame(
  id = 1:10,
  age = rnorm(10, 45, 10),
  income = rnorm(10, 50000, 15000)
)
right <- data.frame(
  id = 11:30,
  age = rnorm(20, 47, 10),
  income = rnorm(20, 52000, 15000)
)

# Match
result <- match_couples(left, right, vars = c("age", "income"))

# Get balance diagnostics
balance <- balance_diagnostics(result, left, right, vars = c("age", "income"))
print(balance)
#> 
#> Balance Diagnostics for Matched Pairs
#> ======================================
#> 
#> Matching Summary:
#>   Method: lap
#>   Matched pairs: 10
#>   Unmatched left: 0 (of 10)
#>   Unmatched right: 10 (of 20)
#> 
#> Variable-level Balance:
#> # A tibble: 2 × 7
#>   Variable `Mean Left` `Mean Right` `Mean Diff` `Std Diff` `Var Ratio` `KS Stat`
#>   <chr>          <dbl>        <dbl>       <dbl>      <dbl>       <dbl>     <dbl>
#> 1 age             45.7         46.1       -0.34     -0.044        1.72       0.3
#> 2 income       53129.       53745.      -616.       -0.042        1.16       0.2
#> 
#> Overall Balance:
#>   Mean |Std Diff|: 0.043 (Excellent)
#>   Max |Std Diff|: 0.044
#>   Vars with |Std Diff| > 0.25: 0.0%
#> 
#> Balance Interpretation:
#>   |Std Diff| < 0.10: Excellent balance
#>   |Std Diff| 0.10-0.25: Good balance
#>   |Std Diff| 0.25-0.50: Acceptable balance
#>   |Std Diff| > 0.50: Poor balance
#> 

# Get balance table
balance_table(balance)
#> # A tibble: 2 × 7
#>   Variable `Mean Left` `Mean Right` `Mean Diff` `Std Diff` `Var Ratio` `KS Stat`
#>   <chr>          <dbl>        <dbl>       <dbl>      <dbl>       <dbl>     <dbl>
#> 1 age             45.7         46.1       -0.34     -0.044        1.72       0.3
#> 2 income       53129.       53745.      -616.       -0.042        1.16       0.2
```
