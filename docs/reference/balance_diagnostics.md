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
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  or
  [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)

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

# Get balance table
balance_table(balance)
```
