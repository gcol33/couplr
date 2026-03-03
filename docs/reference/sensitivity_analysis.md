# Rosenbaum Sensitivity Analysis

Assesses how sensitive a matched comparison is to hidden bias using
Rosenbaum bounds on the Wilcoxon signed-rank statistic.

## Usage

``` r
sensitivity_analysis(
  result,
  left,
  right,
  outcome_var,
  gamma = seq(1, 3, by = 0.25),
  alternative = c("greater", "less", "two.sided"),
  left_id = "id",
  right_id = "id"
)
```

## Arguments

- result:

  A matching_result object from
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  or
  [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)

- left:

  Original left (treated) dataset

- right:

  Original right (control) dataset

- outcome_var:

  Name of the outcome column in `left` and `right`

- gamma:

  Numeric vector of sensitivity parameters (default: seq(1, 3, by =
  0.25)). Gamma = 1 means no hidden bias.

- alternative:

  Direction of the test: "greater" (default), "less", or "two.sided"

- left_id:

  Name of ID column in left (default: "id")

- right_id:

  Name of ID column in right (default: "id")

## Value

An S3 object of class `sensitivity_analysis` containing:

- results:

  Tibble with columns: gamma, t_stat, p_upper, p_lower

- n_pairs:

  Number of matched pairs analyzed

- critical_gamma:

  Smallest gamma at which p_upper \> 0.05

- alternative:

  Direction of test

## Details

Rosenbaum (2002, Chapter 4) bounds quantify how much hidden bias (an
unobserved confounder) would be needed to explain away the observed
treatment effect. The sensitivity parameter Gamma represents the maximum
ratio of treatment odds between two matched units:

- Gamma = 1: No hidden bias (standard Wilcoxon test)

- Gamma = 2: One unit could be twice as likely to receive treatment due
  to an unobserved factor

The function computes upper and lower bounds on the p-value of the
Wilcoxon signed-rank test under each level of hidden bias. A finding is
"insensitive to bias" if p_upper remains below 0.05 even at large Gamma.

## References

Rosenbaum, P.R. (2002). Observational Studies, 2nd edition. Springer.

## Examples

``` r
set.seed(42)
left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, 1, 1))
right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
result <- match_couples(left, right, vars = "x")
sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome")
print(sens)
```
