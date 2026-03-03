# Propensity Score Matching

Matches treated and control units based on estimated propensity scores.
Fits a logistic regression model (or accepts a pre-fitted one), computes
logit propensity scores, and calls
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
with a caliper on the logit scale.

## Usage

``` r
ps_match(
  formula = NULL,
  data = NULL,
  treatment = NULL,
  ps_model = NULL,
  caliper_sd = 0.2,
  method = "auto",
  replace = FALSE,
  ratio = 1L,
  ...
)
```

## Arguments

- formula:

  Formula for propensity score model (treatment ~ covariates). Required
  if `ps_model` is NULL.

- data:

  Combined dataset containing both treated and control units

- treatment:

  Name of the binary treatment column (0/1 or logical)

- ps_model:

  Pre-fitted `glm` object (alternative to formula). If provided,
  `formula` is ignored.

- caliper_sd:

  Caliper width in standard deviations of logit(PS). Default: 0.2
  (Rosenbaum and Rubin recommendation).

- method:

  LAP solver method (default: "auto")

- replace:

  If TRUE, match with replacement (default: FALSE)

- ratio:

  Integer k for k:1 matching (default: 1)

- ...:

  Additional arguments passed to
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)

## Value

A matching_result object with additional propensity score info in
`result$info$ps_model` and `result$info$caliper_value`.

## Details

The propensity score is the probability of treatment assignment
conditional on observed covariates. Matching is performed on the logit
of the propensity score (Rosenbaum and Rubin 1985), which provides
better distributional properties than matching on the raw probability
scale.

The default caliper of 0.2 SD of logit(PS) is recommended by Austin
(2011) as removing approximately 98% of bias.

## Examples

``` r
set.seed(42)
n <- 100
data <- data.frame(
  id = seq_len(n),
  treated = rbinom(n, 1, 0.4),
  age = rnorm(n, 50, 10),
  income = rnorm(n, 50000, 15000)
)
result <- ps_match(treated ~ age + income, data = data, treatment = "treated")
print(result)
```
