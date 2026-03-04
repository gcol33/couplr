# Subclassification on Propensity Score

Divides units into K strata based on quantiles of the propensity score,
then computes within-stratum weights for treatment effect estimation.
This is a simple, transparent approach to propensity score adjustment
that allows visual inspection of balance within each subclass.

## Usage

``` r
subclass_match(
  formula = NULL,
  data = NULL,
  treatment = NULL,
  n_subclasses = 5L,
  ps = NULL,
  ps_model = NULL,
  estimand = "ATT"
)
```

## Arguments

- formula:

  Formula for propensity score model (e.g., `treatment ~ age + income`).
  Ignored if `ps` is provided.

- data:

  Data frame containing all variables

- treatment:

  Character, name of the binary treatment column (0/1)

- n_subclasses:

  Integer, number of subclasses to create (default: 5). Cochran (1968)
  showed that 5 subclasses removes over 90\\ a single covariate.

- ps:

  Optional pre-computed numeric vector of propensity scores (one per row
  in `data`). If NULL, a logistic regression model is fit using
  `formula`.

- ps_model:

  Optional pre-fitted `glm` object for propensity scores

- estimand:

  Target estimand: `"ATT"` (default), `"ATE"`, or `"ATC"`

## Value

An S3 object of class `c("subclass_result", "couplr_result")`
containing:

- matched:

  Tibble with columns `id`, `side`, `subclass`, `ps`, `weight`

- subclass_summary:

  Tibble with per-subclass statistics: counts, mean PS, and overlap
  status

- info:

  List with `n_subclasses`, `estimand`, `n_left`, `n_right`, `method`,
  `vars`

## Details

The algorithm:

1.  Estimate propensity scores via logistic regression (or use
    pre-computed scores)

2.  Divide the propensity score distribution into K quantile-based
    strata

3.  For each stratum, check overlap (both treated and control units
    present)

4.  Compute within-stratum weights based on the target estimand:

    - **ATT**: Treated units get weight 1; control units get weight
      `n_treated_in_stratum / n_control_in_stratum`

    - **ATE**: Both groups get weight proportional to stratum size
      relative to total sample

    - **ATC**: Control units get weight 1; treated units get weight
      `n_control_in_stratum / n_treated_in_stratum`

## Examples

``` r
set.seed(42)
n <- 200
data <- data.frame(
  id = 1:n,
  age = rnorm(n, 40, 10),
  income = rnorm(n, 50000, 15000)
)
data$treatment <- rbinom(n, 1, plogis(-2 + 0.05 * data$age))
result <- subclass_match(treatment ~ age + income, data, treatment = "treatment")
print(result)
```
