# Compute Rosenbaum bounds via normal approximation

Under hidden bias Gamma, each pair's probability of the treated unit
having the larger value is between 1/(1+Gamma) and Gamma/(1+Gamma).

## Usage

``` r
.rosenbaum_bounds(t_obs, ranks, n, gamma, alternative)
```

## Value

List with p_upper and p_lower.
