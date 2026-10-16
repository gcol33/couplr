# Matching Results

Every matching function returns an object inheriting from class
`"couplr_result"`:

|  |  |  |
|----|----|----|
| Function | Class | Matched units |
| [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md), [`cardinality_match()`](https://gillescolling.com/couplr/reference/cardinality_match.md) | `"matching_result"` | pairs |
| [`full_match()`](https://gillescolling.com/couplr/reference/full_match.md) | `"full_matching_result"` | groups |
| [`cem_match()`](https://gillescolling.com/couplr/reference/cem_match.md) | `"cem_result"` | strata |
| [`subclass_match()`](https://gillescolling.com/couplr/reference/subclass_match.md) | `"subclass_result"` | subclasses |

[`print()`](https://rdrr.io/r/base/print.html) gives a short overview
and [`summary()`](https://rdrr.io/r/base/summary.html) the full report:
the distribution of pair distances, the certificate conditions, the
column generation rounds and the balance constraints of a cardinality
matching, where present.
[`glance()`](https://generics.r-lib.org/reference/glance.html) returns
the headline numbers as a one-row tibble and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) (or
[`tidy()`](https://generics.r-lib.org/reference/tidy.html)) the matched
units, one row per pair or per unit.
[`certificate()`](https://gillescolling.com/couplr/reference/result_accessors.md),
[`potentials()`](https://gillescolling.com/couplr/reference/result_accessors.md)
and
[`unmatched_units()`](https://gillescolling.com/couplr/reference/result_accessors.md)
read the certificate, the dual potentials and the unmatched units.
Further methods take any of these results:
[`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md),
[`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md),
[`match_data()`](https://gillescolling.com/couplr/reference/match_data.md),
[`as_matchit()`](https://gillescolling.com/couplr/reference/as_matchit.md)
and
[`cobalt::bal.tab()`](https://ngreifer.github.io/cobalt/reference/bal.tab.html).

## Usage

``` r
# S3 method for class 'matching_result'
plot(x, type = c("histogram", "density", "ecdf"), ...)

# S3 method for class 'matching_result'
autoplot(object, type = c("histogram", "density", "ecdf"), ...)

# S3 method for class 'couplr_result'
glance(x, ...)

# S3 method for class 'couplr_result'
tidy(x, ...)

# S3 method for class 'couplr_result'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)

# S3 method for class 'couplr_result'
print(x, ...)

# S3 method for class 'couplr_result'
summary(object, ...)
```

## Arguments

- x, object:

  A matching result.

- type:

  Type of plot: "histogram" (default), "density", or "ecdf"

- ...:

  Ignored.

- row.names, optional:

  Ignored; present for consistency with the generic.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.
[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `"summary.couplr_result"`.
[`glance()`](https://generics.r-lib.org/reference/glance.html) returns a
one-row tibble with columns `design`, `n_left`, `n_right`,
`n_matched_left`, `n_matched_right`, `n_pairs`, `n_groups`,
`total_distance`, `status`, `certified` and `max_suboptimality`, plus
`best_possible`, `gap`, `n_nodes`, `stopped_on`, `size_certified`
(whether the largest size the constraints admit was proven, which
`certified` also requires of the balance and distance stages) and
`size_seconds` (how long proving it took, `NA` when one network solve
answered the call) for a cardinality matching and `n_rounds`,
`candidate_edges` and `possible_edges` for column generation.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) and
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) return the
matched units.

## Examples

``` r
set.seed(1)
left <- data.frame(id = 1:5, x = rnorm(5))
right <- data.frame(id = 6:15, x = rnorm(10))
m <- match_couples(left, right, vars = "x", certify = TRUE)
m
glance(m)
as.data.frame(m)
certificate(m)
summary(m)
```
