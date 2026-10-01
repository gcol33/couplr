# Report for a pruned match

The fields a `cardinality_report` carries that the prune loop computes,
and `NA` for the ones it does not. The loop maximizes nothing and
derives no bound, so `best_possible` and `gap` have no value to hold;
`NA` is what they hold, rather than a number that would read as a bound.

## Usage

``` r
.cardinality_heuristic_report(pairs, max_std_diff)

# S3 method for class 'cardinality_heuristic'
print(x, ...)
```

## Arguments

- pairs:

  The matched pairs the loop settled on.

- max_std_diff:

  The threshold it pruned toward.

- x:

  A `cardinality_heuristic` report.

- ...:

  Ignored.

## Value

An object of class `c("cardinality_heuristic", "cardinality_report")`.

Invisibly returns `x`.
