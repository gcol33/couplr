# Match across a range of one design choice

Solves the matching once for each value of one argument and returns what
happened at each: how many units were matched, at what total distance,
how balanced the matched sample is, and the certificate saying the
matching is optimal for that value. The points are solved as one
sequence rather than one at a time, each starting from the matching the
previous value found.

## Usage

``` r
match_path(
  left,
  right,
  vars,
  left_id = NULL,
  right_id = NULL,
  vary = "max_distance",
  values,
  distance = "euclidean",
  weights = NULL,
  scale = FALSE,
  auto_scale = FALSE,
  calipers = NULL,
  sigma = NULL,
  certify = TRUE,
  keep_per_row = .implicit_defaults()$keep_per_row,
  width = .implicit_defaults()$width,
  tol = .implicit_defaults()$tol,
  max_rounds = .implicit_defaults()$max_rounds
)
```

## Arguments

- left, right:

  Data frames to match. Both are required.

- vars:

  Character vector of matching variables, present in both.

- left_id, right_id:

  Name of the column holding the unit identifier, or NULL (default) to
  use a column called `id`, then meaningful row names, then synthesized
  ids with a warning. See
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md).

- vary:

  The argument to sweep. `"max_distance"` sweeps the distance cut.

- values:

  The values to sweep it through, ascending.

- distance:

  Distance metric, as in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md).

- weights, scale, auto_scale, sigma:

  Distance construction, as in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md).

- calipers:

  Per-variable calipers held fixed across the path, as in
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md).
  Balance is read on the caller's own variables rather than on the
  coordinates the solver worked in, so a scaled or weighted distance
  does not change the scale the balance is reported on. It is the same
  reading
  [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  gives for a single matching, taken at every point of the sweep.

- certify:

  Whether each point carries a checked certificate. `TRUE` by default:
  the certificate is what says a point's matching is the optimal one for
  its value, which is the claim a path is read for.

- keep_per_row, width, tol, max_rounds:

  The edge-generation loop's search knobs, shared with
  `memory_mode = "implicit"`. Each point converges on any of them.
  `width` is the columns the seed gives a row, and 0, the default, sizes
  it from the number of columns; `$search$seed_width` reports what the
  path used.

## Value

An object of class `couplr_path`: `$path`, one row per point,
`$balance`, one row per point per variable, and the match vector,
certificate, round record and Hall witness for each of them.

## Details

`values` must ascend. Each point is solved from the point before it,
which works because a wider value only ever adds pairs to choose from; a
descending sweep takes pairs away from a matching already using them,
and is refused rather than reordered.

## Examples

``` r
set.seed(1)
left <- data.frame(id = 1:20, x = rnorm(20), y = rnorm(20))
right <- data.frame(id = 1:60, x = rnorm(60), y = rnorm(60))
path <- match_path(left, right, vars = c("x", "y"),
                   vary = "max_distance", values = c(0.5, 1, 2, Inf))
path$path
path$balance
```
