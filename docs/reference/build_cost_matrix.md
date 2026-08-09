# Build cost matrix for matching

This is the main entry point for distance computation.

## Usage

``` r
build_cost_matrix(
  left,
  right,
  vars,
  distance = "euclidean",
  weights = NULL,
  scale = FALSE,
  sigma = NULL,
  memory_mode = "auto",
  caller_supports_lazy = TRUE
)
```

## Arguments

- memory_mode:

  One of "auto" (default), "dense", or "lazy". "auto" warns (or, when
  the caller supports it, switches) when the dense matrix would consume
  a large fraction of free system RAM. `memory_mode = "lazy"` returns a
  `lazy_cost_spec` instead of a matrix when the calling path and
  distance metric support it (built-in metrics via
  [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  with `method = "jv"`/`"auction"`); otherwise it errors clearly rather
  than silently falling back to dense.

- caller_supports_lazy:

  Whether the calling code path can actually consume a `lazy_cost_spec`
  result. Defaults to `TRUE`; callers whose downstream solve path has
  not been made lazy-aware (e.g.
  [`full_match()`](https://gillescolling.com/couplr/reference/full_match.md),
  which uses an entirely different min-cost-flow backend) pass `FALSE`
  so `memory_mode = "auto"` never promotes to lazy for them, and an
  explicit `memory_mode = "lazy"` request errors clearly instead of
  returning a `lazy_cost_spec` the caller cannot use.

## Value

Numeric matrix of distances with optional scaling/weights applied.
