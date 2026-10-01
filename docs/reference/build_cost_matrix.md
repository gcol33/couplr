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
  caller_supports_lazy = TRUE,
  caller_supports_implicit = caller_supports_lazy
)
```

## Arguments

- memory_mode:

  One of "auto" (default), "dense", "lazy" or "implicit". "auto" warns
  (or, when the caller supports it, switches) when the dense matrix
  would consume a large fraction of free system RAM.
  `memory_mode = "lazy"` returns a `lazy_cost_spec` instead of a matrix
  when the calling path and distance metric support it (built-in metrics
  via
  [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  with `method = "jv"`/`"auction"`); otherwise it errors clearly rather
  than silently falling back to dense. `memory_mode = "implicit"`
  returns the same specification marked for the edge-generation loop,
  which solves it without building the pair set at all.

- caller_supports_lazy:

  Whether the calling code path can actually consume a `lazy_cost_spec`
  result for a solve over every pair. Defaults to `TRUE`; a caller with
  no such solve (e.g.
  [`full_match()`](https://gillescolling.com/couplr/reference/full_match.md),
  whose flow over every pair would hold every pair) passes `FALSE` so
  `memory_mode = "auto"` never promotes to lazy for it, and an explicit
  `memory_mode = "lazy"` request errors clearly instead of returning a
  `lazy_cost_spec` the caller cannot use.

- caller_supports_implicit:

  Whether the calling path's design is one the edge-generation loop
  solves. Defaults to whatever the caller says about lazy, since the
  loop reads the same specification.

## Value

Numeric matrix of distances with optional scaling/weights applied.
