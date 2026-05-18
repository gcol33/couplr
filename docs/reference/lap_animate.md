# Animate an assignment algorithm step-by-step

Produce an interactive bipartite-graph animation showing how a chosen
linear-assignment algorithm transforms the matching over time. The
result is an htmlwidget suitable for use in R Markdown, Quarto, pkgdown
vignettes, Shiny apps, or standalone HTML output.

## Usage

``` r
lap_animate(
  cost,
  method = "hungarian",
  maximize = FALSE,
  width = NULL,
  height = NULL,
  elementId = NULL,
  ...
)
```

## Arguments

- cost:

  Numeric cost matrix. Rows = workers/sources, columns = jobs/targets.
  `NA` or `Inf` entries mark forbidden assignments.

- method:

  Character; the algorithm to animate. Must match one of the methods
  registered for animation (see
  [`animated_methods()`](https://gillescolling.com/couplr/reference/animated_methods.md)
  for the current list). Method names are the same strings used by
  [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md),
  e.g. `"hungarian"`, `"jv"`, `"auction"`, `"gabow_tarjan"`.

- maximize:

  Logical; if `TRUE`, animate the maximization variant.

- width, height:

  Optional explicit widget dimensions (pixels or CSS units).

- elementId:

  Optional DOM id for the widget container.

- ...:

  Algorithm-specific extra arguments forwarded to the trace function
  (e.g. `auction_eps`).

## Value

An `htmlwidget` object.

## Details

This is a *teaching* interface. It runs a slower R reference
implementation that emits a state trace at every step, then plays it
back in the browser. For production solving, use
[`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
or
[`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
which call the fast C++ backends.

## Animated methods

Animation support is added incrementally. Call
[`animated_methods()`](https://gillescolling.com/couplr/reference/animated_methods.md)
to see which method strings currently have a registered trace.

## See also

[`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
for production solving,
[`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
for the tidy interface.

## Examples

``` r
if (FALSE) { # \dontrun{
cost <- matrix(c(4, 2, 5,
                 3, 3, 6,
                 7, 5, 4), nrow = 3, byrow = TRUE)
lap_animate(cost, method = "hungarian")
} # }
```
