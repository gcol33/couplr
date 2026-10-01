# Solve an implicit assignment by generating the pairs it needs

The backend for `memory_mode = "implicit"`. Takes either a
`lazy_cost_spec`, which states the complete problem without building it,
or a materialized matrix, which states the same problem and lets the
loop's answer be held against a complete solve of the same numbers.

## Usage

``` r
.assignment_implicit(
  cost,
  maximize = FALSE,
  certify = TRUE,
  method = "auto",
  keep_per_row = .implicit_defaults()$keep_per_row,
  width = .implicit_defaults()$width,
  tol = .implicit_defaults()$tol,
  max_rounds = .implicit_defaults()$max_rounds
)
```

## Value

A `lap_solve_result` carrying the duals the last master produced, the
certificate for the complete problem, and `search`, the record of what
the search cost.
