# Assignment Solvers

`lap_solvers()` lists the solvers
[`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
accepts as `method`, and `register_lap_solver()` adds one.

## Usage

``` r
lap_solvers()

register_lap_solver(
  name,
  solve,
  algorithm = name,
  potentials = FALSE,
  overwrite = FALSE
)
```

## Arguments

- name:

  Name under which the solver is selected with
  `assignment(method = name)`.

- solve:

  The solver function, see Details.

- algorithm:

  Short description shown by `lap_solvers()`.

- potentials:

  Logical; whether `solve` returns optimal dual potentials `u` and `v`
  of the matrix it receives.

- overwrite:

  Logical; whether to replace an existing solver of the same name.
  Built-in solvers cannot be replaced.

## Value

`lap_solvers()` returns a tibble with one row per solver and columns
`method`, `algorithm`, `reference`, `potentials` (returns optimal dual
potentials), `lazy` (available with `memory_mode = "lazy"`) and
`builtin`. `register_lap_solver()` returns `name` invisibly.

## Details

A solver is a function `solve(cost, maximize, ...)` that receives a
numeric matrix with no more rows than columns (forbidden pairs are
`Inf`, or `-Inf` when `maximize = TRUE`) and returns either an integer
vector giving the column assigned to each row, 0 for an unmatched row,
or a list with that vector as `match` and optionally `total_cost`,
`status` and the dual potentials `u` (rows) and `v` (columns).
[`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
transposes wider problems before calling it, computes the status and,
for a solver registered here, attaches a certificate by default:
[`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
checks the returned potentials when `potentials = TRUE` and otherwise
computes optimal ones, so a registered solver's answer is proved optimal
or reported as not.

## Examples

``` r
lap_solvers()

# A greedy heuristic: each row in turn takes its cheapest free column.
greedy <- function(cost, maximize, ...) {
  if (maximize) cost <- -cost
  match <- integer(nrow(cost))
  free <- rep(TRUE, ncol(cost))
  for (i in seq_len(nrow(cost))) {
    j <- which(free)[which.min(cost[i, free])]
    match[i] <- j
    free[j] <- FALSE
  }
  match
}
register_lap_solver("greedy_rows", greedy, algorithm = "Row-wise greedy")
cost <- matrix(c(1, 2, 3, 2, 4, 6, 3, 6, 10), 3)
res <- assignment(cost, method = "greedy_rows")
certificate(res)
```
