# Solver status values

The closed set of values a couplr solve result's `status` field can
take.

## Usage

``` r
solver_status_values()
```

## Value

Character vector of the permitted status values.

## Details

- `"optimal"`:

  The solver terminated on its own optimality condition and every row it
  was asked to match is matched.

- `"partial"`:

  Feasible, but fewer rows are matched than requested, under a
  maximum-cardinality-then-minimum-cost objective.

- `"infeasible"`:

  No assignment of the requested cardinality exists.

- `"eps_optimal"`:

  An auction solve terminated at a caller-supplied epsilon. The result
  is within `n * epsilon` of optimal, which is a bound, not a
  certificate.

- `"iteration_limit"`:

  The solver stopped on an iteration cap rather than on optimality. The
  solution is feasible; its optimality is unproven.

- `"interrupted"`:

  The solver stopped on a time budget or a user interrupt, part way
  through. What it had placed respects every capacity, and is short of
  what the problem asked it to place, so it is neither an answer nor
  evidence that no answer exists.

- `"heuristic"`:

  Produced by a heuristic. Optimality is neither claimed nor checked.

## See also

[`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
to turn a status into a checked certificate.

## Examples

``` r
solver_status_values()
```
