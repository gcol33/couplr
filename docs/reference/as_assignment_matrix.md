# Convert an Assignment Result to a Binary Matrix

Turns a list or tidy assignment result into a 0/1 assignment matrix.
Original dimensions and identifiers are retained where recorded.

## Usage

``` r
as_assignment_matrix(x, n_sources = NULL, n_targets = NULL)
```

## Arguments

- x:

  An assignment result object of class `lap_solve_result`

- n_sources:

  Number of source nodes, optional

- n_targets:

  Number of target nodes, optional

## Value

Integer matrix with 0 and 1 entries
