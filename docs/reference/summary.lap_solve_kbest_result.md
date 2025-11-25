# Get summary of k-best results

Extract summary information from k-best assignment results.

## Usage

``` r
# S3 method for class 'lap_solve_kbest_result'
summary(object, ...)
```

## Arguments

- object:

  An object of class `lap_solve_kbest_result`.

- ...:

  Additional arguments (unused).

## Value

A tibble with one row per solution containing:

- `rank`: solution rank

- `solution_id`: solution identifier

- `total_cost`: total cost of the solution

- `n_assignments`: number of assignments in the solution
