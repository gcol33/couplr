# Hospital staff scheduling example dataset

A comprehensive example dataset for demonstrating couplr functionality
across vignettes. Contains hospital staff scheduling data with nurses,
shifts, costs, and preference scores suitable for assignment problems,
as well as nurse characteristics for matching workflows.

## Usage

``` r
hospital_staff
```

## Format

A list containing several related datasets:

- basic_costs:

  A 10x10 cost matrix for assigning 10 nurses to 10 shifts. Lower values
  indicate better fit (less overtime, matches skills).

- preferences:

  A 10x10 preference matrix (0-10 scale, higher = more preferred). Use
  with `maximize = TRUE`.

- schedule_df:

  A tibble with 100 rows (10 nurses x 10 shifts) containing:

  nurse_id

  :   Nurse identifier (1-10)

  shift_id

  :   Shift identifier (1-10)

  cost

  :   Assignment cost

  preference

  :   Nurse preference score (0-10)

  skill_match

  :   Whether nurse skills match shift needs (0/1)

- nurses:

  A tibble with 10 rows describing nurse characteristics:

  nurse_id

  :   Nurse identifier (1-10)

  experience_years

  :   Years of experience (1-20)

  department

  :   Primary department (ICU, ER, General, Pediatrics)

  shift_preference

  :   Preferred shift type (day, evening, night)

  certification_level

  :   Certification level (1-3)

- shifts:

  A tibble with 10 rows describing shift requirements:

  shift_id

  :   Shift identifier (1-10)

  department

  :   Department needing coverage

  shift_type

  :   Shift type (day, evening, night)

  min_experience

  :   Minimum years of experience required

  min_certification

  :   Minimum certification level required

- weekly_df:

  A tibble for batch solving: 5 days x 10 nurses x 10 shifts. Contains
  columns: day, nurse_id, shift_id, cost, preference.

- nurses_extended:

  A tibble with 200 nurses for matching examples. Contains: nurse_id,
  age, experience_years, hourly_rate, department, certification_level,
  is_fulltime.

- controls_extended:

  A tibble with 300 potential control nurses for matching examples. Same
  structure as nurses_extended.

## Details

This dataset is used throughout the couplr documentation to provide a
consistent, realistic example that evolves in complexity.

The dataset is designed to demonstrate progressively complex scenarios:

**Basic LAP**
([`vignette("getting-started")`](https://gcol33.github.io/couplr/articles/getting-started.md)):

- `basic_costs`: Simple 10x10 assignment

- `preferences`: Maximization problem

- `schedule_df`: Data frame input, grouped workflows

- `weekly_df`: Batch solving across days

**Algorithm comparison**
([`vignette("algorithms")`](https://gcol33.github.io/couplr/articles/algorithms.md)):

- Use `basic_costs` to compare algorithm behavior

- Modify with NA values for sparse scenarios

**Matching workflows**
([`vignette("matching-workflows")`](https://gcol33.github.io/couplr/articles/matching-workflows.md)):

- `nurses_extended`: Treatment group (full-time nurses)

- `controls_extended`: Control pool (part-time/registry nurses)

- Match on age, experience, department for causal analysis

## See also

[`lap_solve`](https://gcol33.github.io/couplr/reference/lap_solve.md)
for basic assignment solving,
[`lap_solve_batch`](https://gcol33.github.io/couplr/reference/lap_solve_batch.md)
for batch solving,
[`match_couples`](https://gcol33.github.io/couplr/reference/match_couples.md)
for matching workflows,
[`vignette("getting-started")`](https://gcol33.github.io/couplr/articles/getting-started.md)
for introductory tutorial

## Examples

``` r
# Basic assignment: assign nurses to shifts minimizing cost
lap_solve(hospital_staff$basic_costs)
#> Assignment Result
#> =================
#> 
#> # A tibble: 10 × 3
#>    source target  cost
#>     <int>  <int> <dbl>
#>  1      1      8     2
#>  2      2      2     2
#>  3      3      7     2
#>  4      4      4     2
#>  5      5      9     2
#>  6      6     10     3
#>  7      7      1     2
#>  8      8      3     2
#>  9      9      6     3
#> 10     10      5     2
#> 
#> Total cost: 22 
#> Method: hungarian 

# Maximize preferences instead
lap_solve(hospital_staff$preferences, maximize = TRUE)
#> Assignment Result
#> =================
#> 
#> # A tibble: 10 × 3
#>    source target  cost
#>     <int>  <int> <dbl>
#>  1      1      8     9
#>  2      2      2     9
#>  3      3      7     9
#>  4      4      4     9
#>  5      5      9     9
#>  6      6     10     8
#>  7      7      1     9
#>  8      8      3     9
#>  9      9      6     8
#> 10     10      5     9
#> 
#> Total cost: 88 
#> Method: hungarian 

# Data frame workflow
library(dplyr)
hospital_staff$schedule_df |>
  lap_solve(nurse_id, shift_id, cost)
#> Assignment Result
#> =================
#> 
#> # A tibble: 10 × 3
#>    source target  cost
#>     <int>  <int> <dbl>
#>  1      1      8     2
#>  2      2      2     2
#>  3      3      7     2
#>  4      4      4     2
#>  5      5      9     2
#>  6      6     10     3
#>  7      7      1     2
#>  8      8      3     2
#>  9      9      6     3
#> 10     10      5     2
#> 
#> Total cost: 22 
#> Method: hungarian 

# Batch solve weekly schedule
hospital_staff$weekly_df |>
  group_by(day) |>
  lap_solve(nurse_id, shift_id, cost)
#> # A tibble: 50 × 4
#>    day    source target  cost
#>    <fct>   <int>  <int> <dbl>
#>  1 Monday      1      1     2
#>  2 Monday      2      9     2
#>  3 Monday      3      3     1
#>  4 Monday      4      4     0
#>  5 Monday      5      5     2
#>  6 Monday      6      6     1
#>  7 Monday      7      8     4
#>  8 Monday      8     10     1
#>  9 Monday      9      2     4
#> 10 Monday     10      7     1
#> # ℹ 40 more rows

# Matching workflow: match full-time to part-time nurses
match_couples(
  left = hospital_staff$nurses_extended,
  right = hospital_staff$controls_extended,
  vars = c("age", "experience_years", "certification_level"),
  auto_scale = TRUE
)
#> Auto-selected scaling method: standardize
#> Matching Result
#> ===============
#> 
#> Method: lap 
#> Pairs matched: 200 
#> Unmatched (left): 0 
#> Unmatched (right): 100 
#> Total distance: 59.9496 
#> 
#> Matched pairs:
#> # A tibble: 200 × 6
#>    left_id right_id  distance .age_diff .experience_years_diff
#>    <chr>   <chr>        <dbl>     <dbl>                  <dbl>
#>  1 left_1  right_60     0.217        -1                      1
#>  2 left_2  right_94     0.175         2                      0
#>  3 left_3  right_12     0             0                      0
#>  4 left_4  right_253    0.530        -4                      2
#>  5 left_5  right_262    0.481        -5                      1
#>  6 left_6  right_73     0             0                      0
#>  7 left_7  right_296    0.199         0                      1
#>  8 left_8  right_92     0.262         3                      0
#>  9 left_9  right_151    0.477        -3                      2
#> 10 left_10 right_247    0             0                      0
#> # ℹ 190 more rows
#> # ℹ 1 more variable: .certification_level_diff <int>
```
