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

A list containing eight related datasets:

- basic_costs:

  A 10x10 numeric cost matrix for assigning 10 nurses to 10 shifts.
  Values range from approximately 1-15, where lower values indicate
  better fit (less overtime, matches skills, respects preferences). Use
  with
  [`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
  for basic assignment.

- preferences:

  A 10x10 numeric preference matrix on a 0-10 scale, where higher values
  indicate stronger nurse preference for a shift. Use with
  `lap_solve(..., maximize = TRUE)` to optimize preferences rather than
  minimize costs.

- schedule_df:

  A tibble with 100 rows (10 nurses x 10 shifts) in long format for data
  frame workflows:

  nurse_id

  :   Integer 1-10. Unique identifier for each nurse.

  shift_id

  :   Integer 1-10. Unique identifier for each shift.

  cost

  :   Numeric. Assignment cost (same values as basic_costs).

  preference

  :   Numeric 0-10. Nurse preference score.

  skill_match

  :   Integer 0/1. Binary indicator: 1 if nurse skills match shift
      requirements, 0 otherwise.

- nurses:

  A tibble with 10 rows describing nurse characteristics:

  nurse_id

  :   Integer 1-10. Links to schedule_df and basic_costs rows.

  experience_years

  :   Numeric 1-20. Years of nursing experience.

  department

  :   Character. Primary department: "ICU", "ER", "General", or
      "Pediatrics".

  shift_preference

  :   Character. Preferred shift type: "day", "evening", or "night".

  certification_level

  :   Integer 1-3. Certification level where 3 is highest (e.g., 1=RN,
      2=BSN, 3=MSN).

- shifts:

  A tibble with 10 rows describing shift requirements:

  shift_id

  :   Integer 1-10. Links to schedule_df and basic_costs cols.

  department

  :   Character. Department needing coverage.

  shift_type

  :   Character. Shift type: "day", "evening", or "night".

  min_experience

  :   Numeric. Minimum years of experience required.

  min_certification

  :   Integer 1-3. Minimum certification level.

- weekly_df:

  A tibble for batch solving with 500 rows (5 days x 10 nurses x 10
  shifts):

  day

  :   Character. Day of week: "Mon", "Tue", "Wed", "Thu", "Fri".

  nurse_id

  :   Integer 1-10. Nurse identifier.

  shift_id

  :   Integer 1-10. Shift identifier.

  cost

  :   Numeric. Daily assignment cost (varies by day).

  preference

  :   Numeric 0-10. Daily preference score.

  Use with `group_by(day)` for solving each day's schedule.

- nurses_extended:

  A tibble with 200 nurses for matching examples, representing a
  treatment group (e.g., full-time nurses):

  nurse_id

  :   Integer 1-200. Unique identifier.

  age

  :   Numeric 22-65. Nurse age in years.

  experience_years

  :   Numeric 0-40. Years of nursing experience.

  hourly_rate

  :   Numeric 25-75. Hourly wage in dollars.

  department

  :   Character. Primary department assignment.

  certification_level

  :   Integer 1-3. Certification level.

  is_fulltime

  :   Logical. TRUE for full-time status.

- controls_extended:

  A tibble with 300 potential control nurses (e.g., part-time or
  registry nurses) for matching. Same structure as nurses_extended.
  Designed to have systematic differences from nurses_extended (older,
  less experience on average) to demonstrate matching's ability to
  create comparable groups.

## Details

This dataset is used throughout the couplr documentation to provide a
consistent, realistic example that evolves in complexity. It supports
three use cases: (1) basic LAP solving with cost matrices, (2) batch
solving across multiple days, and (3) matching workflows comparing nurse
groups.

The dataset is designed to demonstrate progressively complex scenarios:

**Basic LAP**
([`vignette("getting-started")`](https://gillescolling.com/couplr/articles/getting-started.md)):

- `basic_costs`: Simple 10x10 assignment

- `preferences`: Maximization problem

- `schedule_df`: Data frame input, grouped workflows

- `weekly_df`: Batch solving across days

**Algorithm comparison**
([`vignette("algorithms")`](https://gillescolling.com/couplr/articles/algorithms.md)):

- Use `basic_costs` to compare algorithm behavior

- Modify with NA values for sparse scenarios

**Matching workflows**
([`vignette("matching-workflows")`](https://gillescolling.com/couplr/articles/matching-workflows.md)):

- `nurses_extended`: Treatment group (full-time nurses)

- `controls_extended`: Control pool (part-time/registry nurses)

- Match on age, experience, department for causal analysis

## See also

[`lap_solve`](https://gillescolling.com/couplr/reference/lap_solve.md)
for basic assignment solving,
[`lap_solve_batch`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
for batch solving,
[`match_couples`](https://gillescolling.com/couplr/reference/match_couples.md)
for matching workflows,
[`vignette("getting-started")`](https://gillescolling.com/couplr/articles/getting-started.md)
for introductory tutorial

## Examples

``` r
# Basic assignment: assign nurses to shifts minimizing cost
lap_solve(hospital_staff$basic_costs)

# Maximize preferences instead
lap_solve(hospital_staff$preferences, maximize = TRUE)

# Data frame workflow
library(dplyr)
hospital_staff$schedule_df |>
  lap_solve(nurse_id, shift_id, cost)

# Batch solve weekly schedule
hospital_staff$weekly_df |>
  group_by(day) |>
  lap_solve(nurse_id, shift_id, cost)

# Matching workflow: match full-time to part-time nurses
match_couples(
  left = hospital_staff$nurses_extended,
  right = hospital_staff$controls_extended,
  vars = c("age", "experience_years", "certification_level"),
  auto_scale = TRUE
)
```
