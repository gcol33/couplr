# Convert couplr Result to matchit Object

Constructs a `matchit`-class S3 object from a couplr result, enabling
use with any function that accepts MatchIt objects (e.g., cobalt,
marginaleffects).

## Usage

``` r
as_matchit(
  result,
  left,
  right,
  formula = NULL,
  left_id = "id",
  right_id = "id",
  estimand = NULL,
  ...
)
```

## Arguments

- result:

  A couplr result object (matching_result, full_matching_result,
  cem_result, or subclass_result)

- left:

  Data frame of left (treated) units

- right:

  Data frame of right (control) units

- formula:

  Optional formula used for matching. If not provided, a default formula
  is constructed from `result$info$vars`.

- left_id:

  Name of ID column in left (default: `"id"`)

- right_id:

  Name of ID column in right (default: `"id"`)

- estimand:

  Target estimand stamped on the matchit object: one of `"ATT"`, `"ATC"`
  or `"ATE"`. `NULL` (default) reads it from the design, which every
  couplr front door records as `info$estimand`. MatchIt and
  marginaleffects read this field to pick the target population and the
  weighting of the effect estimate, so give it explicitly whenever the
  design does not determine it – in particular when `left` holds the
  controls, which makes the design's left-focal weighting an ATC.

- ...:

  Additional arguments (ignored)

## Value

An S3 object of class `"matchit"` with fields:

- match.matrix:

  Match matrix (treated x controls)

- treat:

  Named treatment vector (1/0)

- weights:

  Matching weights

- X:

  Covariate matrix

- call:

  Original call

- info:

  Metadata from couplr

## Examples

``` r
if (FALSE) { # \dontrun{
left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
right <- data.frame(id = 6:15, age = runif(10, 20, 70))
result <- match_couples(left, right, vars = "age")
mi <- as_matchit(result, left, right)
# Now use with cobalt:
cobalt::bal.tab(mi)
} # }
```
