# Full Matching

Assigns every unit (left and right) to a matched group with variable
ratios (1:k or k:1). Unlike 1:1 matching, full matching does not discard
units, producing matched groups where each group contains at least one
left and one right unit.

## Usage

``` r
full_match(
  left,
  right,
  vars,
  distance = "euclidean",
  min_controls = 1,
  max_controls = Inf,
  caliper = NULL,
  caliper_sd = NULL,
  weights = NULL,
  scale = FALSE,
  auto_scale = FALSE,
  sigma = NULL,
  left_id = "id",
  right_id = "id",
  method = "optimal",
  memory_mode = "auto"
)
```

## Arguments

- left:

  Data frame of left (treated) units

- right:

  Data frame of right (control) units

- vars:

  Character vector of variable names to match on

- distance:

  Distance metric: `"euclidean"` (default), `"mahalanobis"`,
  `"manhattan"`, or a custom function

- min_controls:

  Minimum number of right units per group (default: 1)

- max_controls:

  Maximum number of right units per group (default: Inf)

- caliper:

  Maximum allowable distance for a match. Units with no eligible partner
  within the caliper are left unmatched.

- caliper_sd:

  If not NULL, caliper is expressed in standard deviations of the pooled
  distance distribution rather than absolute units.

- weights:

  Named numeric vector of variable weights

- scale:

  Scaling method: `FALSE` (default), `"robust"`, `"standardize"`, or
  `"range"`

- auto_scale:

  If TRUE, automatically preprocess and scale variables

- sigma:

  Optional covariance matrix for Mahalanobis distance

- left_id:

  Name of ID column in left (default: `"id"`)

- right_id:

  Name of ID column in right (default: `"id"`)

- method:

  Matching algorithm: `"optimal"` (default) uses min-cost max-flow to
  find the globally optimal group assignment minimizing total distance;
  `"greedy"` uses a fast two-pass heuristic.

- memory_mode:

  One of "auto" (default), "dense" or "implicit". "auto" warns if the
  dense cost matrix would consume a large fraction of free system RAM.
  "dense" skips the RAM check entirely. "implicit" solves
  `method = "optimal"` without building the pair set: the flow is solved
  over a growing subset of pairs, the pairs it omits are priced against
  the flow's node potentials, and the subset grows until none prices
  below zero, so the groups are optimal over every pair; a `search`
  element records what that cost. Under "implicit" `caliper_sd` reads
  every pair's distance once to take its standard deviation, holding two
  running sums rather than the distances. "lazy" is not available here:
  a flow solved over every pair holds every pair.

## Value

An S3 object of class `c("full_matching_result", "couplr_result")`
containing:

- groups:

  Tibble with columns `group_id`, `id`, `side` (`"left"`/`"right"`), and
  `weight`

- status:

  What the solver terminated on, one of `"optimal"` (every unit placed
  in a group meeting `min_controls`, at minimum total distance),
  `"partial"` (groups formed, some units left over, under a
  maximum-cardinality-then-minimum-cost objective), `"infeasible"` (no
  group meets the requested bounds), or `"heuristic"`
  (`method = "greedy"`, which neither claims nor checks optimality). See
  [`solver_status_values`](https://gillescolling.com/couplr/reference/solver_status_values.md).

- info:

  List with `n_groups`, `n_left`, `n_right`, `n_unmatched_left`,
  `n_unmatched_right`, `method`, `vars`

- unmatched:

  Left and right IDs that no group contains. Every unit is either a row
  of `groups` or an entry here.

- potentials:

  Node potentials from the flow solve, a list with elements `left` and
  `right` holding one value per unit. They are one representative of the
  dual, in the gauge the solver fixes. Present for `method = "optimal"`
  only.

- certificate:

  A `flow_certificate` from
  [`verify_flow`](https://gillescolling.com/couplr/reference/verify_flow.md),
  checking the solved flow and its potentials against the optimality
  conditions. `status` says what the solver terminated on; this says
  what was checked. Present for `method = "optimal"` only. Under
  `memory_mode = "implicit"` the check covers the pairs the flow was
  solved over, and `omitted_proven_floor` bounds the reduced cost of
  every pair it omitted; `certified_optimal` holds both together.

- search:

  Under `memory_mode = "implicit"`: the seed width, the pairs generated
  (`candidate_edges`) against the pairs there are (`possible_edges`),
  the distances computed (`edges_evaluated`) and one row per round.

## Details

`full_match()` builds matched groups of variable size. Under the default
`min_controls = 1` it solves full matching in the sense of Hansen and
Klopfer (2006): a group holds either one left unit and several right
ones or one right unit and several left ones, and both shapes may appear
in the same solution. `max_controls` bounds the many side there,
whichever side that is.

A lower bound above one admits only the one-to-many shape, because a
group built around a single right unit holds exactly one of them and
cannot meet a lower bound of two. Every group is then one left unit with
between `min_controls` and `max_controls` right ones. Both bounds count
right units whichever side holds more, so an instance with too few right
units to give every left unit that many is refused as `"infeasible"`
rather than answered with groups counted the other way round.

Two algorithms are available:

**Optimal** (`method = "optimal"`, default): Solves a min-cost max-flow
problem that minimizes total distance across all group assignments
simultaneously, with the optimum found via Dijkstra's algorithm with
Johnson potentials. At `min_controls = 1` the network is an edge cover
over the admissible pairs, which is what lets a group be centred on
either side; above one it is one centre per group and the centres are
the left units.

**Greedy** (`method = "greedy"`): A fast two-pass heuristic:

1.  Each left unit picks its nearest eligible right unit

2.  Remaining right units are assigned to their nearest already-matched
    left unit, respecting `max_controls`

This is faster but does not guarantee globally optimal results.

Every left unit carries weight 1, and the right units of a group share a
total weight equal to the number of left units in that group, so the two
sides of a group weigh the same. A group holding one left unit and k
right units therefore gives each right unit `1/k`; a group holding k
left units and one right unit gives that right unit `k`. `"greedy"`
builds one left unit per group and produces only the first shape.
`"optimal"` at `min_controls = 1` centres a group on whichever side is
larger, so both arise; above one the centres are the left units and the
first shape is the only one.

## Examples

``` r
set.seed(42)
left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
right <- data.frame(id = 6:20, age = runif(15, 20, 70))
result <- full_match(left, right, vars = "age")
print(result)
```
