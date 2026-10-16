# Cardinality Matching

Maximizes the number of matched pairs subject to balance constraints,
and reports how far the matched sample sits from the largest one the
constraints admit.

## Usage

``` r
cardinality_match(
  left,
  right,
  vars,
  fine = NULL,
  refined = NULL,
  refined_exact = 1L,
  moments = NULL,
  max_std_diff = Inf,
  engine = c("auto", "flow", "branch_bound", "heuristic"),
  distance = "euclidean",
  weights = NULL,
  scale = FALSE,
  auto_scale = FALSE,
  sigma = NULL,
  max_distance = Inf,
  calipers = NULL,
  left_id = NULL,
  right_id = NULL,
  time_limit = 30,
  node_limit = 500L,
  method = "auto",
  max_iter = 100L,
  batch_fraction = 0.1,
  memory_mode = "auto"
)
```

## Arguments

- left:

  Data frame of "left" units, one row each.

- right:

  Data frame of "right" units, one row each.

- vars:

  Character vector of matching variable names.

- fine:

  Exact fine balance on one partition: a character vector of column
  names, cross-classified, or a one-sided formula. In a formula `+` and
  `:` both mean interaction, so `~ region + sex` and `~ region:sex` both
  ask for balance on every region-by-sex cell. `fine = X` is the same
  request as `refined = list(X), refined_exact = 1`.

- refined:

  A nested hierarchy of groupings, coarsest first: a list, each element
  a character vector or one-sided formula, or a character vector as
  shorthand for its own sequence of prefixes, so `c("region", "site")`
  means region, then region crossed with site. Each level must sit
  inside the one before it.

- refined_exact:

  How many of the coarsest levels of `refined` are enforced exactly
  (default: 1). The remaining levels are balanced as closely as the
  exact levels allow.

- moments:

  Moment constraints beyond `max_std_diff`: a named numeric of
  standardized-difference bounds, as in `c(x = 0.05, y = 0.1)`, or a
  list of entries with fields `var`, `stat` (`"std_diff"` or
  `"mean_diff"`), `transform`, `max` and `min`. A variable bounded here
  must not also be bounded by `max_std_diff`.

- max_std_diff:

  Maximum absolute standardized difference on every variable in `vars`
  (default: `Inf`, stating no such constraint). A finite value states
  one moment row per variable and per direction, which is what sends a
  call to the search over unit selections; see Details.

- engine:

  Which solver answers the problem: `"flow"` for a single min-cost flow
  solve, available when no moment row is stated; `"branch_bound"` for
  the search over unit selections that moment rows need; `"auto"`
  (default) for whichever of the two the stated constraints call for;
  and `"heuristic"` for the pruning loop.

- distance:

  Distance metric (default: "euclidean").

- weights:

  Optional named vector of variable weights.

- scale:

  Scaling method (default: FALSE).

- auto_scale:

  If TRUE, check variable health and select scaling automatically
  (default: FALSE).

- sigma:

  Bandwidth for the distance metrics that take one.

- max_distance:

  Pairs farther apart than this are not available.

- calipers:

  Named vector of caliper widths, each on a column of both data frames:
  a matching variable or another one, such as a propensity score.

- left_id, right_id:

  Name of the id column on each side. When absent, ids come from an `id`
  column, from row names, or are synthesized.

- time_limit:

  Seconds the search may run (default: 30). A search that stops on the
  clock returns a result that depends on the speed of the machine. Set
  `time_limit = Inf` and bound the search with `node_limit` to control
  the amount of search independently of machine speed. Floating-point
  ties can change the search order across platforms.

- node_limit:

  Branch-and-bound nodes the search may open, counted over all of its
  stages (default: 500).

- method:

  LAP solver method for the heuristic's initial match (default: "auto").
  Reaches `engine = "heuristic"` only.

- max_iter:

  Maximum pruning iterations (default: 100). Reaches
  `engine = "heuristic"` only.

- batch_fraction:

  Fraction of the remaining pairs the heuristic deletes per iteration
  (default: 0.1). Reaches `engine = "heuristic"` only.

- memory_mode:

  One of "auto" (default), "dense" or "implicit". "implicit" solves the
  flow and branch and bound engines without building the pair set: every
  network solve is taken over the pairs generated so far, the pairs it
  omits are priced against its potentials, and the ones pricing below
  zero are added and the solve repeated, so each solve is optimal over
  every pair. The distance range the tier weights are built on is read
  in one pass over the pairs. The result carries a `search` record.
  "auto" never selects it, and the heuristic engine does not take it.

## Value

A `matching_result` object. Beyond the fields every matching carries it
holds:

- `cardinality` - the report: `n_matched`, `best_possible`, `gap`,
  `gap_fraction`, `certified`, `stopped_on`, `n_nodes`, and the state of
  every stated constraint. See
  [`print.cardinality_report()`](https://gillescolling.com/couplr/reference/dot-cardinality_report.md).

- `certificate` - present when the search certified optimality, with
  `arithmetic = "exact"`: the bounds, the comparisons between them and
  the matched sample's objective and constraint values it rests on are
  all decided exactly.

- `potentials` - the dual potentials of the network solve the matched
  sample came from, a list with elements `left` and `right` holding one
  value per unit, named by id, in distance terms: the reduced cost of a
  pair is its distance less the two, with the tier weights already
  folded in. Under `engine = "flow"` that solve is the whole problem,
  and the potentials certify the matched sample as its optimum. Under
  branch and bound it is the solve that pairs the selected units, and
  the potentials certify that pairing as the cheapest one for those
  units; the bound on the whole problem is `cardinality$best_possible`.
  Present when that solve was certified.

- `status` - `"optimal"`, `"iteration_limit"`, or `"heuristic"`.

- `info$engine` - the engine that answered.

`info$pruning_iterations` and `info$pairs_removed` are carried by
`engine = "heuristic"`.

## Details

Cardinality matching (Zubizarreta 2012) asks for the largest matched
sample that satisfies stated balance constraints, rather than the
closest one. The constraints come in two kinds, and which kinds are
stated decides how the problem is solved.

Fine and refined covariate balance are representable in the matching
network itself (Rosenbaum, Ross and Silber 2007; Pimentel, Kelz, Silber
and Rosenbaum 2015): every category becomes a node, and a single
min-cost flow solve returns the largest balanced sample together with a
dual certificate of its optimality, at polynomial cost. `max_std_diff`
defaults to `Inf`, stating no moment constraint, so a call asking for
fine or refined balance alone takes this path and comes back certified:

    cardinality_match(left, right, vars, fine = "region")

Linear moment constraints, bounds on a standardized or mean difference,
do not fit in the network. Summed over the matched pairs, such a bound
is a linear constraint on which units are matched, never on how they are
paired, so `engine = "branch_bound"` searches over unit selections: an
integer programme with one 0/1 variable per unit and a few rows, solved
by branch and bound on its linear relaxation. A finite `max_std_diff`
states one such row per variable and per direction, so it is what moves
a call onto this path.

The objective is lexicographic, and the search settles it one stage at a
time. It first finds the most matched units the constraints admit, then
the least imbalance at each refined level, coarsest first, and last the
least total distance. A pair is never added at the cost of a match and
never dropped to shorten one. The distance stage reads the cheapest
pairing of a selection off the balance network and adds a cut from the
potentials of that solve (Benders decomposition), and when calipers or
`max_distance` forbid pairs, a selection whose units cannot all be
paired adds a Hall-condition row instead.

The report says how the search ended. `certified` is `TRUE` only when
every stage closed, and `gap` says how many matched units separate the
answer from the bound when the first stage did not. Every stopping path
returns a matched set that satisfies every stated constraint, together
with a bound valid for the whole problem. When the distance stage stops
on the budget, the matched sample is a largest balanced one, with `gap`
zero, whose total distance is the smallest the search reached.

The certificate carries no tolerance. Each bound is a weak-duality bound
read from the relaxation's duals in exact arithmetic or with a rigorous
rounding margin, so it holds whatever the simplex returned, and every
selection is checked against the constraints exactly before it is
accepted.

How long the search runs depends on how far the relaxation sits from the
best integer selection. With continuous covariates the relaxation's
bound on the number of matched units is usually within one unit of the
optimum, and the search closes at its first node when it finds a
selection that reaches it. When the optimum lies one unit below that
bound, proving it takes many more nodes, and the report shows the
incumbent beside a bound one unit above it. Set `node_limit` and
`time_limit` to a budget you are willing to spend, and read `stopped_on`
and `gap` to see what the budget bought.

`engine = "heuristic"` runs a pruning loop instead: a full match, then
repeated deletion of the pairs carrying the worst variable's imbalance.
Its properties follow from the loop. It starts from a full match and
deletes pairs, and never re-adds one, so it cannot recover from an
over-aggressive deletion. It never maximizes cardinality, it only
shrinks a starting match. It considers one variable per iteration, the
current worst, so balance on the others is incidental. It computes no
bound, so it cannot say how far its answer sits from the largest
balanced sample; `best_possible` and `gap` are `NA` and `certified` is
`FALSE`.

## References

Zubizarreta, J. R. (2012). Using mixed integer programming for matching
in an observational study of kidney failure after surgery. *Journal of
the American Statistical Association*, 107(500), 1360-1371.

Rosenbaum, P. R., Ross, R. N. and Silber, J. H. (2007). Minimum distance
matched sampling with fine balance in an observational study of
treatment for ovarian cancer. *Journal of the American Statistical
Association*, 102(477), 75-83.

Pimentel, S. D., Kelz, R. R., Silber, J. H. and Rosenbaum, P. R. (2015).
Large, sparse optimal matching with refined covariate balance in an
observational study of the health outcomes produced by new surgeons.
*Journal of the American Statistical Association*, 110(510), 515-527.

## See also

[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
for distance-minimizing matching,
[`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
for reading balance off any matched sample.

## Examples

``` r
set.seed(42)
left <- data.frame(id = 1:20, x = rnorm(20), y = rnorm(20),
                   region = rep(c("A", "B"), length.out = 20))
right <- data.frame(id = 21:50, x = rnorm(30, 0.5), y = rnorm(30, 0.3),
                    region = rep(c("A", "B"), length.out = 30))

# Exact fine balance on region, no moment constraint: one flow solve,
# answered with a certificate.
fit <- cardinality_match(left, right, vars = c("x", "y"),
                         fine = "region")
fit$cardinality

# A standardized-difference bound as well: the same match, searched over
# unit selections under a small node budget.
bb <- cardinality_match(left, right, vars = c("x", "y"),
                        fine = "region", max_std_diff = 0.1,
                        node_limit = 25L)
bb$cardinality
```
