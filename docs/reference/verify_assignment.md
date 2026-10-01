# Verify that an assignment is optimal

Checks a solved assignment against the linear-programming optimality
conditions and returns the result of each check. Unlike the `status`
field on a solve result, which records what the solver terminated on,
this is a statement about the matching: `certified_optimal` is `TRUE`
only when every condition holds.

## Usage

``` r
verify_assignment(
  x,
  cost = NULL,
  duals = NULL,
  maximize = FALSE,
  tol = 1e-09,
  arithmetic = c("auto", "exact", "double")
)

# S3 method for class 'assignment_certificate'
print(x, ...)
```

## Arguments

- x:

  An `assignment_certificate` object.

- cost:

  Numeric cost matrix the assignment was computed on, or a lazy cost
  specification from
  [`compute_distances()`](https://gillescolling.com/couplr/reference/compute_distances.md).
  Required unless `x` already carries one.

- duals:

  Optional list with elements `u` and `v` giving row and column
  potentials in the orientation of `cost`. Overrides any duals on `x`.
  Either element may be a matrix with one row per potential, as
  `exact_u` and `exact_v` are, in which case each potential is the exact
  sum of its row.

- maximize:

  Logical; whether the assignment maximized rather than minimized.
  Defaults to `FALSE`.

- tol:

  Numeric tolerance for the feasibility and slackness comparisons of a
  numerical certificate, ignored by an exact one. The duality-gap
  comparison scales it by the magnitude of the objective, since an
  absolute tolerance on a sum of many terms is not reachable in double
  precision.

- arithmetic:

  One of `"auto"`, `"exact"` or `"double"`. `"auto"`, the default,
  reports the exact conclusion when the exact conditions hold and the
  numerical one otherwise, which is the strongest statement the instance
  supports; the exact conditions imply the numerical ones at any
  non-negative `tol`, so `"auto"` certifies nothing `"double"` would
  refuse. `"exact"` refuses to fall back, so `certified_optimal` is then
  `TRUE` only on an exact certificate. `"double"` decides everything
  within `tol`.

- ...:

  Ignored.

## Value

An object of class `assignment_certificate`, a list with elements:

- `certified_optimal` — logical, the conclusion. `TRUE` only when every
  condition below holds.

- `arithmetic` — `"exact"` or `"double"`, the arithmetic the conclusion
  was reached in.

- `exact_certificate` — logical; whether the conditions hold in exact
  arithmetic. `arithmetic = "double"` does not ask the question, and
  reports `FALSE` here and in `exact_available`.

- `exact_available` — logical; whether the exact question was asked of
  every condition. It is `FALSE` under `arithmetic = "double"`, and on a
  certificate assembled from a scan that priced part of the problem
  against a tolerance rather than evaluating it, which is what the
  edge-generation loop hands back.

- `n_exact_violations`, `n_exact_untight` — pairs failing exact dual
  feasibility, and matched pairs not exactly tight, under the duals
  given. Potentials recovered from the matching prove the matching and
  leave these counts describing the duals that missed.

- `exact_duals_source` — which potentials decided the exact certificate:
  `"solver"` for the duals as supplied or solved, `"supplied"` for
  expansions passed in `duals`, `"recovered"` for potentials recovered
  from the matching, and `"none"` when no exact certificate was reached.

- `exact_u`, `exact_v` — on an exact certificate, the row and column
  potentials that decided it, one row per potential whose sum is its
  exact value (a single column when the solver's doubles decided it);
  `NULL` when there is no exact certificate.

- `structurally_valid_matching` — logical; no column claimed twice, no
  forbidden pair matched, no index out of range. Unmatched rows are
  permitted, so this holds for a partial matching.

- `all_rows_matched` — logical; every row of the short side holds a
  column.

- `primal_feasible` — logical; both of the two above. The primal
  constrains every row of the short side to hold exactly one pair, so a
  partial matching is a valid matching and not a feasible solution, and
  no conclusion rests on it. `primal_objective` is still reported for
  one, since an unmatched row costs nothing and leaves the sum
  meaningful.

- `dual_feasible` — logical; `c_ij - u_i - v_j >= -tol` over every
  admissible pair, and, when there are more columns than rows,
  `v_j <= tol` for every column.

- `complementary_slackness` — logical; both halves.

- `cs_matched_tight`, `cs_unmatched_free` — the two halves separately.

- `primal_objective`, `dual_objective`, `duality_gap` — numeric.

- `max_suboptimality` — numeric; the most any feasible solution can beat
  this one by, in the cost unit. It adds to the duality gap the slack
  the dual conditions were allowed: `n_rows` times the depth the reduced
  costs were permitted below zero, plus, where the sign condition
  applies, `n_cols` times the height the column duals were permitted
  above it, plus an envelope for each objective's own rounding.
  Compensated summation buys back the accumulation error rather than
  removing it, so each sum is charged `(2u + gamma_n^2)` times the sum
  of its terms' magnitudes and the assembly is rounded outward at every
  step. The number is an upper bound in double arithmetic and not an
  estimate of one. An exact certificate proves the matching optimal and
  reports zero. `NA` when `primal_feasible` is `FALSE`: the quantity is
  what a feasible solution can beat this one by, and there is no answer
  for a candidate that is not one.

- `certified_reduced_cost_floor` — numeric; the lower bound proved for
  the reduced cost of every admissible pair, the ones never evaluated
  included. Equal to `min_reduced_cost` when every pair was visited, and
  below it when a pruning pricer proved only its own threshold.

- `min_reduced_cost`, `worst_i`, `worst_j` — the most violated pair, if
  any.

- `max_matched_slack`, `max_v_unmatched`, `max_v` — the quantities the
  slackness and sign conditions bound.

- `n_matched`, `n_rows`, `n_cols`, `transposed`, `tolerance`.

Invisibly returns `x`.

## What the certificate proves

The conditions are decided in one of two arithmetics, and the
certificate says which one it used.

An **exact certificate** decides every condition in exact arithmetic,
with no tolerance anywhere. It proves that the matching attains the
minimum total cost of the cost matrix as supplied. The three quantities
in each condition are IEEE doubles, and a double is a rational number,
so `c_ij - u_i - v_j` has an exact sign; the check evaluates that sign
exactly rather than reading the sign of a rounded difference. A cost
matrix that is itself a rounding of something else — Mahalanobis
distances, say — is still certified as the matrix it is, which is the
problem the solver was given.

A **numerical certificate** decides the same conditions within `tol`. It
establishes optimality up to that tolerance and no further. It is what
is available when the potentials are not exactly optimal for the matrix:
a potential computed as `c_ij - v_j` misses exact tightness by the
rounding of that subtraction, and a matched arc off by one unit in the
last place is enough to put the exact conclusion out of reach.

The exact certificate does not depend on the solver's potentials being
exact. Optimal duals are sums and differences of cost entries, and a
double often cannot hold one: on computed costs such as Euclidean or
Mahalanobis distances the solver's potentials miss exact tightness on
some matched arc by a unit in the last place. When the duals given pass
the numerical reading but not the exact one, exact potentials are
recovered from the matching itself, as shortest-path distances over the
cost entries held in exact multi-component arithmetic (Shewchuk 1997),
and the conditions are decided on those. A matching that is not optimal
has no such potentials, so the recovery cannot certify one; it finds the
improving cycle or path instead, and the numerical reading stands. Duals
that fail even the numerical reading certify nothing, and the check
fails on them. The recovery costs one pass over the admissible pairs.

Exact potentials generally do not fit in one double each, so they are
returned as `exact_u` and `exact_v`, matrices with one row per potential
whose row sums are the exact values. Summing each row in rational
arithmetic (for example with
[`gmp::as.bigq`](https://rdrr.io/pkg/gmp/man/bigrational.html))
reproduces them, and passing them back as `duals` has the conditions
checked again from the cost matrix alone, so the certificate can be
re-checked without trusting this code.

The check needs dual variables. If `x` carries them (as
[`assignment_duals()`](https://gillescolling.com/couplr/reference/assignment_duals.md)
results do), they are used. Otherwise they are obtained by solving
`cost` with
[`assignment_duals()`](https://gillescolling.com/couplr/reference/assignment_duals.md),
which costs a second solve. Either way the duals are verified, not
trusted: dual feasibility is checked over every admissible pair, so
duals that do not certify anything cause the verification to fail rather
than pass.

Optimal duals are shared by all optimal solutions of a linear program,
so a matching from one solver can be certified against duals from
another. That is what makes it possible to certify solvers that return
no duals of their own.

## See also

[`assignment()`](https://gillescolling.com/couplr/reference/assignment.md),
[`assignment_duals()`](https://gillescolling.com/couplr/reference/assignment_duals.md),
[`solver_status_values()`](https://gillescolling.com/couplr/reference/solver_status_values.md)

## Examples

``` r
set.seed(1)
cost <- matrix(runif(100), 10, 10)
verify_assignment(assignment(cost), cost)

# A rectangular problem, where the condition on unmatched columns bites.
# Passing the duals result reuses its duals instead of solving again.
rect <- matrix(runif(120), 6, 20)
verify_assignment(assignment_duals(rect), rect)

# Integer costs carry an integer optimal dual solution, so the conditions
# hold with no tolerance and the certificate is exact.
int_cost <- matrix(sample(1:100, 64, replace = TRUE), 8, 8)
cert <- verify_assignment(assignment(int_cost), int_cost)
cert$arithmetic
```
