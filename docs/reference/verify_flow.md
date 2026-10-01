# Verify that a flow is optimal

Checks a flow and a set of node potentials against the
linear-programming optimality conditions for the minimum-cost flow
problem, and returns the result of each check. Unlike the `status` field
on a solve result, which records what the solver terminated on, this is
a statement about the flow: `certified_optimal` is `TRUE` only when
every condition holds.

## Usage

``` r
verify_flow(
  x,
  problem = NULL,
  potential = NULL,
  tol = 1e-09,
  arithmetic = c("auto", "exact", "double")
)

# S3 method for class 'flow_certificate'
print(x, ...)
```

## Arguments

- x:

  A `flow_certificate` object.

- problem:

  The flow problem the flow belongs to: a `couplr_flow_problem` or a
  list with elements `n_nodes`, `supply` and `arcs`, where `arcs` is a
  data frame with columns `tail`, `head`, `lower`, `upper` and `cost`
  and node ids run from 1. Required unless `x` already carries one.

- potential:

  Optional numeric vector of node potentials, one per node. Overrides
  any potentials on `x`. It may also be a matrix with one row per node,
  as `exact_potential` is, in which case each potential is the exact sum
  of its row.

- tol:

  Relative tolerance for the feasibility and slackness comparisons. Each
  arc scales it by the largest of its cost and its two potentials, and
  the duality-gap comparison scales it by the magnitude of the
  objective, since a threshold below the resolution of the arithmetic
  that produced a number is not one a correct answer can meet. Ignored
  by an exact certificate.

- arithmetic:

  One of `"auto"`, `"exact"` or `"double"`, as for
  [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md).
  `"auto"`, the default, reports the exact conclusion when the exact
  conditions hold and the numerical one otherwise.

- ...:

  Ignored.

## Value

An object of class `flow_certificate`, a list with elements:

- `certified_optimal` - logical, the conclusion. `TRUE` only when every
  condition below holds.

- `primal_feasible` - logical; every arc inside its bounds and every
  node's net flow equal to its supply.

- `n_capacity_violations`, `n_conservation_violations`,
  `max_conservation_error` - what primal feasibility failed on.

- `dual_feasible` - logical; no arc that can still take flow prices
  below its own `-tol(a)`.

- `complementary_slackness` - logical; no arc above its lower bound
  prices above its own `tol(a)`.

- `n_cs_violations`, `min_residual_reduced_cost`, `worst_arc` - the
  smallest reduced cost over the residual graph and the arc attaining
  it, which is a violation when it falls below that arc's `-tol(a)`.
  `worst_arc` is 0 when no arc can either take or give up flow.

- `dual_tolerance` - the widest `tol(a)` any comparison was made
  against, so the verdict names the resolution it was reached at.

- `primal_objective`, `dual_objective`, `duality_gap` - numeric.

- `arithmetic` - `"exact"` or `"double"`, the arithmetic the conclusion
  was reached in.

- `exact_available`, `exact_certificate` - whether the exact question
  was asked, and whether its conditions hold.

- `n_exact_violations` - residual arcs whose reduced cost has the wrong
  sign exactly, under the potentials given.

- `exact_potentials_source` - which potentials decided the exact
  certificate: `"solver"`, `"supplied"`, `"recovered"`, or `"none"`.

- `exact_potential` - on an exact certificate, the node potentials that
  decided it, one row per node whose sum is its exact value; `NULL` when
  there is no exact certificate.

- `tolerance` - the relative `tol` as supplied.

Invisibly returns `x`.

## Details

The conditions are decided in one of two arithmetics, as they are by
[`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md),
and the certificate says which. The **exact** reading decides the sign
of every residual arc's reduced cost with no tolerance: the flow is
integral, so conservation and the bounds are exact already, and the
duality gap, being the sum of the slackness violations weighted by
`|cbar|`, is then exactly zero. When the potentials supplied pass the
numerical reading but miss exactness, which on computed costs they
usually do by a unit in the last place, exact potentials are recovered
from the flow itself as the shortest-path distances of its residual
graph, held in exact multi-component arithmetic. A flow that is not
optimal has none, because its residual graph carries a negative cycle,
so the recovery cannot certify one, and potentials that fail even the
numerical reading certify nothing. The **numerical** reading makes the
comparisons within a relative tolerance and establishes optimality to
that tolerance.

The problem is


      min  sum_a cost(a) f(a)
      s.t. (flow out of v) - (flow into v) = supply(v)   at every node v
           lower(a) <= f(a) <= upper(a)                  on every arc a

and, writing `cbar(a) = cost(a) + pi(tail(a)) - pi(head(a))` for the
reduced cost under the potentials `pi`, the conditions checked are
primal feasibility, `cbar(a) >= -tol(a)` on every arc that can still
take flow, and `cbar(a) <= tol(a)` on every arc carrying more than its
lower bound. Their objective form is checked too: the duality gap is the
sum of the slackness violations weighted by `|cbar|`, so it is where a
violation too small to trip the per-arc tolerance still accumulates.

`tol` is relative. Each arc is compared against


      tol(a) = tol * max(1, |cost(a)|, |pi(tail(a))|, |pi(head(a))|)

because `cbar(a)` is computed from those three numbers, and its last
bits are worth the largest of them times the machine epsilon. A design
that stacks lexicographic tier weights, as
[`cardinality_match()`](https://gillescolling.com/couplr/reference/cardinality_match.md)
does to rank cardinality above balance above distance, reaches
potentials in the millions, where one unit in the last place is around
1e-9 and an exactly optimal flow cannot meet an absolute 1e-9. The scale
never falls below 1, so a problem whose costs and potentials are of
order 1 is checked against `tol` itself. The widest tolerance any
comparison used is reported as `dual_tolerance`.

The check needs potentials. If `x` carries them, as a solve result does,
they are used. Otherwise they are obtained by solving `problem`, which
costs a second solve. Either way they are verified, not trusted: an arc
priced below the tolerance makes the verification fail rather than pass.

Optimal potentials are shared by all optimal solutions of a linear
program, so a flow from one solver can be certified against potentials
from another.

## See also

[`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md),
[`solver_status_values()`](https://gillescolling.com/couplr/reference/solver_status_values.md)

## Examples

``` r
# Two supply nodes shipping to two demand nodes, stated directly as a flow.
prob <- list(
  n_nodes = 4,
  supply  = c(2, 1, -2, -1),
  arcs = data.frame(
    tail  = c(1, 1, 2, 2),
    head  = c(3, 4, 3, 4),
    lower = c(0, 0, 0, 0),
    upper = c(2, 2, 2, 2),
    cost  = c(1, 3, 2, 1)
  )
)

# Both of node 1's units go to node 3, node 2's unit goes to node 4.
verify_flow(c(2, 0, 0, 1), prob, potential = c(0, 0, 1, 1))

# The same flow against potentials that certify nothing.
verify_flow(c(2, 0, 0, 1), prob, potential = c(0, 0, 0, 0))
```
