
# couplr <img src="man/figures/logo.svg" align="right" height="139" alt="couplr logo" />

*matching two groups into pairs*

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/couplr)](https://CRAN.R-project.org/package=couplr)
[![R-CMD-check](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gcol33/couplr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/gcol33/couplr/graph/badge.svg?flag=r)](https://app.codecov.io/gh/gcol33/couplr)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

couplr pairs the units of two groups, such as treated and control
patients, plots, sites or samples, so that the total covariate distance
over all pairs is as small as possible.

The pairing is a linear assignment problem: the distances between every
unit of one group and every unit of the other form a cost matrix, and
the solver picks one partner per unit so that the summed distance is
minimal. couplr solves it exactly in C++, by default with the
Jonker-Volgenant algorithm, so the total distance is the global minimum
and a solve can be certified optimal. You then check covariate balance
on the matched pairs and decide whether to use them as they are or to
tighten the match with calipers and blocks.

couplr supports:

- **Large pools.** `memory_mode = "lazy"` computes each distance when
  the solver needs it, and `memory_mode = "implicit"` generates only the
  pairs the optimum needs, so neither builds the full distance matrix.
- **Constraints and strata.** Calipers, a maximum distance, matching
  within blocks and several controls per treated unit are arguments of
  [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.html).
- **Other matching designs.** Full matching, coarsened exact matching,
  cardinality matching, propensity score subclassification and
  propensity score matching take the same two-group input and return
  results the diagnostics understand.
- **Balance and sensitivity.**
  [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.html)
  reports standardized differences, variance ratios and
  Kolmogorov-Smirnov statistics, and
  [`sensitivity_analysis()`](https://gillescolling.com/couplr/reference/sensitivity_analysis.html)
  computes Rosenbaum bounds for unmeasured confounding.
- **Analysis with other matching packages.** `as_matchit()` converts a
  result to a [MatchIt](https://kosukeimai.github.io/MatchIt/) object,
  so [cobalt](https://ngreifer.github.io/cobalt/) balance tables and
  [marginaleffects](https://marginaleffects.com/) estimates run on it.
- **The assignment solvers.**
  [`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.html)
  takes a cost matrix directly and offers nineteen solvers by name, the
  k best assignments and batches of independent problems.

The solve has no random seed: the same input gives the same pairing on
every run. When several pairings tie at the minimum, which one comes
back can depend on the order of the rows. [Comparison with
Alternatives](https://gillescolling.com/couplr/articles/comparison.html)
sets couplr beside MatchIt,
[optmatch](https://github.com/markmfredrickson/optmatch) and
[designmatch](https://cran.r-project.org/package=designmatch).

## Installation

Install the released version from CRAN. Windows and macOS get a binary
package and need no build tools:

``` r
install.packages("couplr")
```

The development version compiles C++ code. Install the build tools for
your operating system first:

| Operating system | Build tools |
|----|----|
| Windows | [Rtools](https://cran.r-project.org/bin/windows/Rtools/) matching your R version |
| macOS | Run `xcode-select --install` in Terminal |
| Debian / Ubuntu | Install `r-base-dev` |

Check that R can find the compiler:

``` r
pkgbuild::has_build_tools(debug = TRUE)
```

Then install couplr from GitHub:

``` r
# install.packages("pak")
pak::pak("gcol33/couplr")
```

The C++ code needs a compiler that supports C++17. It links only to
Rcpp, which is installed from CRAN with the package, and needs no system
libraries.

## Usage

1.  Match the two groups with `match_couples()`.
2.  Check covariate balance with `balance_diagnostics()`.
3.  Join the pairs to their covariates with `join_matched()` for the
    analysis.

The input is two data frames, one row per unit, each with an identifier
column (`id` by default) and the covariates named in `vars`.
`auto_scale = TRUE` puts covariates measured in different units on a
common scale before the distances are computed.

``` r
library(couplr)

set.seed(1)
treated <- data.frame(
  id = 1:6,
  age = round(rnorm(6, 45, 10)),
  income = round(rnorm(6, 55, 12))
)
control <- data.frame(
  id = 1:12,
  age = round(rnorm(12, 50, 12)),
  income = round(rnorm(12, 48, 15))
)

vars <- c("age", "income")
result <- match_couples(treated, control, vars = vars, auto_scale = TRUE)
#> Auto-selected scaling method: standardize
result
#> couplr result: one-to-one matching, optimal
#>   Left units:         6 matched of 6
#>   Right units:        6 matched of 12
#>   Pairs:              6
#>   Total distance:     6.44862
```

Each of the six treated units has one control partner, and six controls
stay unmatched. Balance is checked on the matched pairs:

``` r
bal <- balance_diagnostics(result, treated, control, vars = vars)
bal
```

`join_matched()` returns one row per pair, with the covariates of both
partners and their differences:

``` r
paired <- join_matched(result, treated, control)
paired
#> # A tibble: 6 × 10
#>   pair_id left_id right_id distance .age_diff .income_diff age_left income_left
#>     <int>   <int>    <int>    <dbl>     <dbl>        <dbl>    <dbl>       <dbl>
#> 1       1       1        1    0.453        -4            4       39          61
#> 2       2       2        9    1.39        -14           10       47          64
#> 3       3       3        2    1.64         14           15       37          62
#> 4       4       4        6    0.236         0           -3       61          51
#> 5       5       5        7    1.06        -12            5       48          73
#> 6       6       6       12    1.67         11           18       37          60
#> # ℹ 2 more variables: age_right <dbl>, income_right <dbl>
```

## Matching large pools

The exact solve applies to large pools as well.
`memory_mode = "implicit"` generates candidate pairs in rounds and stops
when the optimum needs no more of them; `method = "greedy"` gives up the
optimality guarantee for speed and keeps the same scaling, constraints
and blocking:

``` r
match_couples(treated, control, vars = vars, memory_mode = "implicit")
match_couples(treated, control, vars = vars, method = "greedy")
```

[Matching
Workflows](https://gillescolling.com/couplr/articles/matching-workflows.html)
compares optimal and greedy matching and the greedy strategies.

## Scaling and constraints

`scale` chooses how covariates are scaled (`"standardize"`, `"range"` or
`"robust"`). `max_distance` forbids pairs beyond a covariate distance,
`calipers` sets a maximum absolute difference per variable, `block_id`
names a column to match within, and `ratio` sets the number of controls
per treated unit:

``` r
match_couples(
  treated, control,
  vars = vars,
  scale = "robust",
  max_distance = 2,
  calipers = list(age = 5),
  ratio = 2
)
```

Treated units without a partner inside the constraints stay unmatched,
and the printed result counts them.

## Other matching designs

`full_match()` forms groups of variable size in which every unit is
used, `cem_match()` matches exactly on coarsened covariates, and
`cardinality_match()` maximizes the number of matched pairs subject to
balance constraints:

``` r
full_match(treated, control, vars = vars)
cem_match(treated, control, vars = vars)
cardinality_match(treated, control, vars = vars)
```

`ps_match()` and `subclass_match()` take a formula of the treatment
indicator on the covariates and a single data frame.
`sensitivity_analysis()` takes the name of an outcome column and reports
Rosenbaum bounds over a range of the sensitivity parameter gamma.

## The assignment solvers

`lap_solve()` takes a cost matrix, including rectangular ones and
forbidden pairs (`NA` or `Inf`). `method = "auto"` enumerates problems
up to 8 by 8, uses Hopcroft-Karp when the finite costs are all equal or
all 0 or 1, and uses Jonker-Volgenant otherwise:

``` r
cost <- matrix(c(4, 2, 8, 4, 3, 7, 3, 1, 6), nrow = 3, byrow = TRUE)

lap_solve(cost)
lap_solve(cost, method = "hungarian")
lap_solve_kbest(cost, k = 3)
```

`lap_solvers()` lists the nineteen solvers with their references, among
them Hungarian, Kuhn-Munkres, Bertsekas auction, Goldberg-Kennedy cost
scaling, Gabow-Tarjan bit scaling, push-relabel and network simplex.
`bottleneck_assignment()` minimizes the largest cost instead of the
total, and `sinkhorn()` returns an entropy-regularized transport plan.
[The Algorithm
Collection](https://gillescolling.com/couplr/articles/algorithms.html)
describes each solver.

`pixel_morph()` treats two images as source and target pixel sets and
morphs one into the other along the optimal assignment, which shows a
solve as an animation.

## Documentation

Start with [Quick
Start](https://gillescolling.com/couplr/articles/getting-started.html):
a first match, scaling, balance and the joined pairs.

- [Matching
  Workflows](https://gillescolling.com/couplr/articles/matching-workflows.html):
  preprocessing, calipers, blocking, balance diagnostics and a
  treatment-effect estimate from start to end.
- [The Algorithm
  Collection](https://gillescolling.com/couplr/articles/algorithms.html):
  the assignment problem and how each solver works.
- [Comparison with
  Alternatives](https://gillescolling.com/couplr/articles/comparison.html):
  couplr beside MatchIt, optmatch and designmatch.
- [Advanced
  Topics](https://gillescolling.com/couplr/articles/pixel-morphing.html):
  pixel morphing, approximation strategies for large problems and
  optimal transport.
- [Troubleshooting
  Guide](https://gillescolling.com/couplr/articles/troubleshooting.html):
  infeasible problems, poor balance, memory and installation errors.
- [Full reference](https://gillescolling.com/couplr/reference/): every
  function.

## Getting help

Questions and bug reports go to the [GitHub
issues](https://github.com/gcol33/couplr/issues). A bug report is
easiest to act on with a small reproducible example, for instance made
with [reprex](https://reprex.tidyverse.org/).

## Contributing

Pull requests are welcome; see the [contributing
guide](https://gillescolling.com/couplr/CONTRIBUTING.html).

## Support

> “Software is like sex: it’s better when it’s free.” — Linus Torvalds

I’m a PhD student who builds R packages in my free time because I
believe good tools should be free and open. I started these projects for
my own work and figured others might find them useful too.

If this package saved you some time, buying me a coffee is a nice way to
say thanks. It helps with my coffee addiction.

[![Buy Me A
Coffee](https://img.shields.io/badge/-Buy%20me%20a%20coffee-FFDD00?logo=buymeacoffee&logoColor=black)](https://buymeacoffee.com/gcol33)

## License

couplr is licensed under MIT.

## Citation

To cite couplr in publications use:

``` text
Colling G (2026). _couplr: Optimal Pairing and Matching via Linear
Assignment_. doi:10.32614/CRAN.package.couplr
<https://doi.org/10.32614/CRAN.package.couplr>. R package version
1.9.0, <https://gillescolling.com/couplr/>.
```

BibTeX:

``` bibtex
@Manual{colling2026couplr,
  title = {couplr: Optimal Pairing and Matching via Linear Assignment},
  author = {Gilles Colling},
  year = {2026},
  note = {R package version 1.9.0},
  doi = {10.32614/CRAN.package.couplr},
  url = {https://gillescolling.com/couplr/},
}
```

Download the citation as
[BibTeX](https://gillescolling.com/couplr/couplr.bib) or
[RIS](https://gillescolling.com/couplr/couplr.ris).
