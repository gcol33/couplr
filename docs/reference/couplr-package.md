# couplr: Optimal Pairing and Matching via Linear Assignment

Solves optimal pairing and matching problems using linear assignment
algorithms. Provides implementations of the Hungarian method (Kuhn 1955)
[doi:10.1002/nav.3800020109](https://doi.org/10.1002/nav.3800020109) ,
Jonker-Volgenant shortest path algorithm (Jonker and Volgenant 1987)
[doi:10.1007/BF02278710](https://doi.org/10.1007/BF02278710) , Auction
algorithm (Bertsekas 1988)
[doi:10.1007/BF02186476](https://doi.org/10.1007/BF02186476) ,
cost-scaling (Goldberg and Kennedy 1995)
[doi:10.1007/BF01585996](https://doi.org/10.1007/BF01585996) , scaling
algorithms (Gabow and Tarjan 1989)
[doi:10.1137/0218069](https://doi.org/10.1137/0218069) , push-relabel
(Goldberg and Tarjan 1988)
[doi:10.1145/48014.61051](https://doi.org/10.1145/48014.61051) , and
Sinkhorn entropy-regularized transport (Cuturi 2013)
[doi:10.48550/arxiv.1306.0895](https://doi.org/10.48550/arxiv.1306.0895)
. Designed for matching plots, sites, samples, or any pairwise
optimization problem. Supports rectangular matrices, forbidden
assignments, data frame inputs, batch solving, k-best solutions, and
pixel-level image morphing for visualization. Includes automatic
preprocessing with variable health checks, multiple scaling methods
(standardized, range, robust), greedy matching algorithms, and
comprehensive balance diagnostics for assessing match quality using
standardized differences and distribution comparisons.

Solves optimal pairing and matching problems using linear assignment
algorithms. Designed for matching plots, sites, samples, or any pairwise
optimization problem. Provides modern, tidy implementations of
'Hungarian', 'Jonker-Volgenant', 'Auction', and other LAP solvers.

## Main functions

- [`lap_solve`](https://gillescolling.com/couplr/reference/lap_solve.md):
  Solve single assignment problems

- [`lap_solve_batch`](https://gillescolling.com/couplr/reference/lap_solve_batch.md):
  Solve multiple problems efficiently

- [`lap_solve_kbest`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md):
  Find k-best optimal solutions

## See also

Useful links:

- <https://gillescolling.com/couplr/>

- <https://github.com/gcol33/couplr>

- Report bugs at <https://github.com/gcol33/couplr/issues>

## Author

**Maintainer**: Gilles Colling <gilles.colling051@gmail.com>
([ORCID](https://orcid.org/0000-0003-3070-6066)) \[copyright holder\]
