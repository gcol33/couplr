## Shared synthetic-problem recipe for the paper's scaling benchmarks.
## Every reported size is generated here, so the figures, the scaling table and
## the lazy-path timings all describe the same eight-covariate problem.

## 6 continuous (age-like, educ-like, two-earnings-like, two extras),
## 2 binary (race / nodegree-like). Treated shifted slightly to make
## the matching problem non-degenerate. Treated:control = 1:2.
make_data <- function(n_total, seed) {
  set.seed(seed)
  n_t <- round(n_total / 3)
  n_c <- n_total - n_t
  make_group <- function(n, shift) {
    data.frame(
      v1 = rnorm(n, 0.30 * shift, 1),
      v2 = rnorm(n, 0.10 * shift, 1),
      v3 = rnorm(n, 0.40 * shift, 1),
      v4 = rnorm(n, 0.20 * shift, 1),
      v5 = rnorm(n, 0.15 * shift, 1),
      v6 = rnorm(n, 0.05 * shift, 1),
      b1 = rbinom(n, 1, 0.40 + 0.10 * shift),
      b2 = rbinom(n, 1, 0.30 - 0.05 * shift)
    )
  }
  treated <- make_group(n_t, shift = 1); treated$treat <- 1L
  control <- make_group(n_c, shift = 0); control$treat <- 0L
  d <- rbind(treated, control)
  d$id <- seq_len(nrow(d))
  d
}

## Seed is derived from the size, so a given size always yields the same data
## whichever script generates it.
bench_seed <- function(n_total) 20260515 + n_total

covars <- c("v1", "v2", "v3", "v4", "v5", "v6", "b1", "b2")
form   <- as.formula(paste("treat ~", paste(covars, collapse = " + ")))

## ============================================================================
## Cost-matrix regimes for the solver grid
## ============================================================================
## The scaling problem above is one regime: square-ish, dense, metric, eight
## covariates. The solver rules the package exposes are stated over properties
## that problem does not vary, so the grid needs problems that do. A regime is
## a generator plus the properties the rules read, declared once here and used
## by every script that needs them, so a new regime is a list entry rather than
## a branch in a benchmark loop.

## Squared-distance identity, so a metric regime costs one crossproduct rather
## than n * m calls. Negative entries are rounding, not distance.
cross_dist <- function(A, B) {
  d2 <- outer(rowSums(A^2), rowSums(B^2), "+") - 2 * tcrossprod(A, B)
  sqrt(pmax(d2, 0))
}

## `gen(n, m)` draws from the ambient RNG, which the caller seeds, so an
## instance is reproducible from its seed alone.
cost_regimes <- list(
  int_uniform = list(
    label = "integer uniform",
    cost_type = "integer", distribution = "uniform", metric = FALSE,
    gen = function(n, m) matrix(sample.int(10000L, n * m, replace = TRUE), n, m)
  ),
  dbl_uniform = list(
    label = "double uniform",
    cost_type = "double", distribution = "uniform", metric = FALSE,
    gen = function(n, m) matrix(runif(n * m), n, m)
  ),
  binary = list(
    label = "binary",
    cost_type = "binary", distribution = "binary", metric = FALSE,
    gen = function(n, m) matrix(sample(0:1, n * m, replace = TRUE), n, m)
  ),
  constant = list(
    label = "constant",
    cost_type = "constant", distribution = "degenerate", metric = FALSE,
    gen = function(n, m) matrix(1, n, m)
  ),
  tied = list(
    label = "five distinct costs",
    cost_type = "integer", distribution = "tied", metric = FALSE,
    gen = function(n, m) matrix(sample.int(5L, n * m, replace = TRUE), n, m)
  ),
  heavy_tailed = list(
    label = "lognormal",
    cost_type = "double", distribution = "heavy-tailed", metric = FALSE,
    gen = function(n, m) matrix(rlnorm(n * m, 0, 3), n, m)
  ),
  metric_uniform = list(
    label = "euclidean, uniform points",
    cost_type = "double", distribution = "uniform", metric = TRUE,
    gen = function(n, m) {
      d <- 8L
      cross_dist(matrix(runif(n * d), n, d), matrix(runif(m * d), m, d))
    }
  ),
  metric_clustered = list(
    label = "euclidean, clustered points",
    cost_type = "double", distribution = "clustered", metric = TRUE,
    gen = function(n, m) {
      d <- 8L; k <- 8L
      centres <- matrix(rnorm(k * d, 0, 3), k, d)
      draw <- function(q) centres[sample.int(k, q, replace = TRUE), , drop = FALSE] +
        matrix(rnorm(q * d, 0, 0.5), q, d)
      cross_dist(draw(n), draw(m))
    }
  )
)

## A forbidden pattern is what makes the finite entries a graph rather than a
## matrix. Every pattern leaves at least one complete matching in place, so a
## cell that comes back infeasible is a defect and not the design.
forbidden_patterns <- list(
  none = list(
    label = "complete", finite_share = 1, components = 1L,
    apply = function(cost, n, m) cost
  ),
  random_60 = list(
    label = "60% finite, unstructured", finite_share = 0.6, components = 1L,
    apply = function(cost, n, m) .forbid_random(cost, n, m, 0.6)
  ),
  random_25 = list(
    label = "25% finite, unstructured", finite_share = 0.25, components = 1L,
    apply = function(cost, n, m) .forbid_random(cost, n, m, 0.25)
  ),
  block_4 = list(
    label = "four forbidden components", finite_share = 0.25, components = 4L,
    apply = function(cost, n, m) .forbid_blocks(cost, n, m, 4L)
  )
)

## One complete matching is kept finite by construction, so feasibility does not
## depend on the draw. `n <= m` throughout: the grid generates rows against at
## least as many columns.
.forbid_random <- function(cost, n, m, finite_share) {
  keep <- matrix(runif(n * m) < finite_share, n, m)
  keep[cbind(seq_len(n), sample.int(m, n))] <- TRUE
  cost[!keep] <- Inf
  cost
}

## Contiguous blocks: rows and columns are cut into `k` groups of equal size and
## only same-group entries stay finite, which is `k` disconnected components.
## Costs are drawn independently of position, so contiguity is a labelling and
## not a structure the solvers can exploit.
.forbid_blocks <- function(cost, n, m, k) {
  rblk <- pmin(k, ceiling(seq_len(n) / (n / k)))
  cblk <- pmin(k, ceiling(seq_len(m) / (m / k)))
  cost[outer(rblk, cblk, "!=")] <- Inf
  cost
}

## One cell of the solver grid: the matrix, and the properties it was built to
## have. `seed` alone reproduces it.
make_cost_problem <- function(regime, n_rows, n_cols, pattern, seed) {
  reg <- cost_regimes[[regime]]
  pat <- forbidden_patterns[[pattern]]
  if (is.null(reg)) stop("unknown regime: ", regime)
  if (is.null(pat)) stop("unknown forbidden pattern: ", pattern)
  if (n_rows > n_cols) stop("the grid generates rows against at least as many columns")
  set.seed(seed)
  cost <- pat$apply(reg$gen(n_rows, n_cols), n_rows, n_cols)
  list(
    cost = cost,
    regime = regime, pattern = pattern,
    n_rows = n_rows, n_cols = n_cols,
    cost_type = reg$cost_type, distribution = reg$distribution,
    metric = reg$metric, components = pat$components,
    finite_share = mean(is.finite(cost))
  )
}

## ============================================================================
## Point clouds for the edge-generation grid
## ============================================================================
## The loop is not given a cost matrix, it is given units and a metric, so its
## grid varies the cloud rather than the matrix. Treated:control is 1:2, as it
## is in `make_data()`, and the covariate count is a factor rather than a
## constant.

point_clouds <- list(
  gaussian = list(
    label = "independent normal, treated shifted",
    gen = function(n_t, n_c, d) list(
      left  = matrix(rnorm(n_t * d, 0.3, 1), n_t, d),
      right = matrix(rnorm(n_c * d, 0, 1), n_c, d)
    )
  ),
  clustered = list(
    label = "eight shared clusters",
    gen = function(n_t, n_c, d) {
      k <- 8L
      centres <- matrix(rnorm(k * d, 0, 3), k, d)
      draw <- function(q) centres[sample.int(k, q, replace = TRUE), , drop = FALSE] +
        matrix(rnorm(q * d, 0, 0.5), q, d)
      list(left = draw(n_t), right = draw(n_c))
    }
  ),
  shifted = list(
    label = "treated displaced from the control cloud",
    gen = function(n_t, n_c, d) list(
      left  = matrix(rnorm(n_t * d, 1.5, 1), n_t, d),
      right = matrix(rnorm(n_c * d, 0, 1), n_c, d)
    )
  ),
  lattice_ties = list(
    label = "coordinates on a coarse lattice",
    gen = function(n_t, n_c, d) list(
      left  = round(matrix(rnorm(n_t * d, 0.3, 1), n_t, d) * 4) / 4,
      right = round(matrix(rnorm(n_c * d, 0, 1), n_c, d) * 4) / 4
    )
  ),
  heavy_tailed = list(
    label = "t on two degrees of freedom",
    gen = function(n_t, n_c, d) list(
      left  = matrix(rt(n_t * d, df = 2), n_t, d) + 0.3,
      right = matrix(rt(n_c * d, df = 2), n_c, d)
    )
  ),
  ## Every treated unit's nearest controls are the same small core, so the
  ## nearest-neighbour seed hands the master a graph it cannot match inside and
  ## the loop has to price its way out into the halo.
  contested = list(
    label = "one contested core, diffuse halo",
    gen = function(n_t, n_c, d) {
      n_core <- max(1L, round(0.02 * n_c))
      core <- matrix(rnorm(n_core * d, 0, 0.05), n_core, d)
      halo <- matrix(rnorm((n_c - n_core) * d, 0, 3), n_c - n_core, d)
      list(left = matrix(rnorm(n_t * d, 0, 0.05), n_t, d),
           right = rbind(core, halo))
    }
  ),
  ## Controls on a shell around the treated cloud: every treated-control
  ## distance is nearly the same, so no seed of any width is informative and the
  ## pricing pass has little to prune with.
  shell = list(
    label = "controls on a shell of near-equal distance",
    gen = function(n_t, n_c, d) {
      unit <- function(q) {
        z <- matrix(rnorm(q * d), q, d)
        z / sqrt(rowSums(z^2))
      }
      list(left = matrix(rnorm(n_t * d, 0, 0.01), n_t, d),
           right = unit(n_c) * (1 + rnorm(n_c, 0, 0.001)))
    }
  )
)

## A cloud as the data frame `match_couples()` takes, with the covariate names
## it should be matched on.
make_cloud <- function(cloud, n_total, dim, seed) {
  cl <- point_clouds[[cloud]]
  if (is.null(cl)) stop("unknown cloud: ", cloud)
  set.seed(seed)
  n_t <- round(n_total / 3)
  n_c <- n_total - n_t
  pts <- cl$gen(n_t, n_c, dim)
  nm <- paste0("x", seq_len(dim))
  left <- as.data.frame(pts$left); names(left) <- nm
  right <- as.data.frame(pts$right); names(right) <- nm
  left$treat <- 1L; right$treat <- 0L
  left$id <- seq_len(n_t); right$id <- n_t + seq_len(n_c)
  list(left = left, right = right, vars = nm,
       cloud = cloud, dim = dim, n_total = n_total, seed = seed)
}
