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
