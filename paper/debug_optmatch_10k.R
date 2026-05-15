## Probe: does optmatch::pairmatch survive at n=10k with the cap lifted?
suppressPackageStartupMessages({
  library(optmatch)
})
options(optmatch_max_problem_size = Inf)

set.seed(20260515 + 10000)
n_t <- 3333; n_c <- 6667
make_group <- function(n, shift) {
  data.frame(
    v1 = rnorm(n,  0.30 * shift, 1),
    v2 = rnorm(n,  0.10 * shift, 1),
    v3 = rnorm(n,  0.40 * shift, 1),
    v4 = rnorm(n,  0.20 * shift, 1),
    v5 = rnorm(n,  0.15 * shift, 1),
    v6 = rnorm(n,  0.05 * shift, 1),
    b1 = rbinom(n, 1, 0.40 + 0.10 * shift),
    b2 = rbinom(n, 1, 0.30 - 0.05 * shift)
  )
}
tr <- make_group(n_t, 1); tr$treat <- 1L
ct <- make_group(n_c, 0); ct$treat <- 0L
d <- rbind(tr, ct)

covars <- c("v1","v2","v3","v4","v5","v6","b1","b2")
form <- as.formula(paste("treat ~", paste(covars, collapse = " + ")))

cat("Calling optmatch::pairmatch at n=10000 ...\n"); flush.console()
t0 <- proc.time()[["elapsed"]]
res <- tryCatch(
  pairmatch(form, data = d, controls = 1),
  error = function(e) {
    cat("ERROR after", round(proc.time()[["elapsed"]] - t0, 1), "s:",
        conditionMessage(e), "\n")
    return(NULL)
  }
)
t1 <- proc.time()[["elapsed"]]
if (!is.null(res)) {
  cat("OK in", round(t1 - t0, 1), "s\n")
} else {
  cat("Returned NULL\n")
}
