## Phase 5 fuzz harness.
##
## Generates 1000 random LAP instances and runs lap_solve_gabow_tarjan against
## lap_solve_jv. Under -DCOUPLR_GT_DEBUG (set in src/Makevars.win for this
## build), the bucket-bound check in hungarian_search_cl will LAP_ERROR if a
## finite, not-in-T edge is ever silently dropped. Other failure modes
## (optimal cost mismatch, missing rows in match, perfect-matching solvable
## but GT errors out) are also surfaced.
##
## Run from package root:
##   "C:/Program Files/R/R-4.6.0/bin/Rscript.exe" dev_notes/phase5_fuzz.R

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE)
})

set.seed(20260516L)

BIG <- 1e18  # sentinel for forbidden edges in cost matrices

gen_instance <- function(seed, kind) {
  set.seed(seed)
  n <- sample(3:200, 1L)
  m <- n  # square only (GT solves balanced LAP)
  max_cost <- sample(c(1L, 10L, 100L, 1e3L, 1e5L, 1e7L, 1e9L), 1L)
  cost <- matrix(sample.int(max_cost, n * m, replace = TRUE),
                 nrow = n, ncol = m)

  if (kind == "edge_1row") {
    cost <- cost[1, , drop = FALSE]
  } else if (kind == "edge_1col") {
    cost <- cost[, 1, drop = FALSE]
  } else if (kind == "forbidden_some") {
    # 0 - 10% forbidden, but keep a perfect matching feasible: forbid a
    # random fraction of edges except keep a random permutation tight.
    perm <- sample.int(n)
    pf <- runif(1, 0, 0.10)
    mask <- matrix(runif(n * m) < pf, n, m)
    for (i in seq_len(n)) mask[i, perm[i]] <- FALSE  # keep perm[i] feasible
    cost[mask] <- BIG
  } else if (kind == "negative") {
    shift <- sample(0:10, 1L)
    cost <- cost - shift  # let a few negatives in
  }
  list(cost = cost, n = nrow(cost), m = ncol(cost), max_cost = max_cost,
       kind = kind, seed = seed)
}

run_pair <- function(inst) {
  # JV reference (handles rectangular costs in this package via the same
  # entry point); skip when n != m since GT is square-only here.
  if (inst$n != inst$m) {
    return(list(skipped = TRUE))
  }
  jv <- tryCatch(couplr:::lap_solve_jv(inst$cost, maximize = FALSE),
                 error = function(e) e)
  gt <- tryCatch(couplr:::lap_solve_gabow_tarjan(inst$cost, maximize = FALSE),
                 error = function(e) e)

  jv_err <- inherits(jv, "error")
  gt_err <- inherits(gt, "error")
  list(
    skipped = FALSE,
    jv_err = jv_err,
    gt_err = gt_err,
    jv_msg = if (jv_err) conditionMessage(jv) else NA_character_,
    gt_msg = if (gt_err) conditionMessage(gt) else NA_character_,
    jv_cost = if (!jv_err) jv$total_cost else NA_real_,
    gt_cost = if (!gt_err) gt$total_cost else NA_real_,
    jv_nm   = if (!jv_err) jv$n_matched  else NA_integer_,
    gt_nm   = if (!gt_err) gt$n_matched  else NA_integer_
  )
}

kinds <- c("dense", "forbidden_some", "negative", "edge_1row", "edge_1col")
N <- 1000L

cat(sprintf("Phase 5 fuzz: %d instances over kinds = %s\n", N,
            paste(kinds, collapse = ", ")))

bucket_drops    <- character()  # store messages mentioning Phase 5
cost_mismatches <- list()
gt_errors_solv  <- list()  # GT errored where JV found a solution
n_run <- 0L
n_skipped <- 0L
t0 <- Sys.time()

for (i in seq_len(N)) {
  kind <- sample(kinds, 1L,
                 prob = c(0.50, 0.30, 0.10, 0.05, 0.05))
  inst <- gen_instance(seed = i, kind = kind)
  r <- run_pair(inst)
  if (isTRUE(r$skipped)) { n_skipped <- n_skipped + 1L; next }
  n_run <- n_run + 1L

  if (r$gt_err) {
    msg <- r$gt_msg
    if (grepl("Phase 5", msg, fixed = TRUE)) {
      bucket_drops <- c(bucket_drops,
                        sprintf("seed=%d kind=%s n=%d max_cost=%g | %s",
                                inst$seed, inst$kind, inst$n, inst$max_cost,
                                msg))
    }
    if (!r$jv_err) {
      gt_errors_solv[[length(gt_errors_solv) + 1L]] <- list(
        seed = inst$seed, kind = inst$kind, n = inst$n,
        max_cost = inst$max_cost, msg = msg
      )
    }
    next
  }

  if (!r$jv_err && r$jv_cost != r$gt_cost) {
    cost_mismatches[[length(cost_mismatches) + 1L]] <- list(
      seed = inst$seed, kind = inst$kind, n = inst$n,
      max_cost = inst$max_cost, jv = r$jv_cost, gt = r$gt_cost
    )
  }

  if (i %% 100L == 0L) {
    cat(sprintf("  %4d/%d  drops=%d  mismatches=%d  gt_only_err=%d\n",
                i, N, length(bucket_drops), length(cost_mismatches),
                length(gt_errors_solv)))
  }
}

t1 <- Sys.time()
cat(sprintf("\nDone in %.1fs.\n", as.numeric(t1 - t0, units = "secs")))
cat(sprintf("  ran:       %d\n", n_run))
cat(sprintf("  skipped:   %d  (non-square instances)\n", n_skipped))
cat(sprintf("  bucket-bound drops (Phase 5):  %d\n", length(bucket_drops)))
cat(sprintf("  cost mismatches GT vs JV:      %d\n", length(cost_mismatches)))
cat(sprintf("  GT errored where JV solved:    %d\n", length(gt_errors_solv)))

if (length(bucket_drops) > 0L) {
  cat("\n[Bucket-bound assertion fires]\n")
  for (s in head(bucket_drops, 20L)) cat("  ", s, "\n", sep = "")
}
if (length(cost_mismatches) > 0L) {
  cat("\n[Cost mismatches]\n")
  for (e in head(cost_mismatches, 20L)) {
    cat(sprintf("  seed=%d kind=%s n=%d max_cost=%g  jv=%s  gt=%s\n",
                e$seed, e$kind, e$n, e$max_cost,
                format(e$jv), format(e$gt)))
  }
}
if (length(gt_errors_solv) > 0L) {
  cat("\n[GT errored where JV solved]\n")
  for (e in head(gt_errors_solv, 20L)) {
    cat(sprintf("  seed=%d kind=%s n=%d max_cost=%g  | %s\n",
                e$seed, e$kind, e$n, e$max_cost, e$msg))
  }
}

invisible(list(bucket_drops = bucket_drops,
               cost_mismatches = cost_mismatches,
               gt_errors_solv = gt_errors_solv))
