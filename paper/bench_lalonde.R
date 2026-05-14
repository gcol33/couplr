## Head-to-head: couplr vs MatchIt vs optmatch on the Lalonde NSW data.
## Writes paper/lalonde-results.csv with both a per-package summary and the
## per-covariate |SMD| breakdown used in the paper.
##
## Reproducible via:  Rscript paper/bench_lalonde.R

suppressPackageStartupMessages({
  if (!requireNamespace("MatchIt",        quietly = TRUE)) install.packages("MatchIt",        repos = "https://cloud.r-project.org")
  if (!requireNamespace("optmatch",       quietly = TRUE)) install.packages("optmatch",       repos = "https://cloud.r-project.org")
  if (!requireNamespace("cobalt",         quietly = TRUE)) install.packages("cobalt",         repos = "https://cloud.r-project.org")
  if (!requireNamespace("microbenchmark", quietly = TRUE)) install.packages("microbenchmark", repos = "https://cloud.r-project.org")

  library(MatchIt)
  library(optmatch)
  library(cobalt)
  library(microbenchmark)
  pkgload::load_all("C:/GillesC/Documents/dev/couplr", quiet = TRUE)
})

set.seed(20260515)

paper_dir <- "C:/GillesC/Documents/dev/couplr/paper"

## ---- data: Lalonde NSW, with the standard MatchIt formula ----
data("lalonde", package = "MatchIt")
## one-hot race so all three packages match on the same numeric design
lalonde$race_black  <- as.integer(lalonde$race == "black")
lalonde$race_hispan <- as.integer(lalonde$race == "hispan")
covars <- c("age", "educ", "race_black", "race_hispan",
            "married", "nodegree", "re74", "re75")
form   <- as.formula(paste("treat ~", paste(covars, collapse = " + ")))

## ---- 1) couplr: Mahalanobis distance, optimal 1:1 ----
treated <- subset(lalonde, treat == 1); treated$id <- seq_len(nrow(treated))
control <- subset(lalonde, treat == 0); control$id <- seq_len(nrow(control))

couplr_call <- function() {
  match_couples(
    left     = treated,
    right    = control,
    vars     = covars,
    distance = "mahalanobis"
  )
}

## ---- 2) MatchIt optimal (uses optmatch backend) ----
matchit_call <- function() {
  matchit(form, data = lalonde, method = "optimal", distance = "mahalanobis", ratio = 1)
}

## ---- 3) optmatch direct ----
optmatch_call <- function() {
  pairmatch(form, data = lalonde, controls = 1)
}

cat("warm-up ...\n"); flush.console()
invisible(couplr_call()); invisible(matchit_call()); invisible(optmatch_call())

cat("microbenchmark (5 reps each) ...\n"); flush.console()
mb <- microbenchmark(
  couplr   = couplr_call(),
  MatchIt  = matchit_call(),
  optmatch = optmatch_call(),
  times = 5L,
  unit  = "ms"
)
mb_summary <- summary(mb)
print(mb_summary)

## ---- balance on the *matched* sample for each ----
couplr_res  <- couplr_call()
matchit_res <- matchit_call()
opt_res     <- optmatch_call()

## couplr: extract matched pairs
couplr_pairs <- couplr_res$pairs
couplr_pairs <- couplr_pairs[couplr_pairs[[2]] > 0, ]
left_m   <- merge(couplr_pairs[, 1, drop = FALSE], treated[, c("id", covars)],
                  by.x = names(couplr_pairs)[1], by.y = "id")
right_m  <- merge(couplr_pairs[, 2, drop = FALSE], control[, c("id", covars)],
                  by.x = names(couplr_pairs)[2], by.y = "id")

## Standardize SMD using the *treated-group* SD on the unmatched data
## (Stuart 2010 convention; what cobalt does by default).
treated_sd <- vapply(covars, function(v) sd(lalonde[lalonde$treat == 1, v], na.rm = TRUE), numeric(1))

smd_couplr <- vapply(covars, function(v) {
  l <- as.numeric(left_m[[v]]);  r <- as.numeric(right_m[[v]])
  (mean(l, na.rm = TRUE) - mean(r, na.rm = TRUE)) / treated_sd[v]
}, numeric(1))

## MatchIt: compute SMD with the SAME manual formula on the matched pairs
## so we are comparing matching quality, not balance-calc conventions.
mi_matched_idx <- which(!is.na(matchit_res$subclass))
mi_matched <- lalonde[mi_matched_idx, ]
smd_matchit <- vapply(covars, function(v) {
  x <- as.numeric(mi_matched[[v]]); t <- mi_matched$treat
  l <- x[t == 1]; r <- x[t == 0]
  (mean(l, na.rm = TRUE) - mean(r, na.rm = TRUE)) / treated_sd[v]
}, numeric(1))

## optmatch: pull matched pairs, compute SMD manually
opt_strata <- opt_res
keep <- !is.na(opt_strata)
matched_df <- lalonde[keep, ]
matched_df$strata <- opt_strata[keep]
smd_optmatch <- vapply(covars, function(v) {
  x <- as.numeric(matched_df[[v]])
  t  <- matched_df$treat
  l <- x[t == 1]; r <- x[t == 0]
  (mean(l, na.rm = TRUE) - mean(r, na.rm = TRUE)) / treated_sd[v]
}, numeric(1))

## pre-match SMDs (treated-group SD denominator)
smd_pre <- vapply(covars, function(v) {
  x <- as.numeric(lalonde[[v]]); t <- lalonde$treat
  l <- x[t == 1]; r <- x[t == 0]
  (mean(l, na.rm = TRUE) - mean(r, na.rm = TRUE)) / treated_sd[v]
}, numeric(1))

## ---- write results ----
results <- data.frame(
  package = c("couplr", "MatchIt", "optmatch"),
  median_ms = round(mb_summary$median, 1),
  max_abs_smd = c(max(abs(smd_couplr), na.rm = TRUE),
                  max(abs(smd_matchit), na.rm = TRUE),
                  max(abs(smd_optmatch), na.rm = TRUE)),
  mean_abs_smd = c(mean(abs(smd_couplr), na.rm = TRUE),
                   mean(abs(smd_matchit), na.rm = TRUE),
                   mean(abs(smd_optmatch), na.rm = TRUE))
)
results$median_ms    <- round(results$median_ms,    1)
results$max_abs_smd  <- round(results$max_abs_smd,  3)
results$mean_abs_smd <- round(results$mean_abs_smd, 3)

print(results)
write.csv(results, file.path(paper_dir, "lalonde-results.csv"), row.names = FALSE)

## Per-covariate SMDs, for sanity checking
cat("\nPer-covariate |SMD| after matching:\n")
per_cov <- data.frame(
  covar     = covars,
  unmatched = round(abs(smd_pre),      3),
  couplr    = round(abs(smd_couplr),   3),
  MatchIt   = round(abs(smd_matchit),  3),
  optmatch  = round(abs(smd_optmatch), 3)
)
print(per_cov, row.names = FALSE)
write.csv(per_cov, file.path(paper_dir, "lalonde-per-covariate.csv"),
          row.names = FALSE)

cat("\nDone. Wrote:\n  ", file.path(paper_dir, "lalonde-results.csv"),
    "\n  ", file.path(paper_dir, "lalonde-per-covariate.csv"), "\n")
