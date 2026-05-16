## Phase 7 benchmark: flat bucket array in hungarian_search_cl.
##
## Same shape as the running results table in gt-speedup.md: dense uniform
## random integer costs, max_cost = 1e5, square sizes n in {64, 128, 256,
## 384, 512}. Five trials per size, JV as reference.
##
## Run from package root:
##   "C:/Program Files/R/R-4.6.0/bin/Rscript.exe" dev_notes/phase7_bench.R

suppressPackageStartupMessages({
  devtools::load_all(".", quiet = TRUE, recompile = TRUE)
  library(microbenchmark)
})

bench_one <- function(n, times, max_cost = 1e5L) {
  set.seed(1000L + n)
  cost <- matrix(sample.int(max_cost, n * n, replace = TRUE), n, n)
  invisible(couplr:::lap_solve_gabow_tarjan(cost, maximize = FALSE))
  invisible(couplr:::lap_solve_jv(cost, maximize = FALSE))

  mb <- microbenchmark(
    gabow_tarjan = couplr:::lap_solve_gabow_tarjan(cost, maximize = FALSE),
    jv           = couplr:::lap_solve_jv(cost, maximize = FALSE),
    times = times,
    unit = "ms",
    control = list(warmup = 2L)
  )
  s <- summary(mb)
  data.frame(
    n = n,
    method = s$expr,
    min_ms = s$min,
    median_ms = s$median,
    mean_ms = s$mean,
    max_ms = s$max,
    stringsAsFactors = FALSE
  )
}

sizes <- c(64L, 128L, 256L, 384L, 512L)
results <- do.call(rbind, lapply(sizes, bench_one, times = 20L))
print(results, row.names = FALSE, digits = 4)

med_gt <- results[results$method == "gabow_tarjan", c("n", "median_ms")]
med_jv <- results[results$method == "jv", c("n", "median_ms")]
ratio <- med_gt$median_ms / med_jv$median_ms
cat("\nGT/JV ratios at each n:\n")
print(data.frame(n = med_gt$n, gt_ms = med_gt$median_ms,
                 jv_ms = med_jv$median_ms, ratio = ratio),
      row.names = FALSE, digits = 4)

fit <- lm(log(med_gt$median_ms) ~ log(med_gt$n))
cat(sprintf("\nGT slope over n: %.3f\n", coef(fit)[[2L]]))
