# Correctness + frame-count check for trace_gabow_tarjan.
# GT is defined on integer costs; we test integer-valued matrices primarily.
# Float costs are rounded internally; we still expect the trace's final matching
# to equal the Hungarian optimum on the rounded matrix.

suppressMessages(devtools::load_all(quiet = TRUE))

set.seed(0x1DEADB)

mat_int <- function(n, hi = 100) {
  matrix(sample(0:hi, n * n, replace = TRUE), n, n)
}

check_one <- function(label, cost, maximize = FALSE, tol = 1e-6) {
  # Reference: Hungarian on the same matrix the trace sees (already integer).
  ref <- assignment(cost, maximize = maximize, method = "hungarian")
  w <- lap_animate(cost, method = "gabow_tarjan", maximize = maximize)

  trace_final <- w$x$frames[[length(w$x$frames)]]
  trace_total <- w$x$meta$total_cost
  n_frames <- length(w$x$frames)

  cost_eq <- isTRUE(all.equal(ref$total_cost, trace_total, tolerance = tol))
  status <- if (cost_eq) "OK" else "FAIL"
  cat(sprintf(
    "  %-30s n=%3d  frames=%4d  ref_cost=%9.4g  trace_cost=%9.4g  diff=%9.2e  %s\n",
    label, nrow(cost), n_frames, ref$total_cost, trace_total,
    trace_total - ref$total_cost, status
  ))
  if (!cost_eq) {
    cat("    ref match  :", paste(ref$match, collapse = ", "), "\n")
    cat("    trace match:", paste(trace_final$matching, collapse = ", "), "\n")
  }
  invisible(cost_eq)
}

cat("=== trace_gabow_tarjan vs assignment(method = 'hungarian') oracle ===\n\n")

ok <- TRUE

# Tiny worked example
c0 <- matrix(c(4L, 2L, 5L,
               3L, 3L, 6L,
               7L, 5L, 4L), nrow = 3, byrow = TRUE)
ok <- check_one("tiny 3x3 worked", c0) && ok

# Integer squares
for (n in c(4, 6, 8, 10)) {
  ok <- check_one(sprintf("integer %dx%d hi=10", n, n), mat_int(n, 10)) && ok
}

# Larger cost range -> more bit phases
ok <- check_one("integer 5x5 hi=1000", mat_int(5, 1000)) && ok
ok <- check_one("integer 8x8 hi=10000", mat_int(8, 10000)) && ok

# All-equal costs (degenerate)
ok <- check_one("all-equal 4x4", matrix(7L, 4, 4)) && ok

# Negative costs (need shift)
ok <- check_one("negative 5x5", mat_int(5, 20) - 10) && ok

# Float costs - GT rounds internally. The trace's matching is the optimum for
# the *rounded* matrix; its reported total_cost uses the *original* floats.
# So the right check is: does the trace's matching equal the Hungarian
# optimum's matching on the rounded matrix? (Both should solve the same
# integer problem.)
cat("\n=== Float costs (GT rounds internally) ===\n")
set.seed(7)
cf <- matrix(runif(36, 0, 100), 6, 6)
cf_rounded <- round(cf)
ref_rounded <- assignment(cf_rounded, method = "hungarian")
w <- lap_animate(cf, method = "gabow_tarjan")
trace_matching <- w$x$frames[[length(w$x$frames)]]$matching
# Evaluate both matchings on the rounded matrix and compare costs.
trace_cost_on_rounded <- sum(cf_rounded[cbind(seq_len(6), trace_matching)])
cost_match_on_rounded <- isTRUE(all.equal(ref_rounded$total_cost,
                                          trace_cost_on_rounded))
cat(sprintf("  float 6x6 (compare on rounded): frames=%d ref=%g trace=%g  %s\n",
            length(w$x$frames),
            ref_rounded$total_cost, trace_cost_on_rounded,
            if (cost_match_on_rounded) "OK" else "FAIL"))
ok <- cost_match_on_rounded && ok

# Maximize
ok <- check_one("maximize 5x5 integer", mat_int(5, 20), maximize = TRUE) && ok

# Rectangular: error
rect_err <- tryCatch(
  lap_animate(mat_int(3)[, 1:2], method = "gabow_tarjan"),
  error = function(e) conditionMessage(e)
)
got_err <- is.character(rect_err) && grepl("square", rect_err)
cat(sprintf("  %-30s  %s\n",
            "rectangular expect-error",
            if (got_err) "OK" else "FAIL"))
ok <- got_err && ok

cat("\n", if (ok) "ALL OK" else "FAILURES PRESENT", "\n", sep = "")

# Frame breakdown for one mid-sized run
cat("\n=== Frame breakdown for integer 6x6 hi=100 ===\n")
set.seed(2)
c6 <- mat_int(6, 100)
w6 <- lap_animate(c6, method = "gabow_tarjan")
phase_counts <- table(vapply(w6$x$frames, `[[`, character(1), "phase"))
print(phase_counts)
cat("Total frames:", length(w6$x$frames), "\n")

out_html <- file.path("dev_notes", "trace_gabow_tarjan.html")
htmlwidgets::saveWidget(
  lap_animate(c6, method = "gabow_tarjan"),
  file = normalizePath(out_html, mustWork = FALSE),
  selfcontained = TRUE
)
cat("\nWrote", out_html, "\n")

invisible(NULL)
