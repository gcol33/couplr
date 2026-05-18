# Correctness + frame-count check for trace_jv.
# Compares the trace's final matching/cost to the Hungarian production solver.

suppressMessages(devtools::load_all(quiet = TRUE))

set.seed(0xCAFE)

mat_cost <- function(rows, cols = rows, lo = 0, hi = 100, integers = FALSE) {
  v <- runif(rows * cols, lo, hi)
  if (integers) v <- floor(v)
  matrix(v, rows, cols)
}

check_one <- function(label, cost, maximize = FALSE, tol = 1e-6) {
  ref <- assignment(cost, maximize = maximize, method = "hungarian")
  w <- lap_animate(cost, method = "jv", maximize = maximize)
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

cat("=== trace_jv vs assignment(method = 'hungarian') oracle ===\n\n")

ok <- TRUE

# Tiny worked example
c0 <- matrix(c(4, 2, 5,
               3, 3, 6,
               7, 5, 4), nrow = 3, byrow = TRUE)
ok <- check_one("tiny 3x3 worked", c0) && ok

# Random squares (pre-stages fully exercised)
for (n in c(4, 6, 8, 12, 20, 40)) {
  ok <- check_one(sprintf("random %dx%d", n, n), mat_cost(n)) && ok
}

# Integer-valued
ok <- check_one("integer 8x8", mat_cost(8, integers = TRUE)) && ok
ok <- check_one("integer 15x15", mat_cost(15, integers = TRUE)) && ok

# Heavy ties (lots of equal costs - exercise reduction-transfer ties)
c_ties <- matrix(c(1, 1, 1,
                   1, 1, 1,
                   1, 2, 3), 3, 3, byrow = TRUE)
ok <- check_one("ties 3x3", c_ties) && ok

# All-equal
ok <- check_one("all-equal 4x4", matrix(5, 4, 4)) && ok

# Contention (single row preferred by everyone)
c_fight <- matrix(c( 1,  9,  9,  9,
                     1,  9,  9,  9,
                     9,  1,  9,  9,
                     9,  9,  1,  9), 4, 4, byrow = TRUE)
ok <- check_one("contention 4x4", c_fight) && ok

# Negative costs
ok <- check_one("negative 5x5", mat_cost(5, 5, -50, 50)) && ok

# Forbidden entries: pre-stages skipped, main loop only
cf <- mat_cost(6, 6)
cf[sample.int(36, 8)] <- NA
ok <- check_one("with NA forbidden 6x6", cf) && ok

# Maximize
ok <- check_one("maximize 5x5", mat_cost(5, 5), maximize = TRUE) && ok

# Rectangular: expect error
rect_err <- tryCatch(
  lap_animate(mat_cost(3, 7), method = "jv"),
  error = function(e) conditionMessage(e)
)
got_err <- is.character(rect_err) && grepl("square", rect_err)
cat(sprintf("  %-30s  %s\n",
            "rectangular expect-error",
            if (got_err) "OK" else "FAIL"))
ok <- got_err && ok

cat("\n", if (ok) "ALL OK" else "FAILURES PRESENT", "\n", sep = "")

# Frame breakdown for one mid-sized run
cat("\n=== Frame breakdown for random 8x8 (full pre-stages) ===\n")
set.seed(1)
c8 <- mat_cost(8)
w8 <- lap_animate(c8, method = "jv")
phase_counts <- table(vapply(w8$x$frames, `[[`, character(1), "phase"))
print(phase_counts)
cat("Total frames:", length(w8$x$frames), "\n")

cat("\n=== Frame breakdown for 6x6 with forbidden (main-loop only) ===\n")
set.seed(2)
cf2 <- mat_cost(6)
cf2[sample.int(36, 8)] <- NA
wf <- lap_animate(cf2, method = "jv")
phase_counts_f <- table(vapply(wf$x$frames, `[[`, character(1), "phase"))
print(phase_counts_f)
cat("Total frames:", length(wf$x$frames), "\n")

out_html <- file.path("dev_notes", "trace_jv.html")
htmlwidgets::saveWidget(
  lap_animate(c0, method = "jv"),
  file = normalizePath(out_html, mustWork = FALSE),
  selfcontained = TRUE
)
cat("\nWrote", out_html, "\n")

invisible(NULL)
