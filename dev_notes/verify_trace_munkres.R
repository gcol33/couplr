# Correctness + frame-count check for trace_munkres.
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
  w <- lap_animate(cost, method = "munkres", maximize = maximize)
  trace_final <- w$x$frames[[length(w$x$frames)]]
  trace_total <- w$x$meta$total_cost
  n_frames <- length(w$x$frames)

  cost_eq <- isTRUE(all.equal(ref$total_cost, trace_total, tolerance = tol))
  status <- if (cost_eq) "OK" else "FAIL"
  cat(sprintf(
    "  %-30s n=%3d  frames=%5d  ref_cost=%9.4g  trace_cost=%9.4g  diff=%9.2e  %s\n",
    label, nrow(cost), n_frames, ref$total_cost, trace_total,
    trace_total - ref$total_cost, status
  ))
  if (!cost_eq) {
    cat("    ref match  :", paste(ref$match, collapse = ", "), "\n")
    cat("    trace match:", paste(trace_final$matching, collapse = ", "), "\n")
  }
  invisible(cost_eq)
}

cat("=== trace_munkres vs assignment(method = 'hungarian') oracle ===\n\n")

ok <- TRUE

# Tiny worked example
c0 <- matrix(c(4, 2, 5,
               3, 3, 6,
               7, 5, 4), nrow = 3, byrow = TRUE)
ok <- check_one("tiny 3x3 worked", c0) && ok

# Random squares
for (n in c(4, 6, 8, 10)) {
  ok <- check_one(sprintf("random %dx%d", n, n), mat_cost(n)) && ok
}

# Integer-valued
ok <- check_one("integer 5x5", mat_cost(5, integers = TRUE)) && ok
ok <- check_one("integer 8x8", mat_cost(8, integers = TRUE)) && ok

# Heavy ties (lots of equal costs)
c_ties <- matrix(c(1, 1, 1,
                   1, 1, 1,
                   1, 2, 3), 3, 3, byrow = TRUE)
ok <- check_one("ties 3x3", c_ties) && ok

# All-equal
ok <- check_one("all-equal 4x4", matrix(5, 4, 4)) && ok

# Contention (single column preferred by everyone)
c_fight <- matrix(c( 1,  9,  9,  9,
                     1,  9,  9,  9,
                     9,  1,  9,  9,
                     9,  9,  1,  9), 4, 4, byrow = TRUE)
ok <- check_one("contention 4x4", c_fight) && ok

# Negative costs
ok <- check_one("negative 5x5", mat_cost(5, 5, -50, 50)) && ok

# Forbidden entries
cf <- mat_cost(5, 5)
cf[sample.int(25, 4)] <- NA
ok <- check_one("with NA forbidden 5x5", cf) && ok

# Maximize
ok <- check_one("maximize 5x5", mat_cost(5, 5), maximize = TRUE) && ok

# Identity-like (already optimal after row reduction)
ok <- check_one("identity 4x4", diag(4)) && ok

# Rectangular: expect error
rect_err <- tryCatch(
  lap_animate(mat_cost(3, 7), method = "munkres"),
  error = function(e) conditionMessage(e)
)
got_err <- is.character(rect_err) && grepl("square", rect_err)
cat(sprintf("  %-30s  %s\n",
            "rectangular expect-error",
            if (got_err) "OK" else "FAIL"))
ok <- got_err && ok

cat("\n", if (ok) "ALL OK" else "FAILURES PRESENT", "\n", sep = "")

# Frame breakdown for one mid-sized run
cat("\n=== Frame breakdown for random 5x5 ===\n")
set.seed(1)
c5 <- mat_cost(5)
w5 <- lap_animate(c5, method = "munkres")
phase_counts <- table(vapply(w5$x$frames, `[[`, character(1), "phase"))
print(phase_counts)
cat("Total frames:", length(w5$x$frames), "\n")

cat("\n=== Frame breakdown for tiny 3x3 worked ===\n")
w0 <- lap_animate(c0, method = "munkres")
phase_counts_0 <- table(vapply(w0$x$frames, `[[`, character(1), "phase"))
print(phase_counts_0)
cat("Total frames:", length(w0$x$frames), "\n")

out_html <- file.path("dev_notes", "trace_munkres.html")
htmlwidgets::saveWidget(
  w0,
  file = normalizePath(out_html, mustWork = FALSE),
  selfcontained = TRUE
)
cat("\nWrote", out_html, "\n")

invisible(NULL)
