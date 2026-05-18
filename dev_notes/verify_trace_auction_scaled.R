# Correctness + frame-count check for trace_auction_scaled.
# Compares the trace's final matching/cost to the Hungarian production solver.

suppressMessages(devtools::load_all(quiet = TRUE))

set.seed(0xCAFE)

mat_cost <- function(rows, cols = rows, lo = 0, hi = 100, integers = FALSE) {
  v <- runif(rows * cols, lo, hi)
  if (integers) v <- floor(v)
  matrix(v, rows, cols)
}

check_one <- function(label, cost, maximize = FALSE,
                       alpha = 7, initial_epsilon_factor = 1.0,
                       final_epsilon = NULL, tol = 1e-3) {
  ref <- assignment(cost, maximize = maximize, method = "hungarian")
  w <- lap_animate(cost, method = "auction_scaled",
                   maximize = maximize,
                   alpha = alpha,
                   initial_epsilon_factor = initial_epsilon_factor,
                   final_epsilon = final_epsilon)
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

cat("=== trace_auction_scaled vs assignment(method = 'hungarian') oracle ===\n\n")

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

# Heavy ties
c_ties <- matrix(c(1, 1, 1,
                   1, 1, 1,
                   1, 2, 3), 3, 3, byrow = TRUE)
ok <- check_one("ties 3x3", c_ties) && ok

# All-equal
ok <- check_one("all-equal 4x4", matrix(5, 4, 4)) && ok

# Contention
c_fight <- matrix(c( 1,  9,  9,  9,
                     1,  9,  9,  9,
                     9,  1,  9,  9,
                     9,  9,  1,  9), 4, 4, byrow = TRUE)
ok <- check_one("contention 4x4", c_fight) && ok

# Negative costs
ok <- check_one("negative 5x5", mat_cost(5, 5, -50, 50)) && ok

# Maximize
ok <- check_one("maximize 5x5", mat_cost(5, 5), maximize = TRUE) && ok

# Different alpha (alpha=4 = pow2 schedule from cpp)
ok <- check_one("alpha=4 random 6x6", mat_cost(6), alpha = 4) && ok

# Larger alpha (slower scaling, more phases)
ok <- check_one("alpha=2 random 5x5", mat_cost(5), alpha = 2) && ok

# Forbidden entries
cf <- mat_cost(5, 5)
cf[sample.int(25, 3)] <- NA
ok <- check_one("with NA forbidden 5x5", cf) && ok

# Rectangular: expect error
rect_err <- tryCatch(
  lap_animate(mat_cost(3, 7), method = "auction_scaled"),
  error = function(e) conditionMessage(e)
)
got_err <- is.character(rect_err) && grepl("square", rect_err)
cat(sprintf("  %-30s  %s\n",
            "rectangular expect-error",
            if (got_err) "OK" else "FAIL"))
ok <- got_err && ok

cat("\n", if (ok) "ALL OK" else "FAILURES PRESENT", "\n", sep = "")

# Frame breakdown for one mid-sized run
cat("\n=== Frame breakdown for random 6x6 (default alpha=7) ===\n")
set.seed(1)
c6 <- mat_cost(6)
w6 <- lap_animate(c6, method = "auction_scaled")
phase_counts <- table(vapply(w6$x$frames, `[[`, character(1), "phase"))
print(phase_counts)
cat("Total frames:", length(w6$x$frames), "\n")

cat("\n=== Frame breakdown for tiny 3x3 worked (default alpha=7) ===\n")
w0 <- lap_animate(c0, method = "auction_scaled")
phase_counts_0 <- table(vapply(w0$x$frames, `[[`, character(1), "phase"))
print(phase_counts_0)
cat("Total frames:", length(w0$x$frames), "\n")

cat("\n=== Frame breakdown for 5x5 random with alpha=2 (slow scaling) ===\n")
set.seed(2)
c5 <- mat_cost(5)
w5 <- lap_animate(c5, method = "auction_scaled", alpha = 2)
phase_counts_5 <- table(vapply(w5$x$frames, `[[`, character(1), "phase"))
print(phase_counts_5)
cat("Total frames:", length(w5$x$frames), "\n")

out_html <- file.path("dev_notes", "trace_auction_scaled.html")
htmlwidgets::saveWidget(
  w0,
  file = normalizePath(out_html, mustWork = FALSE),
  selfcontained = TRUE
)
cat("\nWrote", out_html, "\n")

invisible(NULL)
