# Smoke test for lap_animate infrastructure.
# Loads package via devtools, confirms registry + widget construction work,
# then writes a standalone HTML to inspect visually.

suppressMessages(devtools::load_all(quiet = TRUE))

cat("Animated methods:", paste(animated_methods(), collapse = ", "), "\n\n")

cost <- matrix(c(4, 2, 5,
                 3, 3, 6,
                 7, 5, 4), nrow = 3, byrow = TRUE)

w <- lap_animate(cost, method = "hungarian")

cat("Class of result :", paste(class(w), collapse = ", "), "\n")
cat("Algorithm       :", w$x$meta$algorithm, "\n")
cat("Total cost      :", w$x$meta$total_cost, "\n")
cat("Frames          :", length(w$x$frames), "\n")
cat("Final matching  :", w$x$frames[[2]]$matching, "\n")
cat("Final descr.    :", w$x$frames[[2]]$description, "\n\n")

# Try every registered method
for (m in animated_methods()) {
  w_m <- lap_animate(cost, method = m)
  ok <- inherits(w_m, "htmlwidget") &&
    length(w_m$x$frames) >= 2 &&
    !is.null(w_m$x$meta$total_cost)
  cat(sprintf("  %-15s frames=%d total_cost=%.2f  %s\n",
              m, length(w_m$x$frames), w_m$x$meta$total_cost,
              if (ok) "OK" else "FAIL"))
}

# Edge cases ---------------------------------------------------------------
cat("\n--- Edge cases ---\n")

# Unknown method should error with a helpful message
res <- tryCatch(lap_animate(cost, method = "no_such_method"),
                error = function(e) conditionMessage(e))
cat("Unknown method error:\n  ", res, "\n", sep = "")

# Rectangular cost (3 rows, 4 cols)
rcost <- matrix(runif(12, 1, 10), nrow = 3, ncol = 4)
wr <- lap_animate(rcost, method = "jv")
cat(sprintf("\nRectangular 3x4: frames=%d matching=%s\n",
            length(wr$x$frames),
            paste(wr$x$frames[[2]]$matching, collapse = ",")))

# Forbidden edges (NA in cost)
fcost <- matrix(runif(9, 1, 10), 3, 3); fcost[1, 3] <- NA
wf <- lap_animate(fcost, method = "jv")
cat(sprintf("Forbidden NA at [1,3]: frames=%d matching=%s\n",
            length(wf$x$frames),
            paste(wf$x$frames[[2]]$matching, collapse = ",")))

# Save a standalone HTML for visual inspection.
out_html <- file.path("dev_notes", "smoke_test_animate.html")
htmlwidgets::saveWidget(w, file = normalizePath(out_html, mustWork = FALSE),
                        selfcontained = TRUE)
cat("\nWrote", out_html, "\n")
