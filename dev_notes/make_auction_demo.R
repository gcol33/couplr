# Generate a more visually interesting auction demo (6x6 integer costs with
# enough contention to produce displacements).
suppressMessages(devtools::load_all(quiet = TRUE))

set.seed(42)
cost <- matrix(sample(1:20, 36, replace = TRUE), 6, 6)
cat("Cost matrix:\n"); print(cost); cat("\n")

w <- lap_animate(cost, method = "auction")
cat("Frames:", length(w$x$frames), "\n")
cat("Optimal cost:", w$x$meta$total_cost, "\n")
cat("Final matching:", w$x$frames[[length(w$x$frames)]]$matching, "\n")

phase_counts <- table(vapply(w$x$frames, `[[`, character(1), "phase"))
print(phase_counts)

htmlwidgets::saveWidget(
  w,
  file = normalizePath(file.path("dev_notes", "trace_auction_6x6.html"), mustWork = FALSE),
  selfcontained = TRUE
)
cat("\nWrote dev_notes/trace_auction_6x6.html\n")
