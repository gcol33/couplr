suppressMessages(devtools::load_all(quiet = TRUE))

# --- Tiny 3x3 with a coarse final eps (3 phases, ~12 frames) ----------------
c0 <- matrix(c(4, 2, 5,
               3, 3, 6,
               7, 5, 4), nrow = 3, byrow = TRUE)
w0 <- lap_animate(c0, method = "auction_scaled", alpha = 3, final_epsilon = 0.1)
cat("Tiny 3x3 (alpha=3, eps_final=0.1):", length(w0$x$frames), "frames\n")
print(table(vapply(w0$x$frames, `[[`, character(1), "phase")))
htmlwidgets::saveWidget(
  w0,
  file = normalizePath(file.path("dev_notes", "trace_auction_scaled_3x3.html"),
                       mustWork = FALSE),
  selfcontained = TRUE
)

# --- 5x5 integer with alpha=4 ("pow2"-style, 4 phases) ---------------------
set.seed(3)
c5 <- matrix(sample(1:25, 25, replace = TRUE), 5, 5)
w5 <- lap_animate(c5, method = "auction_scaled", alpha = 4, final_epsilon = 0.05)
cat("\n5x5 integer (alpha=4, eps_final=0.05):", length(w5$x$frames), "frames\n")
print(table(vapply(w5$x$frames, `[[`, character(1), "phase")))
htmlwidgets::saveWidget(
  w5,
  file = normalizePath(file.path("dev_notes", "trace_auction_scaled_5x5.html"),
                       mustWork = FALSE),
  selfcontained = TRUE
)

# --- 6x6 random with default alpha=7 -- show the full default schedule -----
set.seed(7)
c6 <- matrix(sample(1:50, 36, replace = TRUE), 6, 6)
w6 <- lap_animate(c6, method = "auction_scaled", alpha = 7, final_epsilon = 0.01)
cat("\n6x6 integer (alpha=7, eps_final=0.01):", length(w6$x$frames), "frames\n")
print(table(vapply(w6$x$frames, `[[`, character(1), "phase")))
htmlwidgets::saveWidget(
  w6,
  file = normalizePath(file.path("dev_notes", "trace_auction_scaled_6x6.html"),
                       mustWork = FALSE),
  selfcontained = TRUE
)

cat("\nWrote three demo HTMLs.\n")
