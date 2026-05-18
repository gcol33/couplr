suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))
ms <- animated_methods()
cat("Smoke-rendering", length(ms), "methods to dev_notes/animation_preview/\n")
dir.create("dev_notes/animation_preview", showWarnings = FALSE, recursive = TRUE)

set.seed(11)
cost_std <- matrix(sample.int(99, 64), 8, 8)
cost_bin <- matrix(c(0,1,0,1,0,1,1,0, 1,0,1,0,1,0,0,1,
                     0,1,0,1,1,0,1,0, 1,0,1,0,0,1,0,1,
                     0,1,1,0,0,1,1,0, 1,0,0,1,1,0,0,1,
                     1,0,1,0,1,1,0,0, 0,1,0,1,0,0,1,1), 8, 8)

passes <- 0L; fails <- 0L
for (m in ms) {
  cat("  ", m, "...")
  cm <- if (m == "hk01") cost_bin
        else if (m == "bruteforce") cost_std[1:6, 1:6]
        else cost_std
  tryCatch({
    w <- lap_animate(cm, method = m)
    htmlwidgets::saveWidget(w, sprintf("dev_notes/animation_preview/%s.html", m),
                            selfcontained = TRUE)
    cat(" OK\n"); passes <- passes + 1L
  }, error = function(e) {
    cat(" FAILED:", conditionMessage(e), "\n"); fails <<- fails + 1L
  })
}
cat(sprintf("\n%d ok / %d fail / %d total\n", passes, fails, length(ms)))
