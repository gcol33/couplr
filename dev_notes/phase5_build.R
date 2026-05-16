## Build couplr with -DCOUPLR_GT_DEBUG (Makevars.win edited in place).
## Run from package root.
suppressPackageStartupMessages({
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("devtools not installed")
  }
})
unlink("src/symbols.rds")
devtools::load_all(".", quiet = FALSE, recompile = TRUE)
cat("\nload_all ok\n")
