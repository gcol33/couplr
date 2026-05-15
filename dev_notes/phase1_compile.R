## Phase 1 helper: regenerate Rcpp attributes + reload package.
## Use via:  Rscript dev_notes/phase1_compile.R
suppressPackageStartupMessages({
  library(Rcpp)
  library(devtools)
})

cat("[1/3] Rcpp::compileAttributes()...\n")
Rcpp::compileAttributes(".")

cat("[2/3] devtools::document()...\n")
devtools::document(".", quiet = TRUE)

cat("[3/3] devtools::load_all()...\n")
devtools::load_all(".", quiet = TRUE, recompile = TRUE)

cat("OK\n")
