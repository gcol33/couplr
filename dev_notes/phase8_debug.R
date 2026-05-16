suppressPackageStartupMessages(devtools::load_all(".", quiet=TRUE, recompile=TRUE))
set.seed(456)
for (trial in 1:5) {
  cost <- matrix(sample(0:9, 9, replace = TRUE), nrow = 3, ncol = 3)
  cat(sprintf("--- Trial %d ---\n", trial))
  print(cost)
  res <- tryCatch(couplr:::gt_match_gt(cost, max_iters = 200L),
                  error = function(e) {cat("ERROR:", conditionMessage(e), "\n"); NULL})
  if (!is.null(res)) {
    feas <- couplr:::gt_check_one_feasible(cost, res$row_match, res$col_match,
                                            res$y_u, res$y_v)
    cat(" row_match:", res$row_match, " col_match:", res$col_match,
        " y_u:", res$y_u, " y_v:", res$y_v, " feas:", feas, "\n")
  }
}
