#' Solve many assignment instances
#' @param costs list of numeric matrices, or a 3D array
#' @param maximize logical
#' @param method algorithm to use (see assignment)
#' @return data.frame with columns: id, match (list<int>), total_cost, method_used
#' @export
assignment_batch <- function(costs, maximize = FALSE,
                             method = c("auto","jv","hungarian","auction","sap","bruteforce")) {
  method <- match.arg(method)
  mats <- if (is.array(costs) && length(dim(costs)) == 3) {
    lapply(seq_len(dim(costs)[3]), function(i) costs[,,i])
  } else {
    costs
  }
  out <- lapply(seq_along(mats), function(i) {
    sol <- assignment(mats[[i]], maximize = maximize, method = method)
    data.frame(id = i,
               match = I(list(sol$match)),
               total_cost = sol$total_cost,
               method_used = sol$method_used)
  })
  do.call(rbind, out)
}

#' @export
print.assignment_batch_result <- function(x, ...) {
  cat("Batch assignment result (", nrow(x), " problems)\n", sep="")
  print(as.data.frame(x), ...)
  invisible(x)
}
