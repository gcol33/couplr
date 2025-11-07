#' @useDynLib assignr, .registration = TRUE
#' @importFrom Rcpp evalCpp
NULL
# Internal prep step for LAP: NA -> forbidden, rectangular OK, optional maximize flip
# Returns list(cost=row-major numeric, mask=integer 0/1, n, m, cmax)
prepare_cost_matrix <- function(cost, maximize = FALSE) {
  cost <- as.matrix(cost)
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")
  lap_prepare_cost_matrix(cost, maximize)
}
