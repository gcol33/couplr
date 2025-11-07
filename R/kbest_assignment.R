#' k-best linear assignments (Murty)
#' @param cost numeric matrix (NA = forbidden)
#' @param k number of solutions
#' @param maximize logical
#' @param method "murty" (reserved for future variants)
#' @param single_method sub-solver for each node ("jv")
#' @export
kbest_assignment <- function(cost, k = 3, maximize = FALSE,
                             method = c("murty"),
                             single_method = c("jv")) {
  method <- match.arg(method)
  single_method <- match.arg(single_method)
  cost <- as.matrix(cost)
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")

  x <- lap_kbest_murty(cost, as.integer(k), maximize, single_method)

  # reshape: rank, match_id, row, col, cost_edge, cost_total
  matches <- x$matches          # k x n integer matrix (rows = solutions)
  totals  <- as.numeric(x$totals)
  nsol <- nrow(matches); n <- ncol(matches)

  if (nsol == 0L) {
    return(data.frame(rank=integer(), match_id=integer(), row=integer(),
                      col=integer(), cost_edge=double(), cost_total=double()))
  }

  rows <- rep(seq_len(n), times = nsol)
  cols <- as.integer(t(matches))         # vectorized by column of t -> solution-major
  rank <- rep(seq_len(nsol), each = n)
  match_id <- rep(seq_len(nsol), each = n)
  edge_costs <- cost[cbind(rows, cols)]
  tot <- rep(totals, each = n)

  data.frame(
    rank = rank,
    match_id = match_id,
    row = rows,
    col = cols,
    cost_edge = edge_costs,
    cost_total = tot
  )
}
