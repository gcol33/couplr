#' k-best linear assignment solutions (Murty's algorithm)
#'
#' Compute the top-\eqn{k} optimal solutions to the linear assignment problem
#' using Murty's algorithm. Each solution represents a distinct assignment
#' of rows to columns with its total cost.
#'
#' @param cost Numeric cost matrix where rows represent sources and columns represent
#'   targets. Use `NA` or `Inf` for forbidden assignments.
#' @param k Integer; the number of best distinct solutions to return.
#' @param maximize Logical; if `TRUE`, maximizes total cost instead of minimizing.
#' @param method Character string indicating which k-best algorithm to use.
#'   Currently only `"murty"` is implemented (reserved for future variants).
#' @param single_method Character string giving the base assignment solver
#'   used within each Murty node. Typically `"jv"`.
#'
#' @return A `data.frame` with one row per assignment in each solution and columns:
#' \itemize{
#'   \item `rank` – rank of the solution (1 = best),
#'   \item `match_id` – identifier for the solution (same as rank),
#'   \item `row`, `col` – assigned row and column indices,
#'   \item `cost_edge` – cost of the assignment edge,
#'   \item `cost_total` – total cost of the full solution.
#' }
#'
#' @details
#' Murty’s algorithm systematically partitions the solution space of the
#' assignment problem to find the next-best solutions after the optimal one.
#' Each subproblem is solved with a base LAP solver (`single_method`).
#'
#' @examples
#' cost <- matrix(c(
#'   4, 2, 5,
#'   3, 3, 6,
#'   7, 5, 4
#' ), nrow = 3, byrow = TRUE)
#' 
#' kbest_assignment(cost, k = 3)
#'
#' @export
kbest_assignment <- function(cost, k = 3, maximize = FALSE,
                             method = c("murty"),
                             single_method = c("jv")) {
  method <- match.arg(method)
  single_method <- match.arg(single_method)
  cost <- as.matrix(cost)
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")

  x <- lap_kbest_murty(cost, as.integer(k), maximize, single_method)

  matches <- x$matches
  totals  <- as.numeric(x$totals)
  nsol <- nrow(matches); n <- ncol(matches)

  if (nsol == 0L) {
    return(data.frame(
      rank = integer(),
      match_id = integer(),
      row = integer(),
      col = integer(),
      cost_edge = double(),
      cost_total = double()
    ))
  }

  rows <- rep(seq_len(n), times = nsol)
  cols <- as.integer(t(matches))
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
