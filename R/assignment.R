#' Linear assignment solver
#'
#' Solve the linear assignment problem (minimum- or maximum-cost matching)
#' using several algorithms. Forbidden edges can be marked as `NA` or `Inf`.
#'
#' @param cost Numeric matrix; rows = tasks, columns = agents. `NA` or `Inf`
#'   entries are treated as forbidden assignments.
#' @param maximize Logical; if `TRUE`, maximizes the total cost instead of minimizing.
#' @param method Character string indicating the algorithm to use.
#'   One of `"auto"`, `"jv"`, `"hungarian"`, `"auction"`, `"auction_gs"`,
#'   `"sap"`, `"ssp"`, `"csflow"`, `"hk01"`, or `"bruteforce"`.
#'   `"ssp"` is accepted as an alias for `"sap"`.
#' @param auction_eps Optional numeric epsilon for the Auction/Auction-GS methods.
#'   If `NULL`, an internal default (e.g., `1e-9`) is used.
#' @param eps Deprecated. Use `auction_eps`. If provided and `auction_eps` is `NULL`,
#'   its value is used for `auction_eps`.
#'
#' @return An object of class `lap_solve_result`, a list with elements:
#' \itemize{
#'   \item `match` — integer vector of length `min(nrow(cost), ncol(cost))`
#'         giving the assigned column for each row (0 if unassigned).
#'   \item `total_cost` — numeric scalar, the objective value.
#'   \item `status` — character scalar, e.g. `"optimal"`.
#'   \item `method_used` — character scalar, the algorithm actually used.
#' }
#'
#' @details
#' `method = "auto"` selects an algorithm based on problem size/shape and data
#' characteristics (e.g., small problems → `"bruteforce"`, binary/all-equal costs → `"hk01"`,
#' sparse/very rectangular → `"sap"`, large dense → `"auction"`, otherwise `"jv"`/`"hungarian"`).
#'
#' @examples
#' cost <- matrix(c(4,2,5, 3,3,6, 7,5,4), nrow = 3, byrow = TRUE)
#' res  <- assignment(cost)
#' res$match; res$total_cost
#'
#' @export
assignment <- function(cost, maximize = FALSE,
                       method = c("auto","jv","hungarian","auction","auction_gs",
                                  "sap","ssp","csflow","hk01","bruteforce"),
                       auction_eps = NULL, eps = NULL) {
  method <- match.arg(method)

  # Back-compat: eps → auction_eps
  if (!is.null(eps) && is.null(auction_eps)) {
    auction_eps <- eps
  }

  # alias for compatibility
  if (method == "ssp") method <- "sap"

  cost <- as.matrix(cost)
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")

  hk01_candidate <- function(M) {
    x <- as.numeric(M[is.finite(M)])
    if (!length(x)) return(FALSE)
    ux <- sort(unique(round(x, 12)))
    if (length(ux) == 1L) return(TRUE)                       # all-equal
    if (length(ux) == 2L && all(ux %in% c(0, 1))) return(TRUE) # exact 0/1
    FALSE
  }

  n <- nrow(cost); m <- ncol(cost)

  if (method == "auto") {
    if (n <= 5 && m <= 5) {
      method <- "bruteforce"
    } else if (hk01_candidate(cost)) {
      method <- "hk01"
    } else {
      na_rate <- mean(is.na(cost))
      if (n <= 10 && m <= 10) {
        method <- "jv"
      } else if (na_rate > 0.35 || m >= 2 * n) {
        method <- "sap"
      } else if (n > 20 && n <= 80 && na_rate < 0.1 && n <= m) {
        method <- "hungarian"
      } else if (n >= 100 && n <= m) {
        method <- "auction"
      } else {
        method <- "jv"
      }
    }
  }

  # auto-transpose if rows > cols
  transposed <- FALSE
  work <- cost
  if (n > m) {
    work <- t(cost)
    transposed <- TRUE
    tmp <- n; n <- m; m <- tmp
  }

  res_raw <- switch(
    method,
    "bruteforce" = lap_solve_bruteforce(work, maximize),
    "jv"         = lap_solve_jv(work, maximize),
    "hungarian"  = lap_solve_hungarian(work, maximize),
    "auction"    = lap_solve_auction(work, maximize, auction_eps),
    "auction_gs" = lap_solve_auction_gs(work, maximize, auction_eps),
    "sap"        = lap_solve_ssp(work, maximize),
    "csflow"     = lap_solve_csflow(work, maximize),
    "hk01"       = lap_solve_hk01(work, maximize),
    stop("Unknown or unimplemented method: ", method)
  )

  match_out <- as.integer(res_raw$match)
  if (transposed) {
    n0 <- ncol(work)   # original nrows
    m0 <- nrow(work)   # original ncols
    inv <- integer(n0); inv[] <- 0L
    for (i in seq_len(m0)) {
      j <- match_out[i]
      if (j > 0L) inv[j] <- i
    }
    match_out <- inv
  }

  out <- list(
    match = match_out,
    total_cost = as.numeric(res_raw$total_cost),
    status = "optimal",
    method_used = method
  )
  class(out) <- "lap_solve_result"
  out
}
