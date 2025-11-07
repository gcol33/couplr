#' Linear assignment solver
#'
#' Provides several algorithms for solving the linear assignment problem
#' (a.k.a. minimum-cost matching). Forbidden edges can be set as `NA`.
#'
#' @param cost numeric matrix; rows = tasks, columns = agents
#' @param maximize logical; if TRUE, maximizes total cost instead of minimizing
#' @param method character; one of
#'   `"auto"`, `"jv"`, `"hungarian"`, `"auction"`, `"auction_gs"`, `"sap"`, `"ssp"`, `"csflow"`, `"hk01"`, or `"bruteforce"`
#' @param auction_eps optional numeric epsilon for the Auction and Auction-GS methods
#'   (default: internal value `1e-9`)
#' @export
assignment <- function(cost, maximize = FALSE,
                       method = c("auto","jv","hungarian","auction","auction_gs",
                                  "sap","ssp","csflow","hk01","bruteforce"),
                       auction_eps = NULL, eps = NULL) {
  method <- match.arg(method)

  # Support both eps and auction_eps for backward compatibility
  if (!is.null(eps) && is.null(auction_eps)) {
    auction_eps <- eps
  }

  # alias for compatibility with tests / external callers
  if (method == "ssp") method <- "sap"

  cost <- as.matrix(cost)
  if (any(is.nan(cost))) stop("NaN not allowed in `cost`")

  # Conservative detector for hk01: exactly all-equal or exactly binary {0,1}
  hk01_candidate <- function(M) {
    x <- as.numeric(M[is.finite(M)])
    if (!length(x)) return(FALSE)
    ux <- sort(unique(round(x, 12)))
    if (length(ux) == 1L) return(TRUE)                 # all-equal
    if (length(ux) == 2L && all(ux %in% c(0, 1))) return(TRUE)  # exact 0/1
    FALSE
  }

  n <- nrow(cost); m <- ncol(cost)

  if (method == "auto") {
    # Tiny problems: bruteforce guarantees correctness and is fast
    if (n <= 5 && m <= 5) {
      method <- "bruteforce"
    } else if (hk01_candidate(cost)) {
      method <- "hk01"        # fast special case: binary {0,1} or all-equal
    } else {
      na_rate <- mean(is.na(cost))
      if (n <= 10 && m <= 10) {
        method <- "jv"        # small problems
      } else if (na_rate > 0.35 || m >= 2*n) {
        method <- "sap"        # sparse or very rectangular
      } else if (n > 20 && n <= 80 && na_rate < 0.1 && n <= m) {
        method <- "hungarian"  # medium-dense problems
      } else if (n >= 100 && n <= m) {
        method <- "auction"    # large dense problems
      } else {
        method <- "jv"         # general-purpose fallback
      }
    }
  }

  # ---- auto-transpose if n > m ----
  transposed <- FALSE
  work <- cost
  if (n > m) {
    work <- t(cost)
    transposed <- TRUE
    tmp <- n; n <- m; m <- tmp
  }

  # call the chosen solver on `work`
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

  # map back if we transposed
  match_out <- as.integer(res_raw$match)
  if (transposed) {
    # `work` is m0 x n0. match_out has length m0: row i (orig col i) -> col j (orig row j).
    # Invert to a length n0 vector: original row j -> original col i.
    n0 <- ncol(work)   # original nrows
    m0 <- nrow(work)   # original ncols
    inv <- integer(n0); inv[] <- 0L
    for (i in seq_len(m0)) {
      j <- match_out[i]
      if (j > 0L) inv[j] <- i
    }
    match_out <- inv
  }

  list(
    match = match_out,
    total_cost = as.numeric(res_raw$total_cost),
    status = "optimal",
    method_used = method
  )
}
