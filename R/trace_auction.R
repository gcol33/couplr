# ==============================================================================
# Reference forward auction (Bertsekas 1988) with frame-by-frame trace
# ==============================================================================
# Teaching implementation. The production solver lives in C++ and is reached
# via assignment(cost, method = "auction"); this R version exists purely to
# emit a step-by-step state trace for lap_animate().
#
# Algorithmic outline (minimization, square cost matrix):
#
#   1. Treat as a maximization of v[i,j] := -c[i,j] (or +c if maximize=TRUE).
#      Each object j carries a price p[j], initially 0. A person i's "net
#      profit" from object j is a[i,j] := v[i,j] - p[j].
#   2. While any person is unassigned:
#      a. Pick an unassigned person i.
#      b. Identify their best object j* = argmax_j a[i,j] (profit v_best)
#         and second-best (profit w_best).
#      c. Person i bids on j*, raising its price by
#                p[j*] += (v_best - w_best) + eps.
#         The margin (v_best - w_best) is the smallest amount that, at the new
#         price, leaves i indifferent between j* and their next favourite; the
#         +eps ensures strict progress.
#      d. If j* was held by some person k, k is displaced (becomes unassigned).
#         Then i is assigned to j*.
#   3. The fixed eps is chosen small enough that an eps-equilibrium implies
#      an exactly optimal assignment. For integer costs eps < 1/n is sufficient;
#      for real-valued costs we pick eps proportional to the cost scale.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_auction <- function(cost, maximize = FALSE, ...) {
  cost <- as.matrix(cost)
  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix.", call. = FALSE)
  }
  n <- nrow(cost); m <- ncol(cost)
  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.", call. = FALSE)
  }
  if (n != m) {
    stop(
      "trace_auction currently requires a square cost matrix (nrow == ncol). ",
      "For production solving on rectangular inputs use ",
      "assignment(cost, method = \"auction\") or lap_solve(cost, method = \"auction\").",
      call. = FALSE
    )
  }

  # --- Internal max-problem view ---------------------------------------------
  # vmat[i,j] is the "value" person i gets from object j. We are maximising
  # the total value, equivalently minimising the total cost when maximize=FALSE.
  vmat <- if (maximize) cost else -cost
  finite_mask <- is.finite(vmat)
  if (!any(finite_mask)) {
    stop("`cost` has no finite entries.", call. = FALSE)
  }
  scale <- max(abs(vmat[finite_mask]))
  big <- (scale + 1) * (n + m + 1)
  vmat[!finite_mask] <- -big   # forbidden -> very negative value (won't be bid on)

  # --- Pick eps ---------------------------------------------------------------
  # Integer costs => eps < 1/n suffices for exact optimality.
  # Real costs    => use a small fraction of the cost scale.
  is_integer_cost <- all(abs(cost[finite_mask] - round(cost[finite_mask])) < .Machine$double.eps^0.5)
  eps <- if (is_integer_cost) 1 / (n + 1) else max(scale, 1) / max(n * n * 100, 1)

  # --- State ----------------------------------------------------------------
  p <- numeric(m)                       # object prices
  assign_object <- integer(n)           # person -> object (0 = unassigned)
  assign_person <- integer(m)           # object -> person (0 = unassigned)

  frames <- list()
  step <- 0L

  emit <- function(phase, description,
                   active_edges = list(), path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- list(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = assign_object,
      dual_u       = NULL,      # auction has no row potentials
      dual_v       = p,         # column prices shown as "v=" in the widget
      active_edges = active_edges,
      path         = path
    )
  }

  emit(
    "init",
    sprintf(
      paste0(
        "Initialise: every object has price 0 and no person is assigned. ",
        "Bidding increment eps = %.4g. Persons (rows) will repeatedly bid on objects (cols); ",
        "winning bids raise the object's price, displacing the previous holder if any."
      ),
      eps
    )
  )

  # --- Main loop ------------------------------------------------------------
  iter <- 0L
  max_iter <- 200L * n * n            # safety cap

  while (any(assign_object == 0L) && iter < max_iter) {
    iter <- iter + 1L

    i <- which(assign_object == 0L)[1]

    # Net profit of each object for person i
    a <- vmat[i, ] - p

    j_star <- which.max(a)
    v_best <- a[j_star]
    a_tmp <- a
    a_tmp[j_star] <- -Inf
    j_second <- which.max(a_tmp)
    w_best <- a_tmp[j_second]
    if (!is.finite(w_best)) w_best <- v_best   # degenerate case (only one valid object)

    bid <- (v_best - w_best) + eps

    emit(
      "select_person",
      sprintf(
        "Person %d is unassigned. They evaluate net profit a[%d,j] = v[%d,j] - p[j] across every object.",
        i, i, i
      ),
      active_edges = lapply(seq_len(m), function(jj) c(i, jj))
    )

    emit(
      "bid",
      sprintf(
        paste0(
          "Best object for person %d is %d (profit %.4g); second-best is object %d (profit %.4g). ",
          "Person %d bids on object %d, raising its price by (best - second + eps) = %.4g."
        ),
        i, j_star, v_best, j_second, w_best, i, j_star, bid
      ),
      active_edges = list(c(i, j_star), c(i, j_second))
    )

    # Apply the bid
    p[j_star] <- p[j_star] + bid

    old_holder <- assign_person[j_star]
    if (old_holder > 0L && old_holder != i) {
      assign_object[old_holder] <- 0L
      assign_person[j_star] <- 0L
      emit(
        "displace",
        sprintf(
          "Object %d was held by person %d - they are displaced and become unassigned. Price of object %d is now %.4g.",
          j_star, old_holder, j_star, p[j_star]
        )
      )
    }

    assign_object[i] <- j_star
    assign_person[j_star] <- i

    emit(
      "assign",
      sprintf(
        "Person %d is now assigned to object %d (current price %.4g).",
        i, j_star, p[j_star]
      ),
      path = list(c(i, j_star))
    )
  }

  if (iter >= max_iter) {
    warning(
      "Auction inner loop hit iteration cap (",
      max_iter,
      ") - eps may be too small or the cost matrix is degenerate."
    )
  }

  matched_costs <- cost[cbind(seq_len(n), assign_object)]
  total <- sum(matched_costs, na.rm = TRUE)

  emit(
    "final",
    sprintf(
      "All %d persons assigned after %d bids. Total cost: %.6g.",
      n, iter, total
    )
  )

  list(
    meta = list(
      algorithm   = "auction",
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Bertsekas forward auction (1988). Persons (rows) bid on objects (cols); ",
        "each object carries a price, and a winning bid raises the price by the gap to the ",
        "second-best alternative plus eps. The previous holder of the contested object is displaced ",
        "and re-bids later. Iteration ends when every person is assigned."
      )
    ),
    frames = frames
  )
}

register_trace("auction", trace_auction)
