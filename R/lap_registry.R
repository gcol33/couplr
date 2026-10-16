# ==============================================================================
# Assignment solver registry
# ==============================================================================
# assignment() looks its `method` up here. Each entry holds the function that
# solves a dense cost matrix with no more rows than columns, and what the
# certificate and the memory modes need to know about it: whether the duals it
# returns are optimal potentials of the matrix it was handed, and whether a
# lazy cost source can drive it. The built-in solvers are registered when the
# namespace loads; register_lap_solver() adds others under the same contract.
# ==============================================================================

.lap_registry <- new.env(parent = emptyenv())

# Store one entry. `solve(cost, maximize, auction_eps)` returns a list with
# `match` (1-based column per row, 0 = unmatched) and optionally `total_cost`,
# `u`, `v` and `status`.
.add_lap_solver <- function(name, solve, algorithm, reference = NA_character_,
                            potentials = FALSE, lazy = FALSE, builtin = TRUE) {
  assign(name, list(name = name, solve = solve, algorithm = algorithm,
                    reference = reference, potentials = potentials,
                    lazy = lazy, builtin = builtin),
         envir = .lap_registry)
}

.register_builtin_lap_solvers <- function() {
  plain <- function(fn) function(cost, maximize, auction_eps = NULL) fn(cost, maximize)
  eps <- function(fn) function(cost, maximize, auction_eps = NULL) fn(cost, maximize, auction_eps)
  add <- .add_lap_solver
  add("bruteforce", plain(lap_solve_bruteforce), "Exhaustive enumeration (n <= 8)")
  add("jv", plain(lap_solve_jv), "Jonker-Volgenant shortest augmenting path",
      "Jonker and Volgenant (1987)", potentials = TRUE, lazy = TRUE)
  add("hungarian", plain(lap_solve_hungarian), "Hungarian algorithm",
      "Kuhn (1955); Munkres (1957)", potentials = TRUE)
  add("munkres", plain(lap_solve_munkres), "Kuhn-Munkres, matrix form",
      "Munkres (1957)")
  add("auction", eps(lap_solve_auction), "Auction with adaptive epsilon",
      "Bertsekas (1988)", lazy = TRUE)
  add("auction_gs", eps(lap_solve_auction_gs), "Auction, Gauss-Seidel bidding",
      "Bertsekas (1988)")
  add("auction_scaled", plain(lap_solve_auction_scaled),
      "Auction with epsilon scaling", "Bertsekas (1988)")
  add("sap", plain(lap_solve_ssp), "Successive shortest paths on the flow network",
      "Ahuja et al. (1993)")
  add("sap_dense", plain(lap_solve_sap_dense),
      "Shortest augmenting path, dense linear scan", "Ahuja et al. (1993)")
  add("csflow", plain(lap_solve_csflow),
      "Successive shortest paths with Johnson potentials", "Ahuja et al. (1993)")
  add("hk01", plain(lap_solve_hk01), "Hopcroft-Karp for 0/1 or constant costs",
      "Hopcroft and Karp (1973)")
  add("ssap_bucket", plain(lap_solve_ssap_bucket),
      "Dial's bucket shortest paths, integer costs", "Ahuja et al. (1993)")
  add("cycle_cancel", plain(lap_solve_cycle_cancel),
      "Minimum mean cycle canceling", "Ahuja et al. (1993)")
  add("gabow_tarjan", plain(lap_solve_gabow_tarjan), "Gabow-Tarjan bit scaling",
      "Gabow and Tarjan (1989)")
  add("lapmod", plain(lap_solve_lapmod), "Jonker-Volgenant on sparse rows",
      "Jonker and Volgenant (1987)")
  add("csa", plain(lap_solve_csa), "Goldberg-Kennedy cost scaling (CSA-Q)",
      "Goldberg and Kennedy (1995)")
  add("ramshaw_tarjan", plain(lap_solve_ramshaw_tarjan),
      "Ramshaw-Tarjan for rectangular matrices", "Ramshaw and Tarjan (2012)")
  add("push_relabel", plain(lap_solve_push_relabel),
      "Push-relabel cost scaling", "Goldberg and Tarjan (1990)")
  add("network_simplex", plain(lap_solve_network_simplex_wrapper),
      "Network simplex", "Ahuja et al. (1993)")
  invisible(NULL)
}

# The registry entry for `method`, with "ssp" read as "sap".
.lap_solver <- function(method) {
  if (!is.character(method) || length(method) != 1L || is.na(method)) {
    stop("`method` must be a single solver name; see lap_solvers().",
         call. = FALSE)
  }
  if (identical(method, "ssp")) method <- "sap"
  entry <- get0(method, envir = .lap_registry, inherits = FALSE)
  if (is.null(entry)) {
    stop("Unknown method \"", method, "\". Available: \"auto\", ",
         paste0("\"", sort(ls(.lap_registry)), "\"", collapse = ", "),
         ". See lap_solvers().", call. = FALSE)
  }
  entry
}

# Run a registered solver on a matrix with rows <= columns and return the raw
# result in the shape the built-in solvers use. A solver may return the match
# vector alone; the total is then read off the matrix.
.run_lap_solver <- function(entry, cost, maximize, auction_eps = NULL) {
  res <- entry$solve(cost, maximize, auction_eps = auction_eps)
  if (!is.list(res)) res <- list(match = res)
  match <- res[["match"]]
  if (!(is.integer(match) || is.double(match)) || is.object(match) ||
      !is.null(dim(match)) || length(match) != nrow(cost) ||
      any(!is.finite(match)) || any(match != trunc(match)) ||
      any(match < 0 | match > ncol(cost))) {
    stop("Solver \"", entry$name, "\" must return one column index per row ",
         "(0 for an unmatched row), integers in 0..", ncol(cost), ".",
         call. = FALSE)
  }
  res$match <- as.integer(match)
  if (is.null(res[["total_cost"]])) {
    hit <- res$match > 0L
    res$total_cost <- sum(cost[cbind(which(hit), res$match[hit])])
  }
  res
}

#' Assignment Solvers
#'
#' `lap_solvers()` lists the solvers [assignment()] accepts as `method`, and
#' `register_lap_solver()` adds one.
#'
#' A solver is a function `solve(cost, maximize, ...)` that receives a numeric
#' matrix with no more rows than columns (forbidden pairs are `Inf`, or `-Inf`
#' when `maximize = TRUE`) and returns either an integer vector giving the
#' column assigned to each row, 0 for an unmatched row, or a list with that
#' vector as `match` and optionally `total_cost`, `status` and the dual
#' potentials `u` (rows) and `v` (columns). [assignment()] transposes wider
#' problems before calling it, computes the status and, for a solver
#' registered here, attaches a certificate by default: [verify_assignment()]
#' checks the returned potentials when `potentials = TRUE` and otherwise
#' computes optimal ones, so a registered solver's answer is proved optimal or
#' reported as not.
#'
#' @param name Name under which the solver is selected with
#'   `assignment(method = name)`.
#' @param solve The solver function, see Details.
#' @param algorithm Short description shown by `lap_solvers()`.
#' @param potentials Logical; whether `solve` returns optimal dual potentials
#'   `u` and `v` of the matrix it receives.
#' @param overwrite Logical; whether to replace an existing solver of the same
#'   name. Built-in solvers cannot be replaced.
#'
#' @return `lap_solvers()` returns a tibble with one row per solver and columns
#'   `method`, `algorithm`, `reference`, `potentials` (returns optimal dual
#'   potentials), `lazy` (available with `memory_mode = "lazy"`) and
#'   `builtin`. `register_lap_solver()` returns `name` invisibly.
#'
#' @examples
#' lap_solvers()
#'
#' # A greedy heuristic: each row in turn takes its cheapest free column.
#' greedy <- function(cost, maximize, ...) {
#'   if (maximize) cost <- -cost
#'   match <- integer(nrow(cost))
#'   free <- rep(TRUE, ncol(cost))
#'   for (i in seq_len(nrow(cost))) {
#'     j <- which(free)[which.min(cost[i, free])]
#'     match[i] <- j
#'     free[j] <- FALSE
#'   }
#'   match
#' }
#' register_lap_solver("greedy_rows", greedy, algorithm = "Row-wise greedy")
#' cost <- matrix(c(1, 2, 3, 2, 4, 6, 3, 6, 10), 3)
#' res <- assignment(cost, method = "greedy_rows")
#' certificate(res)
#'
#' @export
lap_solvers <- function() {
  entries <- mget(ls(.lap_registry), envir = .lap_registry)
  entries <- entries[order(!vapply(entries, `[[`, logical(1), "builtin"))]
  tibble::tibble(
    method = vapply(entries, `[[`, character(1), "name", USE.NAMES = FALSE),
    algorithm = vapply(entries, `[[`, character(1), "algorithm", USE.NAMES = FALSE),
    reference = vapply(entries, function(e) as.character(e$reference), character(1),
                       USE.NAMES = FALSE),
    potentials = vapply(entries, `[[`, logical(1), "potentials", USE.NAMES = FALSE),
    lazy = vapply(entries, `[[`, logical(1), "lazy", USE.NAMES = FALSE),
    builtin = vapply(entries, `[[`, logical(1), "builtin", USE.NAMES = FALSE)
  )
}

#' @rdname lap_solvers
#' @export
register_lap_solver <- function(name, solve, algorithm = name,
                                potentials = FALSE, overwrite = FALSE) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !nzchar(name) || name %in% c("auto", "ssp")) {
    stop("`name` must be a single non-empty string other than \"auto\" and ",
         "\"ssp\".", call. = FALSE)
  }
  if (!is.function(solve)) {
    stop("`solve` must be a function(cost, maximize, ...).", call. = FALSE)
  }
  old <- get0(name, envir = .lap_registry, inherits = FALSE)
  if (!is.null(old) && isTRUE(old$builtin)) {
    stop("\"", name, "\" is a built-in solver and cannot be replaced.",
         call. = FALSE)
  }
  if (!is.null(old) && !isTRUE(overwrite)) {
    stop("A solver named \"", name, "\" is already registered; use ",
         "overwrite = TRUE to replace it.", call. = FALSE)
  }
  user_solve <- solve
  .add_lap_solver(name,
                  function(cost, maximize, auction_eps = NULL) {
                    user_solve(cost, maximize)
                  },
                  algorithm = as.character(algorithm),
                  potentials = isTRUE(potentials), lazy = FALSE,
                  builtin = FALSE)
  invisible(name)
}
