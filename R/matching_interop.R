# ==============================================================================
# Ecosystem Interoperability - cobalt, MatchIt, marginaleffects
# ==============================================================================

#' Convert couplr Result to matchit Object
#'
#' Constructs a \code{matchit}-class S3 object from a couplr result, enabling
#' use with any function that accepts \pkg{MatchIt} objects (e.g.,
#' \pkg{cobalt}, \pkg{marginaleffects}).
#'
#' @param result A couplr result object (matching_result, full_matching_result,
#'   cem_result, or subclass_result)
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param formula Optional formula used for matching. If not provided, a
#'   default formula is constructed from \code{result$info$vars}.
#' @param left_id Name of ID column in left (default: \code{"id"})
#' @param right_id Name of ID column in right (default: \code{"id"})
#' @param estimand Target estimand stamped on the matchit object: one of
#'   \code{"ATT"}, \code{"ATC"} or \code{"ATE"}. \code{NULL} (default) reads it
#'   from the design, which every couplr front door records as
#'   \code{info$estimand}. MatchIt and marginaleffects read this field to pick
#'   the target population and the weighting of the effect estimate, so give it
#'   explicitly whenever the design does not determine it -- in particular when
#'   \code{left} holds the controls, which makes the design's left-focal
#'   weighting an ATC.
#' @param ... Additional arguments (ignored)
#'
#' @return An S3 object of class \code{"matchit"} with fields:
#' \describe{
#'   \item{match.matrix}{Match matrix (treated x controls)}
#'   \item{treat}{Named treatment vector (1/0)}
#'   \item{weights}{Matching weights}
#'   \item{X}{Covariate matrix}
#'   \item{call}{Original call}
#'   \item{info}{Metadata from couplr}
#' }
#'
#' @examples
#' \dontrun{
#' left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
#' right <- data.frame(id = 6:15, age = runif(10, 20, 70))
#' result <- match_couples(left, right, vars = "age")
#' mi <- as_matchit(result, left, right)
#' # Now use with cobalt:
#' cobalt::bal.tab(mi)
#' }
#'
#' @export
as_matchit <- function(result, left, right,
                       formula = NULL,
                       left_id = "id", right_id = "id",
                       estimand = NULL,
                       ...) {

  estimand <- .resolve_estimand(result, estimand)

  # Get match_data for weights and subclass
  md <- match_data(result, left, right, left_id = left_id,
                   right_id = right_id)

  if (nrow(md) == 0) {
    stop("No matched units to convert", call. = FALSE)
  }

  # Determine variable names
  vars <- result$info$vars
  if (is.null(vars)) {
    # Try to infer from data
    exclude <- c("id", left_id, right_id, "treatment", "weights",
                 "subclass", "distance")
    vars <- setdiff(names(md), exclude)
  }

  # Build formula if not provided
  if (is.null(formula)) {
    formula <- stats::as.formula(
      paste("treatment ~", paste(vars, collapse = " + "))
    )
  }

  # Treatment vector
  treat <- stats::setNames(md$treatment, md$id)

  # Weights
  wts <- stats::setNames(md$weights, md$id)

  # Covariates matrix
  X_cols <- intersect(vars, names(md))
  X <- as.data.frame(md[, X_cols, drop = FALSE])
  rownames(X) <- md$id

  # Distance vector
  distance <- if ("distance" %in% names(md)) {
    stats::setNames(md$distance, md$id)
  } else {
    NULL
  }

  # Build match.matrix for 1:1 results
  match_matrix <- NULL
  if (inherits(result, "matching_result") && !is.null(result$pairs)) {
    pairs <- result$pairs
    if (nrow(pairs) > 0) {
      match_matrix <- matrix(
        as.character(pairs$right_id),
        ncol = 1,
        dimnames = list(as.character(pairs$left_id), NULL)
      )
    }
  }

  # Subclass
  subclass <- if ("subclass" %in% names(md)) {
    stats::setNames(as.factor(md$subclass), md$id)
  } else {
    NULL
  }

  # Determine method label
  method_label <- if (inherits(result, "full_matching_result")) {
    "full"
  } else if (inherits(result, "cem_result")) {
    "cem"
  } else if (inherits(result, "subclass_result")) {
    "subclass"
  } else {
    "nearest"
  }

  structure(
    list(
      match.matrix = match_matrix,
      model = list(formula = formula),
      treat = treat,
      distance = distance,
      weights = wts,
      subclass = subclass,
      X = X,
      call = match.call(),
      info = list(
        method = method_label,
        source = "couplr",
        couplr_info = result$info
      ),
      nn = NULL,
      method = method_label,
      estimand = estimand,
      formula = formula
    ),
    class = "matchit"
  )
}

# The estimand a matchit object is labelled with. It comes from the design,
# which every front door records in info$estimand, and a caller can override it
# -- the design knows how it weights, not which side of the caller's data holds
# the treated units. There is no default: a guessed estimand propagates into the
# reported causal quantity rather than failing loudly.
.resolve_estimand <- function(result, estimand) {
  if (!is.null(estimand)) {
    estimand <- toupper(as.character(estimand))
    if (length(estimand) != 1L || !estimand %in% c("ATT", "ATE", "ATC")) {
      stop("estimand must be one of 'ATT', 'ATE', 'ATC'", call. = FALSE)
    }
    return(estimand)
  }

  est <- result$info$estimand
  if (is.null(est) || is.na(est)) {
    stop("This result carries no estimand, so there is nothing to label the ",
         "matchit object with. MatchIt and marginaleffects read that field to ",
         "choose the target population, so pass estimand = \"ATT\", \"ATC\" ",
         "or \"ATE\".", call. = FALSE)
  }

  dropped <- result$info$focal_discarded
  if (!is.null(dropped) && !is.na(dropped) && dropped > 0L) {
    warning(sprintf(
      paste0("The design did not retain %d of the focal (left) units, so ",
             "estimand = \"%s\" refers to the matched focal subset rather ",
             "than to all of them."),
      dropped, est), call. = FALSE)
  }

  est
}


# ==============================================================================
# cobalt bal.tab methods
# ==============================================================================

#' Balance Table for Matching Results (cobalt integration)
#'
#' S3 method enabling \code{cobalt::bal.tab()} on couplr result objects.
#' Requires the \pkg{cobalt} package to be installed.
#'
#' @param x A couplr result object
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param data Data frame used for subclassification (for subclass_result only)
#' @param ... Additional arguments. Arguments named in [as_matchit()]'s
#'   signature go to the conversion; the rest go to \code{cobalt::bal.tab()}.
#'
#' @return A cobalt balance table object
#'
#' @details
#' These methods convert couplr results to the format cobalt expects
#' (a matchit-class object) and then delegate to cobalt's own
#' \code{bal.tab.matchit()} method. The \pkg{cobalt} package must be
#' installed but is not required for couplr to function.
#'
#' @name bal.tab.matching_result
NULL

# One conversion for the three pair-shaped result classes. `...` carries
# arguments for two different functions, so it is split by whose formals name
# them: as_matchit() gets its own, cobalt::bal.tab() gets the rest. Forwarding
# the whole of `...` to both hands each function the other's arguments.
.bal_tab_via_matchit <- function(x, left, right, ...) {
  if (!requireNamespace("cobalt", quietly = TRUE)) {
    stop("Package 'cobalt' is required for bal.tab(). Install with: install.packages('cobalt')",
         call. = FALSE)
  }
  dots <- list(...)
  own <- setdiff(names(formals(as_matchit)), c("result", "left", "right", "..."))
  to_matchit <- dots[intersect(names(dots), own)]
  to_cobalt <- dots[setdiff(names(dots), own)]

  mi <- do.call(as_matchit, c(list(x, left, right), to_matchit))
  do.call(cobalt::bal.tab, c(list(mi), to_cobalt))
}

#' @rdname bal.tab.matching_result
#' @exportS3Method cobalt::bal.tab
bal.tab.matching_result <- function(x, left, right, ...) {
  .bal_tab_via_matchit(x, left, right, ...)
}

#' @rdname bal.tab.matching_result
#' @exportS3Method cobalt::bal.tab
bal.tab.full_matching_result <- function(x, left, right, ...) {
  .bal_tab_via_matchit(x, left, right, ...)
}

#' @rdname bal.tab.matching_result
#' @exportS3Method cobalt::bal.tab
bal.tab.cem_result <- function(x, left, right, ...) {
  .bal_tab_via_matchit(x, left, right, ...)
}

#' @rdname bal.tab.matching_result
#' @exportS3Method cobalt::bal.tab
bal.tab.subclass_result <- function(x, data = NULL, ...) {
  if (!requireNamespace("cobalt", quietly = TRUE)) {
    stop("Package 'cobalt' is required for bal.tab(). Install with: install.packages('cobalt')",
         call. = FALSE)
  }
  md <- match_data(x, data = data)
  treat_var <- if ("treatment" %in% names(md)) "treatment" else x$info$treatment
  cobalt::bal.tab(md, treat = treat_var, weights = "weights", ...)
}
