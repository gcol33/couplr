#' @keywords internal
"_PACKAGE"

#' assignr: Fast linear assignment solver for R
#'
#' Modern, tidy implementations of the Hungarian and Jonker–Volgenant algorithms
#' for solving linear assignment problems.
#'
#' @section Main functions:
#' \itemize{
#'   \item{\code{\link{lap_solve}}}: Solve single assignment problems
#'   \item{\code{\link{lap_solve_batch}}}: Solve multiple problems efficiently
#'   \item{\code{\link{lap_solve_kbest}}}: Find k-best optimal solutions
#' }
#'
#' @name assignr-package
#' @aliases assignr
#' @useDynLib assignr, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom rlang enquo quo_is_null eval_tidy
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr group_by group_vars group_keys group_split bind_rows bind_cols summarise first n arrange
#' @importFrom purrr map_dfr
#' @importFrom magrittr %>%
NULL

#' Re-export of dplyr::group_by
#'
#' @name group_by
#' @rdname group_by
#' @keywords internal
#' @export
#' @importFrom dplyr group_by
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
