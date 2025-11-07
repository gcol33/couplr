#' Example cost matrices for assignment problems
#'
#' Small example datasets for demonstrating assignR functionality
#'
#' @format A list containing several example cost matrices:
#' \describe{
#'   \item{simple_3x3}{A simple 3x3 cost matrix}
#'   \item{rectangular_3x5}{A 3x5 rectangular cost matrix}
#'   \item{sparse_with_na}{A matrix with NA values indicating forbidden assignments}
#'   \item{binary_costs}{A matrix with binary (0/1) costs}
#' }
#'
#' @examples
#' # Use simple example
#' lap_solve(example_costs$simple_3x3)
#'
#' # Rectangular problem
#' lap_solve(example_costs$rectangular_3x5)
#'
#' # With forbidden assignments
#' lap_solve(example_costs$sparse_with_na)
#'
#' @export
example_costs <- list(
  simple_3x3 = matrix(c(
    4, 2, 5,
    3, 3, 6,
    7, 5, 4
  ), nrow = 3, byrow = TRUE),
  
  rectangular_3x5 = matrix(c(
    1, 2, 3, 4, 5,
    6, 5, 4, 3, 2,
    2, 3, 4, 5, 6
  ), nrow = 3, byrow = TRUE),
  
  sparse_with_na = matrix(c(
    4, 2, NA,
    3, NA, 6,
    NA, 5, 4
  ), nrow = 3, byrow = TRUE),
  
  binary_costs = matrix(c(
    0, 1, 1,
    1, 0, 1,
    1, 1, 0
  ), nrow = 3, byrow = TRUE)
)

#' Example assignment problem data frame
#'
#' A tidy data frame representation of assignment problems, suitable for
#' use with grouped workflows.
#'
#' @format A tibble with 18 rows and 4 columns:
#' \describe{
#'   \item{sim}{Simulation/problem identifier (1 or 2)}
#'   \item{source}{Source node index (1, 2, or 3)}
#'   \item{target}{Target node index (1, 2, or 3)}
#'   \item{cost}{Cost of assignment}
#' }
#'
#' @examples
#' library(dplyr)
#'
#' # Solve both problems
#' example_df |>
#'   group_by(sim) |>
#'   lap_solve(source, target, cost)
#'
#' # Or use batch solving
#' example_df |>
#'   group_by(sim) |>
#'   lap_solve_batch(source, target, cost)
#'
#' @export
example_df <- tibble::tibble(
  sim = rep(1:2, each = 9),
  source = rep(1:3, times = 6),
  target = rep(1:3, each = 3, times = 2),
  cost = c(
    # Simulation 1
    4, 2, 5,
    3, 3, 6,
    7, 5, 4,
    # Simulation 2
    1, 2, 3,
    4, 3, 2,
    5, 4, 1
  )
)
