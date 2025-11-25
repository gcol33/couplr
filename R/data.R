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

#' Hospital staff scheduling example dataset
#'
#' A comprehensive example dataset for demonstrating couplr functionality
#' across vignettes. Contains hospital staff scheduling data with nurses,
#' shifts, costs, and preference scores suitable for assignment problems,
#' as well as nurse characteristics for matching workflows.
#'
#' This dataset is used throughout the couplr documentation to provide
#' a consistent, realistic example that evolves in complexity.
#'
#' @format A list containing several related datasets:
#' \describe{
#'   \item{basic_costs}{A 10x10 cost matrix for assigning 10 nurses to 10 shifts.
#'     Lower values indicate better fit (less overtime, matches skills).}
#'   \item{preferences}{A 10x10 preference matrix (0-10 scale, higher = more preferred).
#'     Use with \code{maximize = TRUE}.
#'   }
#'   \item{schedule_df}{A tibble with 100 rows (10 nurses x 10 shifts) containing:
#'     \describe{
#'       \item{nurse_id}{Nurse identifier (1-10)}
#'       \item{shift_id}{Shift identifier (1-10)}
#'       \item{cost}{Assignment cost}
#'       \item{preference}{Nurse preference score (0-10)}
#'       \item{skill_match}{Whether nurse skills match shift needs (0/1)}
#'     }
#'   }
#'   \item{nurses}{A tibble with 10 rows describing nurse characteristics:
#'     \describe{
#'       \item{nurse_id}{Nurse identifier (1-10)}
#'       \item{experience_years}{Years of experience (1-20)}
#'       \item{department}{Primary department (ICU, ER, General, Pediatrics)}
#'       \item{shift_preference}{Preferred shift type (day, evening, night)}
#'       \item{certification_level}{Certification level (1-3)}
#'     }
#'   }
#'   \item{shifts}{A tibble with 10 rows describing shift requirements:
#'     \describe{
#'       \item{shift_id}{Shift identifier (1-10)}
#'       \item{department}{Department needing coverage}
#'       \item{shift_type}{Shift type (day, evening, night)}
#'       \item{min_experience}{Minimum years of experience required}
#'       \item{min_certification}{Minimum certification level required}
#'     }
#'   }
#'   \item{weekly_df}{A tibble for batch solving: 5 days x 10 nurses x 10 shifts.
#'     Contains columns: day, nurse_id, shift_id, cost, preference.
#'   }
#'   \item{nurses_extended}{A tibble with 200 nurses for matching examples.
#'     Contains: nurse_id, age, experience_years, hourly_rate, department,
#'     certification_level, is_fulltime.
#'   }
#'   \item{controls_extended}{A tibble with 300 potential control nurses for
#'     matching examples. Same structure as nurses_extended.
#'   }
#' }
#'
#' @details
#' The dataset is designed to demonstrate progressively complex scenarios:
#'
#' \strong{Basic LAP} (\code{vignette("getting-started")}):
#' \itemize{
#'   \item \code{basic_costs}: Simple 10x10 assignment
#'   \item \code{preferences}: Maximization problem
#'   \item \code{schedule_df}: Data frame input, grouped workflows
#'   \item \code{weekly_df}: Batch solving across days
#' }
#'
#' \strong{Algorithm comparison} (\code{vignette("algorithms")}):
#' \itemize{
#'   \item Use \code{basic_costs} to compare algorithm behavior
#'   \item Modify with NA values for sparse scenarios
#' }
#'
#' \strong{Matching workflows} (\code{vignette("matching-workflows")}):
#' \itemize{
#'   \item \code{nurses_extended}: Treatment group (full-time nurses)
#'   \item \code{controls_extended}: Control pool (part-time/registry nurses)
#'   \item Match on age, experience, department for causal analysis
#' }
#'
#' @examples
#' # Basic assignment: assign nurses to shifts minimizing cost
#' lap_solve(hospital_staff$basic_costs)
#'
#' # Maximize preferences instead
#' lap_solve(hospital_staff$preferences, maximize = TRUE)
#'
#' # Data frame workflow
#' library(dplyr)
#' hospital_staff$schedule_df |>
#'   lap_solve(nurse_id, shift_id, cost)
#'
#' # Batch solve weekly schedule
#' hospital_staff$weekly_df |>
#'   group_by(day) |>
#'   lap_solve(nurse_id, shift_id, cost)
#'
#' # Matching workflow: match full-time to part-time nurses
#' match_couples(
#'   left = hospital_staff$nurses_extended,
#'   right = hospital_staff$controls_extended,
#'   vars = c("age", "experience_years", "certification_level"),
#'   auto_scale = TRUE
#' )
#'
#' @seealso
#' \code{\link{lap_solve}} for basic assignment solving,
#' \code{\link{lap_solve_batch}} for batch solving,
#' \code{\link{match_couples}} for matching workflows,
#' \code{vignette("getting-started")} for introductory tutorial
#'
#' @export
hospital_staff <- local({
  set.seed(42)  # Reproducible example data


  # Basic 10x10 cost matrix
  # Costs represent: travel time + skill mismatch + overtime likelihood
  basic_costs <- matrix(
    c(
      3, 7, 5, 9, 4, 8, 6, 2, 7, 5,
      6, 2, 8, 4, 7, 3, 9, 5, 4, 6,
      5, 4, 3, 7, 8, 6, 2, 9, 5, 4,
      8, 6, 7, 2, 5, 4, 3, 6, 8, 7,
      4, 9, 6, 5, 3, 7, 8, 4, 2, 9,
      7, 3, 4, 8, 6, 2, 5, 7, 9, 3,
      2, 8, 9, 6, 4, 5, 7, 3, 6, 8,
      9, 5, 2, 3, 7, 8, 4, 6, 3, 2,
      6, 4, 5, 7, 9, 3, 6, 8, 4, 5,
      5, 6, 4, 8, 2, 9, 3, 5, 7, 6
    ),
    nrow = 10, byrow = TRUE,
    dimnames = list(
      paste0("Nurse_", 1:10),
      paste0("Shift_", 1:10)
    )
  )

  # Preference matrix (0-10, higher = more preferred)
  preferences <- matrix(
    c(
      8, 4, 6, 2, 7, 3, 5, 9, 4, 6,
      5, 9, 3, 7, 4, 8, 2, 6, 7, 5,
      6, 7, 8, 4, 3, 5, 9, 2, 6, 7,
      3, 5, 4, 9, 6, 7, 8, 5, 3, 4,
      7, 2, 5, 6, 8, 4, 3, 7, 9, 2,
      4, 8, 7, 3, 5, 9, 6, 4, 2, 8,
      9, 3, 2, 5, 7, 6, 4, 8, 5, 3,
      2, 6, 9, 8, 4, 3, 7, 5, 8, 9,
      5, 7, 6, 4, 2, 8, 5, 3, 7, 6,
      6, 5, 7, 3, 9, 2, 8, 6, 4, 5
    ),
    nrow = 10, byrow = TRUE,
    dimnames = list(
      paste0("Nurse_", 1:10),
      paste0("Shift_", 1:10)
    )
  )

  # Schedule data frame (long format)
  schedule_df <- tibble::tibble(
    nurse_id = rep(1:10, each = 10),
    shift_id = rep(1:10, times = 10),
    cost = as.vector(t(basic_costs)),
    preference = as.vector(t(preferences)),
    skill_match = sample(0:1, 100, replace = TRUE, prob = c(0.3, 0.7))
  )

  # Nurse characteristics
  departments <- c("ICU", "ER", "General", "Pediatrics")
  shift_types <- c("day", "evening", "night")

  nurses <- tibble::tibble(
    nurse_id = 1:10,
    experience_years = c(5, 12, 3, 8, 15, 2, 10, 7, 4, 11),
    department = c("ICU", "ER", "General", "ICU", "Pediatrics",
                   "General", "ER", "ICU", "Pediatrics", "General"),
    shift_preference = c("day", "night", "day", "evening", "day",
                         "evening", "night", "day", "evening", "night"),
    certification_level = c(2, 3, 1, 2, 3, 1, 3, 2, 1, 2)
  )

  # Shift requirements
  shifts <- tibble::tibble(
    shift_id = 1:10,
    department = c("ICU", "ICU", "ER", "ER", "General",
                   "General", "Pediatrics", "Pediatrics", "ICU", "ER"),
    shift_type = c("day", "night", "day", "evening", "day",
                   "night", "day", "evening", "evening", "night"),
    min_experience = c(3, 5, 2, 2, 1, 1, 2, 2, 4, 3),
    min_certification = c(2, 2, 1, 1, 1, 1, 1, 1, 2, 2)
  )

  # Weekly schedule for batch solving (5 days)
  days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  weekly_df <- do.call(rbind, lapply(seq_along(days), function(d) {
    tibble::tibble(
      day = days[d],
      nurse_id = rep(1:10, each = 10),
      shift_id = rep(1:10, times = 10),
      cost = as.vector(t(basic_costs)) + sample(-2:2, 100, replace = TRUE),
      preference = pmax(0, pmin(10, as.vector(t(preferences)) + sample(-1:1, 100, replace = TRUE)))
    )
  }))
  weekly_df$day <- factor(weekly_df$day, levels = days)

  # Extended nurses for matching examples (200 full-time nurses = "treatment")
  nurses_extended <- tibble::tibble(
    nurse_id = 1:200,
    age = round(stats::rnorm(200, mean = 38, sd = 10)),
    experience_years = pmax(0, round(stats::rnorm(200, mean = 8, sd = 5))),
    hourly_rate = round(stats::rnorm(200, mean = 35, sd = 8), 2),
    department = sample(departments, 200, replace = TRUE,
                        prob = c(0.25, 0.30, 0.30, 0.15)),
    certification_level = sample(1:3, 200, replace = TRUE,
                                 prob = c(0.3, 0.5, 0.2)),
    is_fulltime = TRUE
  )
  # Ensure realistic age bounds
  nurses_extended$age <- pmax(22, pmin(65, nurses_extended$age))

  # Control pool for matching (300 part-time/registry nurses)
  # Slightly different distribution to create matching challenge
  controls_extended <- tibble::tibble(
    nurse_id = 201:500,
    age = round(stats::rnorm(300, mean = 42, sd = 12)),
    experience_years = pmax(0, round(stats::rnorm(300, mean = 6, sd = 6))),
    hourly_rate = round(stats::rnorm(300, mean = 32, sd = 10), 2),
    department = sample(departments, 300, replace = TRUE,
                        prob = c(0.20, 0.25, 0.40, 0.15)),
    certification_level = sample(1:3, 300, replace = TRUE,
                                 prob = c(0.4, 0.4, 0.2)),
    is_fulltime = FALSE
  )
  controls_extended$age <- pmax(22, pmin(65, controls_extended$age))

  list(
    basic_costs = basic_costs,
    preferences = preferences,
    schedule_df = schedule_df,
    nurses = nurses,
    shifts = shifts,
    weekly_df = weekly_df,
    nurses_extended = nurses_extended,
    controls_extended = controls_extended
  )
})
