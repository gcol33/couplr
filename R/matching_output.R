# ==============================================================================
# Unified Output Layer - match_data() generic
# ==============================================================================

#' Extract Analysis-Ready Data from Matching Results
#'
#' A generic function that converts any couplr matching result into a single
#' analysis-ready data frame with \code{weights}, \code{subclass}, and
#' \code{distance} columns. This is the couplr equivalent of MatchIt's
#' \code{match.data()}.
#'
#' @param result A couplr result object (matching_result, full_matching_result,
#'   cem_result, or subclass_result)
#' @param ... Additional arguments passed to methods
#'
#' @return A tibble with all original variables plus standardized columns:
#' \describe{
#'   \item{id}{Unit identifier}
#'   \item{treatment}{1 for left/treated, 0 for right/control}
#'   \item{weights}{Matching weights}
#'   \item{subclass}{Matched group/stratum identifier}
#'   \item{distance}{Matching distance (where applicable)}
#' }
#'
#' @details
#' The output format is compatible with downstream packages like \pkg{cobalt},
#' \pkg{WeightIt}, and \pkg{marginaleffects}. The stacked (long) format with
#' \code{treatment} and \code{weights} columns is the standard layout expected
#' by these tools.
#'
#' @examples
#' set.seed(42)
#' left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
#' right <- data.frame(id = 6:15, age = runif(10, 20, 70))
#' result <- match_couples(left, right, vars = "age")
#' md <- match_data(result, left, right)
#' head(md)
#'
#' @export
match_data <- function(result, ...) {
  UseMethod("match_data")
}


#' @rdname match_data
#' @param left Data frame of left (treated) units
#' @param right Data frame of right (control) units
#' @param left_id Name of ID column in left (default: \code{"id"})
#' @param right_id Name of ID column in right (default: \code{"id"})
#' @export
match_data.matching_result <- function(result, left, right,
                                       left_id = "id", right_id = "id",
                                       ...) {
  pairs <- result$pairs
  if (nrow(pairs) == 0) {
    return(tibble::tibble())
  }

  matched_left_ids <- as.character(pairs$left_id)
  matched_right_ids <- as.character(pairs$right_id)

  # Build left portion
  left_rows <- left[as.character(left[[left_id]]) %in% matched_left_ids, ,
                    drop = FALSE]
  left_rows$treatment <- 1L
  left_rows$weights <- 1.0

  # Compute subclass (pair_id)
  pair_lookup <- stats::setNames(seq_len(nrow(pairs)), matched_left_ids)
  left_rows$subclass <- pair_lookup[as.character(left_rows[[left_id]])]

  # Distance for left
  dist_lookup <- stats::setNames(pairs$distance, matched_left_ids)
  left_rows$distance <- dist_lookup[as.character(left_rows[[left_id]])]

  # Build right portion
  right_rows <- right[as.character(right[[right_id]]) %in% matched_right_ids, ,
                      drop = FALSE]
  right_rows$treatment <- 0L
  # Handle replacement matching: a right unit may appear multiple times
  right_pair_info <- stats::setNames(
    seq_len(nrow(pairs)), matched_right_ids
  )
  right_rows$weights <- 1.0
  right_rows$subclass <- right_pair_info[as.character(right_rows[[right_id]])]
  right_rows$distance <- dist_lookup[matched_left_ids[right_rows$subclass]]

  # Harmonize ID column names
  if (left_id != right_id) {
    names(left_rows)[names(left_rows) == left_id] <- "id"
    names(right_rows)[names(right_rows) == right_id] <- "id"
  }

  tibble::as_tibble(rbind(left_rows, right_rows))
}


#' @rdname match_data
#' @export
match_data.full_matching_result <- function(result, left, right,
                                            left_id = "id", right_id = "id",
                                            ...) {
  groups <- result$groups
  if (nrow(groups) == 0) return(tibble::tibble())

  # Left units
  left_groups <- groups[groups$side == "left", ]
  left_rows <- left[as.character(left[[left_id]]) %in% left_groups$id, ,
                    drop = FALSE]
  left_match <- merge(
    data.frame(id_chr = as.character(left_rows[[left_id]]),
               stringsAsFactors = FALSE),
    data.frame(id_chr = left_groups$id, subclass = left_groups$group_id,
               weight = left_groups$weight, stringsAsFactors = FALSE),
    by = "id_chr"
  )
  left_rows$treatment <- 1L
  left_rows$weights <- left_match$weight
  left_rows$subclass <- left_match$subclass
  left_rows$distance <- NA_real_

  # Right units
  right_groups <- groups[groups$side == "right", ]
  right_rows <- right[as.character(right[[right_id]]) %in% right_groups$id, ,
                      drop = FALSE]
  right_match <- merge(
    data.frame(id_chr = as.character(right_rows[[right_id]]),
               stringsAsFactors = FALSE),
    data.frame(id_chr = right_groups$id, subclass = right_groups$group_id,
               weight = right_groups$weight, stringsAsFactors = FALSE),
    by = "id_chr"
  )
  right_rows$treatment <- 0L
  right_rows$weights <- right_match$weight
  right_rows$subclass <- right_match$subclass
  right_rows$distance <- NA_real_

  # Harmonize
  if (left_id != right_id) {
    names(left_rows)[names(left_rows) == left_id] <- "id"
    names(right_rows)[names(right_rows) == right_id] <- "id"
  }

  tibble::as_tibble(rbind(left_rows, right_rows))
}


#' @rdname match_data
#' @param data Data frame containing all units (for CEM and subclassification,
#'   left and right are not always needed separately)
#' @export
match_data.cem_result <- function(result, left, right,
                                  left_id = "id", right_id = "id",
                                  ...) {
  matched <- result$matched

  # Left units
  left_matched <- matched[matched$side == "left", ]
  left_rows <- left[as.character(left[[left_id]]) %in% left_matched$id, ,
                    drop = FALSE]
  lm_info <- merge(
    data.frame(id_chr = as.character(left_rows[[left_id]]),
               stringsAsFactors = FALSE),
    data.frame(id_chr = left_matched$id, weight = left_matched$weight,
               subclass = left_matched$stratum, stringsAsFactors = FALSE),
    by = "id_chr"
  )
  left_rows$treatment <- 1L
  left_rows$weights <- lm_info$weight
  left_rows$subclass <- lm_info$subclass
  left_rows$distance <- NA_real_

  # Right units
  right_matched <- matched[matched$side == "right", ]
  right_rows <- right[as.character(right[[right_id]]) %in% right_matched$id, ,
                      drop = FALSE]
  rm_info <- merge(
    data.frame(id_chr = as.character(right_rows[[right_id]]),
               stringsAsFactors = FALSE),
    data.frame(id_chr = right_matched$id, weight = right_matched$weight,
               subclass = right_matched$stratum, stringsAsFactors = FALSE),
    by = "id_chr"
  )
  right_rows$treatment <- 0L
  right_rows$weights <- rm_info$weight
  right_rows$subclass <- rm_info$subclass
  right_rows$distance <- NA_real_

  # Harmonize
  if (left_id != right_id) {
    names(left_rows)[names(left_rows) == left_id] <- "id"
    names(right_rows)[names(right_rows) == right_id] <- "id"
  }

  # Only include matched units (weight > 0)
  out <- tibble::as_tibble(rbind(left_rows, right_rows))
  out[out$weights > 0, ]
}


#' @rdname match_data
#' @export
match_data.subclass_result <- function(result, data = NULL, ...) {
  matched <- result$matched

  if (!is.null(data)) {
    # Merge in original data columns
    out <- merge(data, matched[, c("id", "subclass", "ps", "weight")],
                 by = "id", all.x = FALSE)
    out$treatment <- ifelse(matched$side[match(out$id, matched$id)] == "left",
                            1L, 0L)
    out$weights <- out$weight
    out$weight <- NULL
    out$distance <- out$ps
  } else {
    out <- matched
    out$treatment <- ifelse(out$side == "left", 1L, 0L)
    out$weights <- out$weight
    out$weight <- NULL
    out$distance <- out$ps
  }

  # Only include units with weight > 0
  out <- out[out$weights > 0, ]
  tibble::as_tibble(out)
}
