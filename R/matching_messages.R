# ==============================================================================
# Fun Error and Warning Messages
# ==============================================================================

#' Couplr message helpers with emoji and humor
#'
#' Light, fun error/warning messages inspired by testthat, themed around
#' coupling and matching. Makes errors less intimidating and more memorable.
#'
#' @name couplr_messages
#' @keywords internal
NULL

# Message styling helpers -----------------------------------------------------

#' Check if emoji should be used
#' @return Logical indicating whether emoji should be used.
#' @keywords internal
use_emoji <- function() {
  # Check if user has disabled emoji via option
  opt <- getOption("couplr.emoji", default = TRUE)
  if (!opt) return(FALSE)

  # Check if in interactive session (emoji look better in console)
  interactive()
}

#' Get a themed emoji
#' @return Character string with the emoji (or empty string if emoji disabled).
#' @keywords internal
couplr_emoji <- function(type = c("error", "warning", "info", "success",
                                   "heart", "broken", "sparkles", "search",
                                   "chart", "warning_sign", "stop", "check")) {
  if (!use_emoji()) return("")

  type <- match.arg(type)

  emojis <- list(
    error = "\U0001F494",         # 💔 Broken heart
    warning = "\U0001F48C",        # 💌 Love letter (warning)
    info = "\U0001F4AC",           # 💬 Speech balloon
    success = "\U0001F496",        # 💖 Sparkling heart
    heart = "\U00002764",          # ❤️ Red heart
    broken = "\U0001F494",         # 💔 Broken heart
    sparkles = "\U00002728",       # ✨ Sparkles
    search = "\U0001F50D",         # 🔍 Magnifying glass
    chart = "\U0001F4CA",          # 📊 Bar chart
    warning_sign = "\U000026A0",   # ⚠️ Warning
    stop = "\U0001F6D1",           # 🛑 Stop sign
    check = "\U00002714"           # ✔️ Check mark
  )

  paste0(emojis[[type]], " ")
}

# Couple-themed error message helpers -----------------------------------------

#' Stop with a fun, themed error message
#' @return No return value, throws an error.
#' @keywords internal
couplr_stop <- function(..., call. = FALSE) {
  msg <- paste0(couplr_emoji("error"), ...)
  stop(msg, call. = call.)
}

#' Warn with a fun, themed warning message
#' @return No return value, called for side effects (issues a warning).
#' @keywords internal
couplr_warn <- function(..., call. = FALSE) {
  msg <- paste0(couplr_emoji("warning"), ...)
  warning(msg, call. = call.)
}

#' Info message with emoji
#' @return No return value, called for side effects (issues a message).
#' @keywords internal
couplr_inform <- function(...) {
  msg <- paste0(couplr_emoji("info"), ...)
  message(msg)
}

#' Success message with emoji
#' @return No return value, called for side effects (issues a message).
#' @keywords internal
couplr_success <- function(...) {
  msg <- paste0(couplr_emoji("success"), ...)
  message(msg)
}

# Specific error messages -----------------------------------------------------

#' Missing data error
#' @return No return value, throws an error.
#' @keywords internal
err_missing_data <- function(dataset = "left") {
  couplr_stop(
    "No matches made - ", dataset, " dataset is empty!\n",
    "  ", couplr_emoji("info"), "Can't couple without candidates. Add some data!"
  )
}

#' Missing variables error
#' @return No return value, throws an error.
#' @keywords internal
err_missing_vars <- function(vars, dataset = "left") {
  couplr_stop(
    "Missing variables in ", dataset, " dataset: ", paste(vars, collapse = ", "), "\n",
    "  ", couplr_emoji("search"), "Variables needed for matching not found.\n",
    "  ", couplr_emoji("info"), "Check your column names or use vars = c(...) to specify."
  )
}

#' Invalid parameter error
#' @return No return value, throws an error.
#' @keywords internal
err_invalid_param <- function(param, value, expected) {
  couplr_stop(
    "Parameter '", param, "' has invalid value: ", value, "\n",
    "  ", couplr_emoji("info"), "Expected: ", expected
  )
}

#' All pairs forbidden error
#' @return No return value, throws an error.
#' @keywords internal
err_no_valid_pairs <- function(reason = NULL) {
  msg <- "No valid pairs to match: every distance is excluded by the matching restrictions"
  if (!is.null(reason)) {
    msg <- paste0(msg, " (", reason, ")")
  }
  couplr_stop(msg, ".
",
              "Widen max_distance or the calipers, or use fewer blocking variables.")
}

#' Constant variable warning
#' @return No return value, called for side effects (issues a warning).
#' @keywords internal
warn_constant_var <- function(var) {
  couplr_warn(
    "Variable '", var, "' is constant (SD = 0) and is excluded from matching."
  )
}

#' Too many zeros warning
#' @return No return value, called for side effects (issues a warning).
#' @keywords internal
warn_many_zeros <- function(pct, n_zeros) {
  couplr_warn(
    sprintf("%.1f%% of distances are zero (%d pairs);
", pct, n_zeros),
    "the data may hold duplicate records or too few distinguishing variables."
  )
}

#' Extreme cost ratio warning
#' @return No return value, called for side effects (issues a warning).
#' @keywords internal
warn_extreme_costs <- function(p95, p99, ratio, problem_vars = NULL) {
  msg <- sprintf(paste0(
    "The distances are highly skewed: the 99th percentile (%.2f) is %.0f times ",
    "the 95th (%.2f)"), p99, ratio, p95)
  if (!is.null(problem_vars) && length(problem_vars) > 0) {
    msg <- paste0(msg, ";
variables with extreme values: ",
                  paste(problem_vars, collapse = ", "))
  }
  couplr_warn(msg, ".")
}

#' Many forbidden pairs warning
#'
#' Raised when the restrictions exclude more than 90% of the candidate pairs,
#' the range in which a complete matching may no longer exist.
#' @return No return value, called for side effects (issues a warning).
#' @keywords internal
warn_many_forbidden <- function(pct_forbidden, n_valid, n_left) {
  couplr_warn(sprintf(paste0(
    "%.1f%% of candidate pairs are excluded by the matching restrictions;\n",
    "%d admissible pairs remain for %d left units."),
    pct_forbidden, n_valid, n_left))
}

#' All distances identical warning
#' @return No return value, called for side effects (issues a warning).
#' @keywords internal
warn_constant_distance <- function(value) {
  couplr_warn(
    sprintf("All distances are identical (%.4f);
", value),
    "the matching variables do not distinguish the units."
  )
}

#' Low match rate info
#' @return No return value, called for side effects (issues a message or warning).
#' @keywords internal
info_low_match_rate <- function(n_matched, n_left, pct) {
  if (pct < 25) {
    couplr_warn(
      sprintf("Only %d of %d left units (%.1f%%) were matched;
",
              n_matched, n_left, pct),
      "the restrictions may be too strict or the two groups may overlap little."
    )
  } else if (pct < 50) {
    couplr_inform(
      sprintf("%d of %d left units (%.1f%%) were matched.", n_matched, n_left, pct)
    )
  }
}

#' High distance matches warning
#' @return No return value, called for side effects (issues a warning).
#' @keywords internal
warn_poor_quality <- function(pct_poor, threshold) {
  couplr_warn(
    sprintf("%.1f%% of matched pairs have a distance above %.2f.",
            pct_poor, threshold)
  )
}

#' Parallel package missing warning (reuse from matching_parallel.R)
#' @return No return value, called for side effects (issues a warning).
#' @keywords internal
warn_parallel_unavailable <- function() {
  couplr_warn(
    "Parallel processing needs the packages 'future' and 'future.apply';
",
    "running sequentially."
  )
}

#' Perfect balance success message
#' @return No return value, called for side effects (issues a message).
#' @keywords internal
success_good_balance <- function(mean_std_diff) {
  if (mean_std_diff < 0.1) {
    couplr_success(
      sprintf("Excellent balance! Mean standardized difference: %.3f", mean_std_diff), "\n",
      "  ", couplr_emoji("heart"), "These couples are well-matched!"
    )
  }
}

# Cost distribution checking --------------------------------------------------

#' Check cost distribution for problems
#'
#' Examines the distance matrix for common issues and provides helpful warnings.
#'
#' @param cost_matrix Numeric matrix of distances
#' @param threshold_zero Threshold for considering distance "zero" (default: 1e-10)
#' @param warn If TRUE, issue warnings for problems found
#' @return List with diagnostic information
#' @keywords internal
check_cost_distribution <- function(cost_matrix, threshold_zero = 1e-10, warn = TRUE) {
  if (is_lazy_cost_spec(cost_matrix)) {
    # The checks below read a stored matrix. A lazy specification stores
    # none, and evaluating every pair here would cost the n*m distance
    # evaluations lazy mode exists to avoid.
    return(list(valid = TRUE, n_finite = NA_integer_, n_total = NA_integer_))
  }

  # One pass over the matrix in place: the admissible costs are never copied,
  # and the quantiles are those quantile() would give on them.
  s <- lap_cost_summary(cost_matrix, BIG_COST, threshold_zero,
                        c(0.5, 0.95, 0.99))
  n_finite <- s$n_admissible
  n_total <- s$n_total

  if (n_finite == 0) {
    if (warn) {
      err_no_valid_pairs("All distances are Inf (likely no valid pairs exist)")
    }
    return(list(
      valid = FALSE,
      n_finite = 0,
      n_total = n_total,
      n_infinite = n_total,
      n_zeros = 0,
      pct_zeros = 0,
      pct_forbidden = if (n_total > 0) 100 else NA_real_,
      min = NA_real_, max = NA_real_, median = NA_real_,
      p95 = NA_real_, p99 = NA_real_
    ))
  }

  n_infinite <- n_total - n_finite
  pct_forbidden <- 100 * n_infinite / n_total

  # Check for too many zeros
  n_zeros <- s$n_zeros
  pct_zeros <- 100 * n_zeros / n_finite

  if (warn && pct_zeros > 10) {
    warn_many_zeros(pct_zeros, n_zeros)
  }

  # Check for constant distances
  if (s$min == s$max) {
    if (warn) {
      warn_constant_distance(s$min)
    }
  }

  # Check for extreme cost ratios
  p95 <- p99 <- NA
  if (n_finite > 10) {
    p95 <- s$quantiles[2]
    p99 <- s$quantiles[3]

    if (p95 > 0 && p99 / p95 > 10) {
      if (warn) {
        warn_extreme_costs(p95, p99, p99 / p95)
      }
    }
  }

  # Check for too many forbidden pairs
  if (warn && pct_forbidden > 90) {
    warn_many_forbidden(pct_forbidden, n_finite, nrow(cost_matrix))
  }

  list(
    valid = TRUE,
    n_finite = n_finite,
    n_total = n_total,
    n_infinite = n_infinite,
    n_zeros = n_zeros,
    pct_zeros = pct_zeros,
    pct_forbidden = pct_forbidden,
    min = s$min,
    max = s$max,
    median = s$quantiles[1],
    p95 = p95,
    p99 = p99
  )
}

#' Diagnose a Distance Matrix
#'
#' Comprehensive diagnostics for a distance matrix with actionable suggestions.
#'
#' @param cost_matrix Numeric matrix of distances
#' @param left Left dataset (for variable checking)
#' @param right Right dataset (for variable checking)
#' @param vars Variables used for matching
#' @param warn If TRUE, issue warnings
#' @return A `distance_diagnostics` list with the cost distribution,
#'   variable issues, problem variables, suggestions, and quality assessment.
#'   `summary()` returns these findings as `summary.distance_diagnostics`.
#' @export
diagnose_distance_matrix <- function(cost_matrix, left = NULL, right = NULL,
                                     vars = NULL, warn = TRUE) {
  if (!is.matrix(cost_matrix) || !is.numeric(cost_matrix)) {
    stop("cost_matrix must be a numeric matrix.", call. = FALSE)
  }

  # Basic distribution check
  dist_diag <- check_cost_distribution(cost_matrix, warn = warn)

  # Variable-specific diagnostics if data provided
  var_issues <- list()
  problem_vars <- character(0)

  if (!is.null(left) && !is.null(right) && !is.null(vars)) {
    for (v in vars) {
      if (v %in% names(left) && v %in% names(right)) {
        # Check for constants
        sd_left <- sd(left[[v]], na.rm = TRUE)
        sd_right <- sd(right[[v]], na.rm = TRUE)

        if (is.na(sd_left) || sd_left < 1e-10) {
          var_issues[[v]] <- "constant in left"
          problem_vars <- c(problem_vars, v)
          if (warn) warn_constant_var(v)
        } else if (is.na(sd_right) || sd_right < 1e-10) {
          var_issues[[v]] <- "constant in right"
          problem_vars <- c(problem_vars, v)
          if (warn) warn_constant_var(v)
        } else {
          # Check for extreme scale differences
          range_left <- diff(range(left[[v]], na.rm = TRUE))
          range_right <- diff(range(right[[v]], na.rm = TRUE))

          if (range_left > 0 && range_right > 0) {
            scale_ratio <- max(range_left, range_right) / min(range_left, range_right)
            if (scale_ratio > 100) {
              var_issues[[v]] <- sprintf("extreme scale difference (%.0fx)", scale_ratio)
              problem_vars <- c(problem_vars, v)
            }
          }
        }
      }
    }
  }

  # Build suggestions
  suggestions <- character(0)

  if (isTRUE(dist_diag$pct_forbidden > 75)) {
    suggestions <- c(suggestions, "Relax constraints (max_distance or calipers)")
  }

  if (dist_diag$pct_zeros > 10) {
    suggestions <- c(suggestions, "Check for duplicates or add distinguishing variables")
  }

  if (!is.na(dist_diag$p99) && !is.na(dist_diag$p95) &&
      dist_diag$p95 > 0 && dist_diag$p99 / dist_diag$p95 > 10) {
    suggestions <- c(suggestions, "Use scaling (scale = 'standardize' or 'robust')")
  }

  if (length(problem_vars) > 0) {
    suggestions <- c(suggestions, "Remove or transform problematic variables")
  }

  structure(list(
    distribution = dist_diag,
    variable_issues = var_issues,
    problem_variables = problem_vars,
    suggestions = suggestions,
    quality = if (dist_diag$valid && dist_diag$pct_forbidden < 50 &&
                  dist_diag$pct_zeros < 10) "good" else
              if (dist_diag$valid) "fair" else "poor"
  ), class = "distance_diagnostics")
}

#' @rdname diagnose_distance_matrix
#' @param x,object A `distance_diagnostics` object.
#' @param ... Ignored.
#' @export
print.distance_diagnostics <- function(x, ...) {
  .print_distance_diagnostics_overview(x)
  invisible(x)
}

.print_distance_diagnostics_overview <- function(x) {
  cat("Distance Matrix Diagnostics\n")
  cat("Quality:", x$quality, "\n")
  cat("Admissible pairs:", x$distribution$n_finite, "of", x$distribution$n_total, "\n")
  cat("Problem variables:", length(x$problem_variables), "\n")
  cat("Suggestions:", length(x$suggestions), "\n")
}

#' @rdname diagnose_distance_matrix
#' @export
summary.distance_diagnostics <- function(object, ...) {
  structure(unclass(object), class = "summary.distance_diagnostics")
}

#' @export
print.summary.distance_diagnostics <- function(x, ...) {
  .print_distance_diagnostics_overview(x)
  cat("\nCost distribution:\n")
  print(data.frame(statistic = names(x$distribution),
    value = vapply(x$distribution, function(value) paste(value, collapse = ", "), "")),
    row.names = FALSE)
  if (length(x$variable_issues)) {
    cat("\nVariable issues:\n")
    print(data.frame(variable = names(x$variable_issues),
                     issue = unlist(x$variable_issues, use.names = FALSE)),
          row.names = FALSE)
  }
  if (length(x$suggestions)) {
    cat("\nSuggestions:\n")
    cat(x$suggestions, sep = "\n")
    cat("\n")
  }
  invisible(x)
}
