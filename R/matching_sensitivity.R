# ==============================================================================
# Rosenbaum Sensitivity Analysis for Matched Pairs
# ==============================================================================

#' Rosenbaum Sensitivity Analysis
#'
#' Assesses how sensitive a matched comparison is to hidden bias using
#' Rosenbaum bounds on the Wilcoxon signed-rank statistic.
#'
#' @param result A matching_result object from [match_couples()]
#' @param left Original left (treated) dataset
#' @param right Original right (control) dataset
#' @param outcome_var Name of the outcome column in `left` and `right`
#' @param gamma Numeric vector of sensitivity parameters
#'   (default: seq(1, 3, by = 0.25)). Gamma = 1 means no hidden bias.
#' @param alternative Direction of the test: "greater" (default), "less",
#'   or "two.sided"
#' @param left_id Name of ID column in left (default: "id")
#' @param right_id Name of ID column in right (default: "id")
#'
#' @return An S3 object of class `sensitivity_analysis` containing:
#' \describe{
#'   \item{results}{Tibble with columns: gamma, t_stat, p_upper, p_lower}
#'   \item{n_pairs}{Number of matched pairs analyzed}
#'   \item{critical_gamma}{Smallest gamma at which p_upper > 0.05}
#'   \item{alternative}{Direction of test}
#' }
#'
#' @details
#' Rosenbaum (2002, Chapter 4) bounds quantify how much hidden bias (an
#' unobserved confounder) would be needed to explain away the observed
#' treatment effect. The sensitivity parameter Gamma represents the maximum
#' ratio of treatment odds between two matched units:
#'
#' - Gamma = 1: No hidden bias (standard Wilcoxon test)
#' - Gamma = 2: One unit could be twice as likely to receive treatment
#'   due to an unobserved factor
#'
#' The function computes upper and lower bounds on the p-value of the
#' Wilcoxon signed-rank test under each level of hidden bias. A finding is
#' "insensitive to bias" if p_upper remains below 0.05 even at large Gamma.
#'
#' @references
#' Rosenbaum, P.R. (2002). Observational Studies, 2nd edition. Springer.
#'
#' @examples
#' set.seed(42)
#' left <- data.frame(id = 1:20, x = rnorm(20), outcome = rnorm(20, 1, 1))
#' right <- data.frame(id = 21:40, x = rnorm(20), outcome = rnorm(20, 0, 1))
#' result <- match_couples(left, right, vars = "x")
#' sens <- sensitivity_analysis(result, left, right, outcome_var = "outcome")
#' print(sens)
#'
#' @export
sensitivity_analysis <- function(result,
                                 left,
                                 right,
                                 outcome_var,
                                 gamma = seq(1, 3, by = 0.25),
                                 alternative = c("greater", "less",
                                                 "two.sided"),
                                 left_id = "id",
                                 right_id = "id") {

  alternative <- match.arg(alternative)

  # --- Validation ---
  if (!inherits(result, "matching_result")) {
    stop("result must be a matching_result object", call. = FALSE)
  }
  if (!is.character(outcome_var) || length(outcome_var) != 1) {
    stop("outcome_var must be a single variable name", call. = FALSE)
  }
  if (!(outcome_var %in% names(left))) {
    stop(sprintf("outcome_var '%s' not found in left", outcome_var),
         call. = FALSE)
  }
  if (!(outcome_var %in% names(right))) {
    stop(sprintf("outcome_var '%s' not found in right", outcome_var),
         call. = FALSE)
  }
  if (!is.numeric(gamma) || any(gamma < 1)) {
    stop("gamma must be numeric with all values >= 1", call. = FALSE)
  }

  # --- Extract matched pair outcomes ---
  pairs <- result$pairs
  if (nrow(pairs) == 0) {
    stop("No matched pairs to analyze", call. = FALSE)
  }

  # Look up outcomes by ID, preserving the row-wise pair correspondence stored
  # in `pairs` (left_id[i] is matched to right_id[i]). Keyed lookup rather than
  # two independent merges, which would each sort by their own key and mis-pair
  # the differences.
  left_lookup <- stats::setNames(left[[outcome_var]], as.character(left[[left_id]]))
  right_lookup <- stats::setNames(right[[outcome_var]], as.character(right[[right_id]]))
  left_outcomes <- unname(left_lookup[as.character(pairs$left_id)])
  right_outcomes <- unname(right_lookup[as.character(pairs$right_id)])

  # Pair differences
  d <- left_outcomes - right_outcomes
  n <- length(d)

  # Remove zero differences (standard Wilcoxon convention)
  nonzero <- d != 0
  d <- d[nonzero]
  n <- length(d)

  if (n == 0) {
    stop("All pair differences are zero; cannot compute sensitivity bounds",
         call. = FALSE)
  }

  # Ranks of absolute differences
  ranks <- rank(abs(d))

  # Observed test statistic: sum of ranks where d > 0
  t_obs <- sum(ranks[d > 0])

  # --- Compute bounds for each gamma ---
  results <- tibble::tibble(
    gamma = numeric(length(gamma)),
    t_stat = numeric(length(gamma)),
    p_upper = numeric(length(gamma)),
    p_lower = numeric(length(gamma))
  )

  for (i in seq_along(gamma)) {
    g <- gamma[i]
    bounds <- .rosenbaum_bounds(t_obs, ranks, n, g, alternative)
    results$gamma[i] <- g
    results$t_stat[i] <- t_obs
    results$p_upper[i] <- bounds$p_upper
    results$p_lower[i] <- bounds$p_lower
  }

  # Critical gamma: smallest where p_upper > 0.05
  sig_idx <- which(results$p_upper > 0.05)
  critical_gamma <- if (length(sig_idx) > 0) {
    results$gamma[sig_idx[1]]
  } else {
    Inf  # Still significant at all tested gamma
  }

  out <- list(
    results = results,
    n_pairs = n,
    t_observed = t_obs,
    critical_gamma = critical_gamma,
    alternative = alternative
  )
  class(out) <- "sensitivity_analysis"
  out
}

#' Compute Rosenbaum bounds via normal approximation
#'
#' Under hidden bias Gamma, each pair's probability of the treated unit
#' having the larger value is between 1/(1+Gamma) and Gamma/(1+Gamma).
#'
#' @return List with p_upper and p_lower.
#' @keywords internal
.rosenbaum_bounds <- function(t_obs, ranks, n, gamma, alternative) {
  p_plus <- gamma / (1 + gamma)   # Upper bound on treatment probability
  p_minus <- 1 / (1 + gamma)     # Lower bound on treatment probability

  # Expected value and variance of T+ under p
  # E[T+] = sum(ranks * p), Var[T+] = sum(ranks^2 * p * (1-p))
  e_upper <- sum(ranks * p_plus)
  var_upper <- sum(ranks^2 * p_plus * (1 - p_plus))

  e_lower <- sum(ranks * p_minus)
  var_lower <- sum(ranks^2 * p_minus * (1 - p_minus))

  sd_upper <- sqrt(var_upper)
  sd_lower <- sqrt(var_lower)

  # Continuity-corrected normal approximation
  switch(alternative,
    greater = {
      z_upper <- (t_obs - 0.5 - e_upper) / sd_upper
      z_lower <- (t_obs - 0.5 - e_lower) / sd_lower
      p_upper <- stats::pnorm(z_upper, lower.tail = FALSE)
      p_lower <- stats::pnorm(z_lower, lower.tail = FALSE)
    },
    less = {
      z_upper <- (t_obs + 0.5 - e_lower) / sd_lower
      z_lower <- (t_obs + 0.5 - e_upper) / sd_upper
      p_upper <- stats::pnorm(z_upper, lower.tail = TRUE)
      p_lower <- stats::pnorm(z_lower, lower.tail = TRUE)
    },
    two.sided = {
      # Two-sided: use the more conservative bound
      z_upper_right <- (t_obs - 0.5 - e_upper) / sd_upper
      z_lower_left <- (t_obs + 0.5 - e_lower) / sd_lower

      p_upper <- 2 * min(
        stats::pnorm(z_upper_right, lower.tail = FALSE),
        stats::pnorm(z_lower_left, lower.tail = TRUE)
      )
      p_upper <- min(p_upper, 1)

      z_lower_right <- (t_obs - 0.5 - e_lower) / sd_lower
      z_upper_left <- (t_obs + 0.5 - e_upper) / sd_upper

      p_lower <- 2 * min(
        stats::pnorm(z_lower_right, lower.tail = FALSE),
        stats::pnorm(z_upper_left, lower.tail = TRUE)
      )
      p_lower <- min(p_lower, 1)
    }
  )

  list(p_upper = p_upper, p_lower = p_lower)
}

#' @rdname sensitivity_analysis
#' @param x A `sensitivity_analysis` object.
#' @param ... Ignored.
#' @export
print.sensitivity_analysis <- function(x, ...) {
  cat(sprintf("Rosenbaum sensitivity analysis (alternative \"%s\")\n",
              x$alternative))
  cat(sprintf("  %d pairs with a nonzero outcome difference\n", x$n_pairs))
  p1 <- x$results$p_upper[x$results$gamma == 1]
  if (length(p1)) cat(sprintf("  p-value without hidden bias (Gamma = 1): %.4g\n", p1))
  cat("  ", .sensitivity_verdict(x), "\n", sep = "")
  invisible(x)
}

# One sentence on how far the conclusion survives hidden bias.
.sensitivity_verdict <- function(x, alpha = 0.05) {
  tested <- range(x$results$gamma)
  if (!is.finite(x$critical_gamma)) {
    return(sprintf("Significant at alpha = %.2f for every Gamma up to %.2f.",
                   alpha, tested[2]))
  }
  below <- x$results$gamma[x$results$gamma < x$critical_gamma]
  if (!length(below)) {
    return(sprintf("Not significant at alpha = %.2f at the smallest Gamma tested (%.2f).",
                   alpha, tested[1]))
  }
  sprintf("Significant at alpha = %.2f up to Gamma = %.2f, not at Gamma = %.2f.",
          alpha, max(below), x$critical_gamma)
}

#' @rdname sensitivity_analysis
#' @param object A `sensitivity_analysis` object.
#' @export
summary.sensitivity_analysis <- function(object, ...) {
  class(object) <- c("summary.sensitivity_analysis", class(object))
  object
}

#' @export
print.summary.sensitivity_analysis <- function(x, ...) {
  print.sensitivity_analysis(x)
  cat(sprintf("  Signed-rank statistic T+ = %.1f\n", x$t_observed))
  cat("\n")
  tab <- as.data.frame(x$results[, c("gamma", "p_lower", "p_upper")])
  names(tab) <- c("Gamma", "p (lower bound)", "p (upper bound)")
  print(tab, row.names = FALSE, digits = 4)
  invisible(x)
}

#' @rdname sensitivity_analysis
#' @param row.names,optional Ignored.
#' @export
as.data.frame.sensitivity_analysis <- function(x, row.names = NULL,
                                               optional = FALSE, ...) {
  as.data.frame(x$results)
}

#' @rdname sensitivity_analysis
#' @param x A sensitivity_analysis object
#' @param alpha Significance level (default: 0.05)
#' @param ... Additional arguments passed to plot
#'
#' @export
#' @method plot sensitivity_analysis
plot.sensitivity_analysis <- function(x, alpha = 0.05, ...) {
  df <- x$results

  graphics::plot(df$gamma, df$p_upper,
                 type = "b", pch = 19, col = "steelblue",
                 xlab = expression(Gamma),
                 ylab = "Upper bound p-value",
                 main = "Rosenbaum Sensitivity Analysis",
                 ...)
  graphics::abline(h = alpha, lty = 2, col = "firebrick")

  invisible(x)
}
