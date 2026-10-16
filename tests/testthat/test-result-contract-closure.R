test_that("one observed value is reported before variance calculations", {
  left <- data.frame(x = c(1, NA, NA), z = 1:3)
  right <- data.frame(x = rep(NA_real_, 3), z = 4:6)
  fit <- preprocess_matching_vars(left, right, c("x", "z"), verbose = FALSE)
  expect_identical(fit$vars, "z")
  expect_identical(fit$excluded_vars, "x")
  expect_identical(fit$left, left)
  expect_identical(fit$right, right)
  health <- summary(fit$health)
  expect_s3_class(health, "summary.variable_health")
  expect_identical(health$issues$type, "insufficient_observations")
  expect_true(is.na(health$summary$sd[health$summary$variable == "x"]))
  expect_output(print(health), "fewer than two observed values")
  kept <- preprocess_matching_vars(left, right, c("x", "z"),
    remove_problematic = FALSE, verbose = FALSE)
  expect_identical(kept$vars, c("x", "z"))
  expect_length(kept$excluded_vars, 0L)
  expect_error(preprocess_matching_vars(left, right, "x", verbose = FALSE),
                "All variables were excluded")
  expect_identical(preprocess_matching_vars(left, right, "x",
    remove_problematic = FALSE, verbose = FALSE)$scaling_method, "none")
})

test_that("preprocessing summaries expose choices and health findings", {
  left <- data.frame(x = 1:3, constant = 1)
  right <- data.frame(x = 4:6, constant = 1)
  for (check in c(TRUE, FALSE)) {
    fit <- preprocess_matching_vars(left, right, c("x", "constant"),
      check_health = check, auto_scale = FALSE, verbose = FALSE)
    detail <- summary(fit)
    expect_s3_class(detail, "summary.preprocessing_result")
    expect_equal(c(detail$n_left, detail$n_right), c(3L, 3L))
    expect_identical(detail$vars, fit$vars)
    expect_identical(detail$excluded_vars, fit$excluded_vars)
    expect_identical(detail$warnings, fit$warnings)
    expect_lt(length(capture.output(print(fit))),
              length(capture.output(print(detail))))
    if (check) expect_output(print(detail), "Issue details") else
      expect_null(detail$health)
  }
})

test_that("dispatch summaries expose the actual decision and rule values", {
  cost <- outer(1:10, 1:12, function(i, j) (i - j)^2 + i / 100)
  for (method in c("auto", "hungarian")) {
    report <- explain_dispatch(cost, method = method)
    detail <- summary(report)
    expect_s3_class(detail, "summary.dispatch_explanation")
    expect_identical(detail$method, assignment(cost, method = method)$method_used)
    expect_identical(detail$considered, report$considered)
    expect_equal(detail$probe$n_total, length(cost))
    expect_output(print(detail), "Rules tested")
    expect_output(print(detail), "Probe:")
    expect_lt(length(capture.output(print(report))),
              length(capture.output(print(detail))))
  }
})

test_that("cardinality report summaries preserve exact and heuristic guarantees", {
  left <- data.frame(id = 1:4, x = 1:4, g = rep(c("a", "b"), each = 2))
  right <- data.frame(id = 5:10, x = 1:6, g = rep(c("a", "b"), each = 3))
  exact <- cardinality_match(left, right, vars = "x", fine = "g")
  report <- exact$cardinality
  expect_s3_class(summary(report), "summary.cardinality_report")
  expect_output(print(summary(report)), "Selected category counts")
  cert <- certificate(exact)
  expect_s3_class(cert, "cardinality_certificate")
  expect_true(glance(cert)$certified)
  expect_identical(summary(cert)$report, report)
  expect_output(print(summary(cert)), "Search report")
  heuristic <- cardinality_match(left, right, vars = "x", engine = "heuristic",
                                  max_std_diff = 0.2)$cardinality
  detail <- summary(heuristic)
  expect_s3_class(detail, "summary.cardinality_report")
  expect_output(print(detail), "Heuristic: no optimality guarantee")
  expect_output(print(detail), "Total distance")
  expect_output(print(detail), "Standardized-difference target")
  expect_true(is.na(detail$report$best_possible))
  expect_lt(length(capture.output(print(heuristic))),
            length(capture.output(print(detail))))
})

test_that("a batch summary counts each input problem once", {
  large <- matrix(100, 4, 4)
  diag(large) <- 1
  fit <- lap_solve_batch(list(matrix(3, 1, 1), large))
  detail <- summary(fit)
  expect_s3_class(detail, "summary.lap_solve_batch_result")
  expect_equal(detail$problems$n_assignments, c(1L, 4L))
  expect_equal(detail$problems$total_cost, c(3, 4))
  expect_equal(mean(detail$problems$total_cost), 3.5)
  expect_identical(attr(fit, "problem_keys"), "problem_id")
  expect_lt(length(capture.output(print(fit))),
            length(capture.output(print(detail))))

  data <- expand.grid(region = c("north", "south"), replicate = 1:2,
                      source = c("a", "b"), target = c("u", "v"),
                      stringsAsFactors = FALSE)
  data$cost <- as.numeric(data$source == "b") + as.numeric(data$target == "v") + 1
  fit <- lap_solve_batch(dplyr::group_by(data, .data$region, .data$replicate),
                          source, target, cost)
  ledger <- summary(fit)$problems
  expect_identical(attr(fit, "problem_keys"), c("region", "replicate"))
  expect_equal(nrow(ledger), 4L)
  expect_equal(ledger$n_assignments, rep(2L, 4))
  expect_equal(ledger$total_cost, rep(4, 4))
  expect_false(anyDuplicated(ledger[c("region", "replicate")]) > 0L)
})

test_that("assignment accessors work across native, tidy and dual results", {
  cost <- matrix(c(1, 20, 20, 1, 40, 40), 2, 3)
  native <- assignment(cost, method = "jv", certify = TRUE)
  tidy <- lap_solve(cost, method = "jv")
  dual <- assignment_duals(cost, certify = TRUE)
  expect_s3_class(dual, "lap_solve_result")
  for (fit in list(native, tidy, dual)) {
    expect_equal(get_total_cost(fit), 2)
    expect_identical(get_method_used(fit), "jv")
    binary <- as_assignment_matrix(fit)
    expect_equal(dim(binary), c(2L, 3L))
    expect_equal(rowSums(binary), c(1, 1))
    expect_equal(colSums(binary), c(1, 1, 0))
    expect_lt(length(capture.output(print(fit))),
              length(capture.output(print(summary(fit)))))
  }
  expect_true(certificate(dual)$certified_optimal)
  expect_identical(potentials(tidy), potentials(assignment(cost, method = "jv")))
  tall <- assignment(t(cost), method = "jv")
  expect_equal(dim(as_assignment_matrix(tall)), c(3L, 2L))
  expect_identical(unmatched_units(tall), which(tall$match == 0L))
  long <- expand.grid(source = c("left A", "left B"), target = c("x", "y", "z"),
                      stringsAsFactors = FALSE)
  long$cost <- as.vector(cost)
  labeled <- lap_solve(long, source, target, cost)
  binary <- as_assignment_matrix(labeled)
  expect_identical(rownames(binary), c("left A", "left B"))
  expect_identical(colnames(binary), c("x", "y", "z"))
  expect_equal(unname(binary), as_assignment_matrix(native))
})

test_that("Sinkhorn reports expose convergence and the observed marginals", {
  cost <- matrix(c(1, 2, 3, 4), 2)
  fit <- sinkhorn(cost, r_weights = c(0.25, 0.75))
  expect_s3_class(fit, "sinkhorn_result")
  expect_identical(as.matrix(fit), fit$transport_plan)
  detail <- summary(fit)
  expect_s3_class(detail, "summary.sinkhorn_result")
  expect_identical(detail$converged, fit$converged)
  expect_identical(detail$iterations, fit$iterations)
  expect_equal(detail$row_marginals, c(0.25, 0.75), tolerance = 1e-5)
  expect_equal(detail$column_marginals, c(0.5, 0.5), tolerance = 1e-5)
  expect_equal(detail$mass, 1)
  expect_lt(length(capture.output(print(fit))),
            length(capture.output(print(detail))))
})

test_that("distance diagnostics report an empty admissible set", {
  for (cost in list(matrix(Inf, 3, 3), matrix(numeric(), 0, 0), diag(3))) {
    report <- diagnose_distance_matrix(cost, warn = FALSE)
    expect_s3_class(report, "distance_diagnostics")
    detail <- summary(report)
    expect_s3_class(detail, "summary.distance_diagnostics")
    expect_equal(detail$distribution$n_total, length(cost))
    expect_lt(length(capture.output(print(report))),
              length(capture.output(print(detail))))
    if (all(!is.finite(cost))) {
      expect_identical(detail$quality, "poor")
      expect_equal(detail$distribution$n_finite, 0)
    }
  }
  expect_error(diagnose_distance_matrix(matrix(letters[1:4], 2)), "numeric matrix")
})

test_that("custom distance callbacks print safely without changing stored distances", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:7, x = 4:7)
  distance <- function(a, b) abs(outer(a[, 1], b[, 1], "-"))
  fit <- compute_distances(left, right, vars = "x", distance = distance,
                           memory_mode = "dense")
  expect_output(print(fit), "custom function")
  expect_output(summary(fit), "custom function")
  expect_identical(fit$metadata$distance, distance)
  expect_equal(unname(as.matrix(fit$cost_matrix)), distance(as.matrix(left["x"]),
                                                          as.matrix(right["x"])),
               ignore_attr = TRUE)
})

test_that("pixel animation reports preserve the embedded media and assignment", {
  skip_if_not_installed("magick")
  fit <- pixel_morph_animate(magick::image_blank(2, 2, "red"),
    magick::image_blank(2, 2, "blue"), n_frames = 2, mode = "exact", show = FALSE)
  expect_s3_class(fit, "pixel_morph_animation")
  expect_s3_class(fit$animation, "magick-image")
  expect_setequal(fit$assignment, 1:4)
  detail <- summary(fit)
  expect_s3_class(detail, "summary.pixel_morph_animation")
  expect_equal(detail$n_frames, length(fit$animation))
  expect_equal(detail$n_assigned, 4)
  expect_equal(detail$n_reused_targets, 0)
  expect_equal(detail$n_unfilled_targets, 0)
  expect_lt(length(capture.output(print(fit))),
            length(capture.output(print(detail))))
})
