test_that("block preparation has its own summary contract", {
  left <- data.frame(id = 1:4, g = c("A", "A", "B", "C"), x = 1:4)
  right <- data.frame(id = 5:10, g = c("A", "A", "A", "B", "B", "D"),
                      x = 5:10)
  for (type in c("none", "group")) {
    blocks <- matchmaker(left, right, block_type = type, block_by = "g")
    expect_false(inherits(blocks, "couplr_result"))
    detail <- summary(blocks)
    expect_s3_class(detail, "summary.matchmaker_result")
    expect_identical(detail$block_summary, blocks$block_summary)
    expect_equal(sum(detail$block_summary$n_left), nrow(blocks$left))
    expect_equal(sum(detail$block_summary$n_right), nrow(blocks$right))
    expect_output(print(detail), "Left units retained")
    expect_output(print(detail), "Block summary")
    expect_lt(length(capture.output(print(blocks))),
              length(capture.output(print(detail))))
    expect_error(certificate(blocks), "no method")
  }
  detail <- summary(matchmaker(left, right, block_type = "group", block_by = "g"))
  expect_setequal(detail$dropped$blocks, c("C", "D"))
  expect_output(print(detail), "Dropped block reasons")
  expect_output(print(detail), "too_small")
  hidden <- matchmaker(left, right, block_type = "group", block_by = "g",
                        return_dropped = FALSE)
  expect_null(summary(hidden)$dropped)
  expect_output(print(summary(hidden)), "Blocks dropped: 2")
})

test_that("specialized assignment summaries preserve objectives and every row", {
  line <- lap_solve_line_metric(1:14, seq(1.25, 14.25))
  detail <- summary(line)
  expect_s3_class(detail, "summary.assignment_result")
  expect_equal(unname(detail$objective), line$total_cost)
  expect_identical(detail$assignments$column, line$match)
  expect_equal(detail$assignments$row, seq_along(line$match))
  expect_output(print(detail), "Total cost: 3.5")
  expect_false(any(grepl("omitted|more assignments", capture.output(print(detail)))))
  expect_true(any(grepl("14 +14", capture.output(print(detail)))))

  for (maximize in c(FALSE, TRUE)) {
    fit <- bottleneck_assignment(matrix(1:12, 3), maximize = maximize)
    detail <- summary(fit)
    expect_equal(unname(detail$objective), fit$bottleneck)
    expect_identical(detail$status, fit$status)
    expect_identical(detail$assignments$column, fit$match)
    expect_output(print(detail), "Bottleneck value")
    expect_output(print(detail), fit$status)
  }
})

test_that("the cardinality summary displays constraints and stage evidence", {
  left <- data.frame(id = 1:4, g = c("a", "a", "b", "b"), x = c(0, 1, 2, 3))
  right <- data.frame(id = 5:10, g = rep(c("a", "b"), each = 3), x = 0:5)
  fit <- cardinality_match(left, right, vars = "x", fine = "g",
                           max_std_diff = 0.2)
  detail <- summary(fit)
  expect_identical(detail$cardinality$constraints, fit$cardinality$constraints)
  expect_output(print(detail), "Balance constraints")
  expect_output(print(detail), "Selected category counts")
  expect_output(print(detail), "Optimization stages")
  expect_output(print(detail), "satisfied")
})
