test_that("match_couples() memory_mode = lazy agrees with dense on the basic path", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  res_dense <- match_couples(left, right, vars = c("x", "y"), memory_mode = "dense")
  res_lazy <- match_couples(left, right, vars = c("x", "y"), memory_mode = "lazy")

  expect_equal(res_lazy$pairs$left_id, res_dense$pairs$left_id)
  expect_equal(res_lazy$pairs$right_id, res_dense$pairs$right_id)
  expect_equal(res_lazy$pairs$distance, res_dense$pairs$distance, tolerance = 1e-9)
  expect_equal(res_lazy$pairs$.x_diff, res_dense$pairs$.x_diff, tolerance = 1e-9)
  expect_equal(res_lazy$pairs$.y_diff, res_dense$pairs$.y_diff, tolerance = 1e-9)
  expect_identical(res_lazy$unmatched, res_dense$unmatched)
})

test_that("match_couples() memory_mode = lazy agrees with dense under a feasible max_distance", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  res_dense <- match_couples(left, right, vars = c("x", "y"), max_distance = 2.5,
                             memory_mode = "dense")
  res_lazy <- match_couples(left, right, vars = c("x", "y"), max_distance = 2.5,
                            memory_mode = "lazy")

  expect_equal(sort(res_lazy$pairs$distance), sort(res_dense$pairs$distance),
              tolerance = 1e-9)
})

test_that("match_couples() memory_mode = lazy agrees with dense under a feasible caliper", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  res_dense <- match_couples(left, right, vars = c("x", "y"), calipers = c(x = 2.5),
                             memory_mode = "dense")
  res_lazy <- match_couples(left, right, vars = c("x", "y"), calipers = c(x = 2.5),
                            memory_mode = "lazy")

  expect_equal(sort(res_lazy$pairs$distance), sort(res_dense$pairs$distance),
              tolerance = 1e-9)
})

test_that("match_couples() memory_mode = lazy warns and returns empty on Hall-infeasible constraints", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  # A tight enough max_distance makes the row/col-pruned submatrix
  # Hall-infeasible; dense recovers a partial matching via its greedy
  # fallback, lazy has no such fallback yet (documented) and reports
  # everyone unmatched with a warning instead of a confusing raw error.
  expect_warning(
    res_lazy <- match_couples(left, right, vars = c("x", "y"), max_distance = 1.5,
                              memory_mode = "lazy"),
    "no feasible full matching"
  )
  expect_equal(nrow(res_lazy$pairs), 0L)
})

test_that("match_couples() excludes replace/ratio/greedy under memory_mode = lazy", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:6), x = rnorm(6))
  right <- data.frame(id = paste0("R", 1:6), x = rnorm(6))

  expect_error(
    match_couples(left, right, vars = "x", replace = TRUE, memory_mode = "lazy"),
    "replace = TRUE"
  )
  expect_error(
    match_couples(left, right, vars = "x", ratio = 2L, memory_mode = "lazy"),
    "ratio > 1"
  )
  expect_error(
    match_couples(left, right, vars = "x", method = "greedy", memory_mode = "lazy"),
    "greedy"
  )
})

test_that("compute_distances() + match_couples(dist_obj) works with memory_mode = lazy", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:10), x = rnorm(10), y = rnorm(10))
  right <- data.frame(id = paste0("R", 1:15), x = rnorm(15), y = rnorm(15))

  dist_obj <- compute_distances(left, right, vars = c("x", "y"), memory_mode = "lazy")
  expect_s3_class(dist_obj$cost_matrix, "lazy_cost_spec")

  res_dense <- match_couples(left, right, vars = c("x", "y"), memory_mode = "dense")
  res_from_dist <- match_couples(dist_obj)

  expect_equal(sort(res_from_dist$pairs$distance), sort(res_dense$pairs$distance),
              tolerance = 1e-9)
})

test_that("full_match() rejects memory_mode = lazy (different solver backend)", {
  set.seed(7)
  left <- data.frame(id = paste0("L", 1:5), x = rnorm(5))
  right <- data.frame(id = paste0("R", 1:10), x = rnorm(10))

  expect_error(
    full_match(left, right, vars = "x", memory_mode = "lazy"),
    "not supported"
  )
  expect_silent(full_match(left, right, vars = "x", memory_mode = "dense"))
})
