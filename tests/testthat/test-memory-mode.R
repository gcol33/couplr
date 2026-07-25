test_that("get_free_ram_mb() never errors and returns NA or a positive number", {
  result <- get_free_ram_mb()
  expect_true(is.na(result) || (is.numeric(result) && result > 0))
})

test_that("estimate_dense_matrix_mb() uses double arithmetic (no integer overflow)", {
  # 50000 x 50000 as integer multiplication would overflow .Machine$integer.max;
  # as.numeric() before multiplying keeps this correct at any realistic scale.
  mb <- estimate_dense_matrix_mb(50000, 50000)
  expect_true(is.finite(mb))
  expect_gt(mb, 0)
})

test_that("resolve_memory_mode(): small problems never probe RAM", {
  # A mock that errors if called proves the cheap cell-count short-circuit
  # actually skips the probe for ordinary-sized problems.
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() stop("should not be called for small problems"),
    .package = "couplr"
  )
  expect_equal(resolve_memory_mode(10, 10, "auto", solver_supports_lazy = FALSE), "dense")
  expect_equal(resolve_memory_mode(10, 10, "auto", solver_supports_lazy = TRUE), "dense")
})

test_that("resolve_memory_mode(): \"dense\" always skips the RAM probe", {
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() stop("should not be called when memory_mode = 'dense'"),
    .package = "couplr"
  )
  expect_equal(resolve_memory_mode(1e6, 1e6, "dense", solver_supports_lazy = TRUE), "dense")
})

test_that("resolve_memory_mode(): \"lazy\" errors when unsupported, returns \"lazy\" when supported", {
  expect_error(
    resolve_memory_mode(10, 10, "lazy", solver_supports_lazy = FALSE),
    "not supported"
  )
  expect_equal(resolve_memory_mode(10, 10, "lazy", solver_supports_lazy = TRUE), "lazy")
})

test_that("resolve_memory_mode(): \"auto\" switches to lazy under tight simulated RAM when supported", {
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() 1000,  # pretend only 1GB free
    .package = "couplr"
  )
  expect_warning(
    mode <- resolve_memory_mode(50000, 50000, "auto", solver_supports_lazy = TRUE),
    "Switching to memory_mode"
  )
  expect_equal(mode, "lazy")
})

test_that("resolve_memory_mode(): \"auto\" warns and stays dense under tight RAM when lazy unsupported", {
  testthat::local_mocked_bindings(
    get_free_ram_mb = function() 1000,
    .package = "couplr"
  )
  expect_warning(
    mode <- resolve_memory_mode(50000, 50000, "auto", solver_supports_lazy = FALSE),
    "Proceeding densely"
  )
  expect_equal(mode, "dense")
})
