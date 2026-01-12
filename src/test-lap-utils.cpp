/*
 * C++ unit tests for lap_utils functions
 * Tests low-level helper functions to improve coverage
 */

#include <testthat.h>
#include <Rcpp.h>
#include "core/lap_utils.h"

using namespace Rcpp;

context("LAP utility functions") {

  // =========================================================================
  // compute_total_cost tests
  // =========================================================================

  test_that("compute_total_cost works for simple 2x2") {
    NumericMatrix cost(2, 2);
    cost(0, 0) = 1.0; cost(0, 1) = 5.0;
    cost(1, 0) = 3.0; cost(1, 1) = 2.0;
    IntegerVector match = {1, 2};  // 1-based: row 0 -> col 1, row 1 -> col 2

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 3.0) < 1e-10);  // 1 + 2 = 3
  }

  test_that("compute_total_cost handles anti-diagonal assignment") {
    NumericMatrix cost(2, 2);
    cost(0, 0) = 1.0; cost(0, 1) = 5.0;
    cost(1, 0) = 3.0; cost(1, 1) = 2.0;
    IntegerVector match = {2, 1};  // 1-based: row 0 -> col 2, row 1 -> col 1

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 8.0) < 1e-10);  // 5 + 3 = 8
  }

  test_that("compute_total_cost handles 3x3 matrix") {
    NumericMatrix cost(3, 3);
    cost(0, 0) = 1.0; cost(0, 1) = 2.0; cost(0, 2) = 3.0;
    cost(1, 0) = 4.0; cost(1, 1) = 5.0; cost(1, 2) = 6.0;
    cost(2, 0) = 7.0; cost(2, 1) = 8.0; cost(2, 2) = 9.0;
    IntegerVector match = {1, 2, 3};  // 1-based diagonal

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 15.0) < 1e-10);  // 1 + 5 + 9 = 15
  }

  test_that("compute_total_cost handles unmatched rows") {
    NumericMatrix cost(3, 2);
    cost(0, 0) = 1.0; cost(0, 1) = 2.0;
    cost(1, 0) = 3.0; cost(1, 1) = 4.0;
    cost(2, 0) = 5.0; cost(2, 1) = 6.0;
    IntegerVector match = {1, 2, 0};  // 1-based, row 2 unmatched (0)

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 5.0) < 1e-10);  // 1 + 4 = 5
  }

  test_that("compute_total_cost handles negative costs") {
    NumericMatrix cost(2, 2);
    cost(0, 0) = -1.0; cost(0, 1) = -5.0;
    cost(1, 0) = -3.0; cost(1, 1) = -2.0;
    IntegerVector match = {1, 2};

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - (-3.0)) < 1e-10);  // -1 + -2 = -3
  }

  test_that("compute_total_cost handles large values") {
    NumericMatrix cost(2, 2);
    cost(0, 0) = 1e10; cost(0, 1) = 1e5;
    cost(1, 0) = 1e5;  cost(1, 1) = 1e10;
    IntegerVector match = {2, 1};

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 2e5) < 1e-5);
  }

  test_that("compute_total_cost with 1x1 matrix") {
    NumericMatrix cost(1, 1);
    cost(0, 0) = 42.5;
    IntegerVector match = {1};

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 42.5) < 1e-10);
  }

  // =========================================================================
  // Edge cases
  // =========================================================================

  test_that("compute_total_cost all rows unmatched") {
    NumericMatrix cost(2, 2);
    cost(0, 0) = 1.0; cost(0, 1) = 2.0;
    cost(1, 0) = 3.0; cost(1, 1) = 4.0;
    IntegerVector match = {0, 0};  // All unmatched (0)

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total) < 1e-10);  // 0
  }

  test_that("compute_total_cost first row unmatched") {
    NumericMatrix cost(2, 2);
    cost(0, 0) = 1.0; cost(0, 1) = 2.0;
    cost(1, 0) = 3.0; cost(1, 1) = 4.0;
    IntegerVector match = {0, 1};  // Only row 1 matched

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 3.0) < 1e-10);  // only 3
  }

  test_that("compute_total_cost rectangular matrix (more cols)") {
    NumericMatrix cost(2, 3);
    cost(0, 0) = 1.0; cost(0, 1) = 2.0; cost(0, 2) = 3.0;
    cost(1, 0) = 4.0; cost(1, 1) = 5.0; cost(1, 2) = 6.0;
    IntegerVector match = {1, 3};  // row 0 -> col 1, row 1 -> col 3

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 7.0) < 1e-10);  // 1 + 6 = 7
  }

  test_that("compute_total_cost with zero costs") {
    NumericMatrix cost(2, 2);
    cost(0, 0) = 0.0; cost(0, 1) = 0.0;
    cost(1, 0) = 0.0; cost(1, 1) = 0.0;
    IntegerVector match = {1, 2};

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total) < 1e-10);  // 0 + 0 = 0
  }

  test_that("compute_total_cost mixed positive and negative") {
    NumericMatrix cost(2, 2);
    cost(0, 0) = 10.0;  cost(0, 1) = -5.0;
    cost(1, 0) = -3.0;  cost(1, 1) = 8.0;
    IntegerVector match = {2, 1};  // anti-diagonal

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - (-8.0)) < 1e-10);  // -5 + -3 = -8
  }

  test_that("compute_total_cost 4x4 optimal diagonal") {
    NumericMatrix cost(4, 4);
    for (int i = 0; i < 4; ++i) {
      for (int j = 0; j < 4; ++j) {
        cost(i, j) = (i == j) ? 1.0 : 100.0;
      }
    }
    IntegerVector match = {1, 2, 3, 4};

    double total = compute_total_cost(cost, match);

    expect_true(std::abs(total - 4.0) < 1e-10);  // 4 * 1 = 4
  }

}
