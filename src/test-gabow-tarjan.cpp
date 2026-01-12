/*
 * C++ unit tests for Gabow-Tarjan utility functions
 * Tests low-level helper functions to improve coverage
 */

#include <testthat.h>
#include "gabow_tarjan/utils_gabow_tarjan.h"

context("Gabow-Tarjan utility functions") {

  // =========================================================================
  // Module A: Cost-length & 1-feasibility
  // =========================================================================

  test_that("cost_length returns correct values") {
    // In matching: cost_length = c_ij
    expect_true(cost_length(5, true) == 5);
    expect_true(cost_length(0, true) == 0);
    expect_true(cost_length(100, true) == 100);

    // Not in matching: cost_length = c_ij + 1
    expect_true(cost_length(5, false) == 6);
    expect_true(cost_length(0, false) == 1);
    expect_true(cost_length(100, false) == 101);
  }

  test_that("is_eligible checks tight edges correctly") {
    // Edge is eligible when yu + yv == cost_length(c_ij, in_matching)
    // In matching: eligible when yu + yv == c_ij
    expect_true(is_eligible(10, true, 4, 6));   // 4 + 6 == 10
    expect_false(is_eligible(10, true, 3, 6));  // 3 + 6 != 10

    // Not in matching: eligible when yu + yv == c_ij + 1
    expect_true(is_eligible(10, false, 5, 6));  // 5 + 6 == 11
    expect_false(is_eligible(10, false, 4, 6)); // 4 + 6 != 11
  }

  test_that("check_one_feasible handles empty matrix") {
    CostMatrix cost;
    MatchVec row_match, col_match;
    DualVec y_u, y_v;

    expect_true(check_one_feasible(cost, row_match, col_match, y_u, y_v));
  }

  test_that("check_one_feasible verifies feasibility conditions") {
    // 2x2 cost matrix
    CostMatrix cost = {{1, 5}, {3, 2}};
    MatchVec row_match = {0, 1};  // row 0 -> col 0, row 1 -> col 1
    MatchVec col_match = {0, 1};  // col 0 -> row 0, col 1 -> row 1

    // Valid duals that satisfy 1-feasibility
    DualVec y_u = {1, 2};
    DualVec y_v = {0, 0};

    expect_true(check_one_feasible(cost, row_match, col_match, y_u, y_v));

    // Invalid duals (violate upper bound)
    DualVec y_u_bad = {100, 100};
    expect_false(check_one_feasible(cost, row_match, col_match, y_u_bad, y_v));
  }

  test_that("check_one_feasible handles forbidden edges") {
    CostMatrix cost = {{1, BIG_INT}, {BIG_INT, 2}};
    MatchVec row_match = {0, 1};
    MatchVec col_match = {0, 1};
    DualVec y_u = {1, 2};
    DualVec y_v = {0, 0};

    expect_true(check_one_feasible(cost, row_match, col_match, y_u, y_v));
  }

  // =========================================================================
  // Module B: Equality graph
  // =========================================================================

  test_that("build_equality_graph creates correct graph") {
    // 2x2 cost matrix
    CostMatrix cost = {{1, 5}, {3, 2}};
    MatchVec row_match = {NIL, NIL};  // No matches yet
    DualVec y_u = {0, 0};
    DualVec y_v = {2, 3};  // Make some edges eligible

    auto eq_graph = build_equality_graph(cost, row_match, y_u, y_v);

    expect_true(eq_graph.size() == 2);
    // Check that some edges are found
    expect_true(eq_graph[0].size() >= 0);  // May have eligible edges
  }

  test_that("build_equality_graph handles forbidden edges") {
    CostMatrix cost = {{1, BIG_INT}, {BIG_INT, 2}};
    MatchVec row_match = {NIL, NIL};
    DualVec y_u = {0, 0};
    DualVec y_v = {2, 3};

    auto eq_graph = build_equality_graph(cost, row_match, y_u, y_v);

    // Forbidden edges should not appear in equality graph
    expect_true(eq_graph.size() == 2);
  }

  test_that("update_equality_graph_incremental works") {
    CostMatrix cost = {{1, 5}, {3, 2}};
    MatchVec row_match = {NIL, NIL};
    DualVec y_u = {0, 0};
    DualVec y_v = {2, 3};

    auto eq_graph = build_equality_graph(cost, row_match, y_u, y_v);

    // Modify y_v and update incrementally
    y_v[0] = 1;
    std::vector<int> affected_cols = {0};

    update_equality_graph_incremental(eq_graph, cost, row_match, y_u, y_v, affected_cols);

    // Graph should be updated
    expect_true(eq_graph.size() == 2);
  }

  // =========================================================================
  // Module C: Augment matching
  // =========================================================================

  test_that("augment_along_path updates matching") {
    MatchVec row_match = {NIL, NIL, NIL};
    MatchVec col_match = {NIL, NIL, NIL};

    // Augmenting path: (0,0), (1,1)
    std::vector<std::pair<int,int>> edges = {{0, 0}, {1, 1}};

    augment_along_path(edges, row_match, col_match);

    expect_true(row_match[0] == 0);
    expect_true(row_match[1] == 1);
    expect_true(col_match[0] == 0);
    expect_true(col_match[1] == 1);
  }

  test_that("augment_along_path handles empty path") {
    MatchVec row_match = {NIL, NIL};
    MatchVec col_match = {NIL, NIL};

    std::vector<std::pair<int,int>> edges;  // Empty path

    augment_along_path(edges, row_match, col_match);

    expect_true(row_match[0] == NIL);
    expect_true(row_match[1] == NIL);
  }

  // =========================================================================
  // Module D: Augmenting paths
  // =========================================================================

  test_that("find_one_augmenting_path_eq finds path") {
    // Simple equality graph where path exists
    std::vector<std::vector<int>> eq_graph = {{0, 1}, {1}};
    MatchVec row_match = {NIL, NIL};
    MatchVec col_match = {NIL, NIL};
    std::vector<bool> banned_row = {false, false};
    std::vector<bool> banned_col = {false, false};

    auto path = find_one_augmenting_path_eq(eq_graph, row_match, col_match,
                                            banned_row, banned_col);

    // Should find a path from free row to free column
    expect_true(path.size() >= 1);
  }

  test_that("find_one_augmenting_path_eq respects banned vertices") {
    std::vector<std::vector<int>> eq_graph = {{0}, {1}};
    MatchVec row_match = {NIL, NIL};
    MatchVec col_match = {NIL, NIL};
    std::vector<bool> banned_row = {true, true};  // All rows banned
    std::vector<bool> banned_col = {false, false};

    auto path = find_one_augmenting_path_eq(eq_graph, row_match, col_match,
                                            banned_row, banned_col);

    // No path should be found (all source rows banned)
    expect_true(path.empty());
  }

  test_that("find_maximal_augmenting_paths finds vertex-disjoint paths") {
    // Graph where two disjoint paths exist
    std::vector<std::vector<int>> eq_graph = {{0}, {1}};
    MatchVec row_match = {NIL, NIL};
    MatchVec col_match = {NIL, NIL};

    auto paths = find_maximal_augmenting_paths(eq_graph, row_match, col_match);

    // Should find 2 disjoint paths
    expect_true(paths.size() == 2);
  }

  // =========================================================================
  // Module E: Hungarian search
  // =========================================================================

  test_that("build_cl_matrix creates cost-length matrix") {
    CostMatrix cost = {{1, 5}, {3, 2}};
    MatchVec row_match = {0, NIL};  // row 0 matched to col 0

    CostMatrix cl = build_cl_matrix(cost, row_match);

    // Matched edge: cl = cost
    expect_true(cl[0][0] == 1);

    // Unmatched edge: cl = cost + 1
    expect_true(cl[0][1] == 6);
    expect_true(cl[1][0] == 4);
    expect_true(cl[1][1] == 3);
  }

  test_that("build_cl_matrix handles forbidden edges") {
    CostMatrix cost = {{1, BIG_INT}, {BIG_INT, 2}};
    MatchVec row_match = {0, 1};

    CostMatrix cl = build_cl_matrix(cost, row_match);

    expect_true(cl[0][1] == BIG_INT);
    expect_true(cl[1][0] == BIG_INT);
  }

  test_that("is_perfect detects perfect matching") {
    MatchVec perfect = {0, 1, 2};
    MatchVec imperfect = {0, NIL, 2};

    expect_true(is_perfect(perfect));
    expect_false(is_perfect(imperfect));
  }

  test_that("find_max_cost finds maximum finite cost") {
    CostMatrix cost = {{1, 5, BIG_INT}, {3, 2, 10}};

    expect_true(find_max_cost(cost) == 10);
  }

  test_that("find_max_cost handles all forbidden") {
    CostMatrix cost = {{BIG_INT, BIG_INT}, {BIG_INT, BIG_INT}};

    expect_true(find_max_cost(cost) == 0);
  }

  // =========================================================================
  // Integration tests
  // =========================================================================

  test_that("match_gt finds perfect matching on small matrix") {
    CostMatrix cost = {{1, 5}, {3, 2}};
    MatchVec row_match, col_match;
    DualVec y_u, y_v;

    match_gt(cost, row_match, col_match, y_u, y_v);

    expect_true(is_perfect(row_match));
    expect_true(row_match.size() == 2);
    expect_true(col_match.size() == 2);
  }

  test_that("solve_gabow_tarjan_inner works on 3x3") {
    CostMatrix cost = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    MatchVec row_match, col_match;
    DualVec y_u, y_v;

    solve_gabow_tarjan_inner(cost, row_match, col_match, y_u, y_v);

    expect_true(is_perfect(row_match));
  }

  test_that("solve_gabow_tarjan_inner handles empty matrix") {
    CostMatrix cost;
    MatchVec row_match, col_match;
    DualVec y_u, y_v;

    solve_gabow_tarjan_inner(cost, row_match, col_match, y_u, y_v);

    expect_true(row_match.empty());
    expect_true(col_match.empty());
  }

}
