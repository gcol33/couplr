// Test suite for pure C++ Greedy Matching solvers

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/greedy_matching.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

// ============================================================================
// Greedy Sorted
// ============================================================================

TEST_CASE("Greedy sorted - basic", "[greedy][sorted][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::greedy_matching_sorted(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.n_matched() == 3);
        // Greedy picks smallest first: 1, then next available
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::greedy_matching_sorted(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::greedy_matching_sorted(cost, false);

        REQUIRE(result.assignment[0] == 0);
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("Greedy sorted - rectangular", "[greedy][sorted][rectangular]") {
    SECTION("2x3") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::greedy_matching_sorted(cost, false);

        REQUIRE(result.n_matched() == 2);
    }

    SECTION("3x5") {
        auto cost = make_cost({
            {10, 1, 20, 30, 40},
            {50, 60, 2, 70, 80},
            {90, 100, 110, 3, 120}
        });

        auto result = lap::greedy_matching_sorted(cost, false);

        REQUIRE(result.n_matched() == 3);
        // Greedy picks 1, 2, 3 in order = 6
        REQUIRE(result.total_cost == Approx(6.0));
    }
}

TEST_CASE("Greedy sorted - maximization", "[greedy][sorted][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::greedy_matching_sorted(cost, true);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
    }
}

// ============================================================================
// Greedy Row Best
// ============================================================================

TEST_CASE("Greedy row_best - basic", "[greedy][row_best][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::greedy_matching_row_best(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::greedy_matching_row_best(cost, false);

        REQUIRE(result.n_matched() == 2);
    }
}

TEST_CASE("Greedy row_best - rectangular", "[greedy][row_best][rectangular]") {
    SECTION("2x3") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::greedy_matching_row_best(cost, false);

        REQUIRE(result.n_matched() == 2);
    }
}

// ============================================================================
// Greedy Priority Queue
// ============================================================================

TEST_CASE("Greedy pq - basic", "[greedy][pq][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::greedy_matching_pq(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::greedy_matching_pq(cost, false);

        REQUIRE(result.n_matched() == 2);
    }
}

TEST_CASE("Greedy pq - rectangular", "[greedy][pq][rectangular]") {
    SECTION("3x5") {
        auto cost = make_cost({
            {10, 1, 20, 30, 40},
            {50, 60, 2, 70, 80},
            {90, 100, 110, 3, 120}
        });

        auto result = lap::greedy_matching_pq(cost, false);

        REQUIRE(result.n_matched() == 3);
        REQUIRE(result.total_cost == Approx(6.0));
    }
}

// ============================================================================
// Greedy dispatcher
// ============================================================================

TEST_CASE("Greedy dispatcher - strategies", "[greedy][dispatcher]") {
    auto cost = make_cost({
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    });

    SECTION("sorted strategy") {
        auto result = lap::greedy_matching(cost, false, "sorted");
        REQUIRE(result.n_matched() == 3);
    }

    SECTION("row_best strategy") {
        auto result = lap::greedy_matching(cost, false, "row_best");
        REQUIRE(result.n_matched() == 3);
    }

    SECTION("pq strategy") {
        auto result = lap::greedy_matching(cost, false, "pq");
        REQUIRE(result.n_matched() == 3);
    }
}

// ============================================================================
// Edge cases
// ============================================================================

TEST_CASE("Greedy - empty matrix", "[greedy][edge]") {
    lap::CostMatrix cost(0, 0);

    SECTION("sorted") {
        auto result = lap::greedy_matching_sorted(cost, false);
        REQUIRE(result.assignment.empty());
    }

    SECTION("row_best") {
        auto result = lap::greedy_matching_row_best(cost, false);
        REQUIRE(result.assignment.empty());
    }

    SECTION("pq") {
        auto result = lap::greedy_matching_pq(cost, false);
        REQUIRE(result.assignment.empty());
    }
}

TEST_CASE("Greedy - forbidden edges", "[greedy][forbidden]") {
    // Use a matrix where greedy can still find a full matching with forbidden edges
    lap::CostMatrix cost(3, 3);
    cost.at(0, 0) = 1;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
    cost.at(1, 0) = 4;  cost.at(1, 1) = 5;  cost.at(1, 2) = 6;
    cost.at(2, 0) = 7;  cost.at(2, 1) = 8;  cost.at(2, 2) = 9;

    // Forbid just one edge - greedy should still find full matching
    cost.forbid(0, 0);

    SECTION("sorted") {
        auto result = lap::greedy_matching_sorted(cost, false);
        REQUIRE(result.n_matched() == 3);
    }

    SECTION("row_best") {
        auto result = lap::greedy_matching_row_best(cost, false);
        REQUIRE(result.n_matched() == 3);
    }

    SECTION("pq") {
        auto result = lap::greedy_matching_pq(cost, false);
        REQUIRE(result.n_matched() == 3);
    }
}
