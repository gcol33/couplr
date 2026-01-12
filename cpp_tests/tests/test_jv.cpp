// Test suite for pure C++ JV solver
// Tests the actual package code directly without Rcpp

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_jv_pure.h"

using Catch::Approx;

// Helper to create cost matrix from initializer list
lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("JV solver - basic square matrices", "[jv][basic]") {
    SECTION("3x3 identity-like costs") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_jv(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        // Optimal: diagonal (1+5+9=15) or anti-diagonal (3+5+7=15)
        REQUIRE(result.total_cost == Approx(15.0));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_jv(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 2);
        // Optimal: (0,0) + (1,1) = 1+4=5 or (0,1) + (1,0) = 2+3=5
        REQUIRE(result.total_cost == Approx(5.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_jv(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 1);
        REQUIRE(result.assignment[0] == 0);
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("JV solver - rectangular matrices", "[jv][rectangular]") {
    SECTION("2x3 (more cols than rows)") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_jv(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 2);
        // Should match all rows
        REQUIRE(result.n_matched() == 2);
        // Optimal: (0,0)=1 + (1,1)=5 = 6
        REQUIRE(result.total_cost == Approx(6.0));
    }

    SECTION("3x5 (more cols than rows)") {
        auto cost = make_cost({
            {10, 1, 20, 30, 40},
            {50, 60, 2, 70, 80},
            {90, 100, 110, 3, 120}
        });

        auto result = lap::solve_jv(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.n_matched() == 3);
        // Optimal picks the small values: 1+2+3=6
        REQUIRE(result.total_cost == Approx(6.0));
    }
}

TEST_CASE("JV solver - maximization", "[jv][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_jv(cost, true);

        REQUIRE(result.status == "optimal");
        // Maximum: 3+5+7=15 or other combo
        // Actually max is 3+6+7=16 or 1+6+9=16... let's check
        // Row 0: max=3 (col 2)
        // Row 1: max=6 (col 2) - conflict
        // Row 2: max=9 (col 2) - conflict
        // Need to find actual max matching
        // Possible: (0,2)=3 + (1,1)=5 + (2,0)=7 = 15
        // Or: (0,0)=1 + (1,2)=6 + (2,1)=8 = 15
        // Or: (0,1)=2 + (1,0)=4 + (2,2)=9 = 15
        // All give 15!
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("JV solver - forbidden edges", "[jv][forbidden]") {
    SECTION("some forbidden edges") {
        lap::CostMatrix cost(3, 3);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
        cost.at(1, 0) = 4;  cost.at(1, 1) = 5;  cost.at(1, 2) = 6;
        cost.at(2, 0) = 7;  cost.at(2, 1) = 8;  cost.at(2, 2) = 9;

        // Forbid the diagonal
        cost.forbid(0, 0);
        cost.forbid(1, 1);
        cost.forbid(2, 2);

        auto result = lap::solve_jv(cost, false);

        REQUIRE(result.status == "optimal");
        // Can't use diagonal, must find alternative
        // Optimal without diagonal: (0,1)=2 + (1,0)=4 + (2,2)=forbidden
        // or (0,1)=2 + (1,2)=6 + (2,0)=7 = 15
        // or (0,2)=3 + (1,0)=4 + (2,1)=8 = 15
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("JV solver - empty and edge cases", "[jv][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_jv(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.empty());
        REQUIRE(result.total_cost == Approx(0.0));
    }
}

TEST_CASE("JV solver - infeasible cases", "[jv][infeasible]") {
    SECTION("more rows than cols throws") {
        auto cost = make_cost({
            {1, 2},
            {3, 4},
            {5, 6}
        });

        REQUIRE_THROWS_AS(lap::solve_jv(cost, false), lap::DimensionException);
    }

    SECTION("row with all forbidden throws") {
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;
        cost.at(1, 0) = 3;  cost.at(1, 1) = 4;

        // Forbid entire row 0
        cost.forbid(0, 0);
        cost.forbid(0, 1);

        REQUIRE_THROWS_AS(lap::solve_jv(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("JV solver - assignment validity", "[jv][validity]") {
    SECTION("assignment is valid permutation") {
        auto cost = make_cost({
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12}
        });

        auto result = lap::solve_jv(cost, false);

        // Check all rows are matched
        REQUIRE(result.assignment.size() == 3);
        for (int i = 0; i < 3; ++i) {
            REQUIRE(result.assignment[i] >= 0);
            REQUIRE(result.assignment[i] < 4);
        }

        // Check no column is used twice
        std::vector<bool> used(4, false);
        for (int j : result.assignment) {
            REQUIRE(!used[j]);
            used[j] = true;
        }
    }
}

TEST_CASE("JV solver - cost computation", "[jv][cost]") {
    SECTION("total cost matches sum of assigned edges") {
        auto cost = make_cost({
            {10, 20, 30},
            {40, 50, 60},
            {70, 80, 90}
        });

        auto result = lap::solve_jv(cost, false);

        // Manually compute expected cost
        double expected = 0;
        for (int i = 0; i < 3; ++i) {
            expected += cost.at(i, result.assignment[i]);
        }

        REQUIRE(result.total_cost == Approx(expected));
    }
}
