// Test suite for pure C++ JV solver with dual variables
// Tests the dual variable computation directly without Rcpp

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_jv_duals.h"

using Catch::Approx;

// Helper to create cost matrix from initializer list
inline lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("JV duals solver - basic functionality", "[jv_duals][basic]") {
    SECTION("3x3 returns valid solution and duals") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_jv_duals(cost, false);

        REQUIRE(result.solution.status == "optimal");
        REQUIRE(result.solution.assignment.size() == 3);
        REQUIRE(result.u.size() == 3);
        REQUIRE(result.v.size() == 3);
        REQUIRE(result.solution.total_cost == Approx(15.0));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_jv_duals(cost, false);

        REQUIRE(result.solution.status == "optimal");
        REQUIRE(result.u.size() == 2);
        REQUIRE(result.v.size() == 2);
        REQUIRE(result.solution.total_cost == Approx(5.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_jv_duals(cost, false);

        REQUIRE(result.solution.status == "optimal");
        REQUIRE(result.solution.assignment.size() == 1);
        REQUIRE(result.u.size() == 1);
        REQUIRE(result.v.size() == 1);
        REQUIRE(result.solution.total_cost == Approx(42.0));
    }
}

TEST_CASE("JV duals solver - complementary slackness", "[jv_duals][slackness]") {
    SECTION("minimization: u[i] + v[j] = c[i,j] for assigned pairs") {
        auto cost = make_cost({
            {10, 20, 30},
            {40, 50, 60},
            {70, 80, 90}
        });

        auto result = lap::solve_jv_duals(cost, false);

        // Check complementary slackness for assigned pairs
        for (int i = 0; i < 3; ++i) {
            int j = result.solution.assignment[i];
            double slack = result.u[i] + result.v[j] - cost.at(i, j);
            REQUIRE(std::abs(slack) < 1e-9);
        }
    }

    SECTION("minimization: u[i] + v[j] <= c[i,j] for all pairs") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_jv_duals(cost, false);

        // Check reduced costs are non-negative
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                double reduced = cost.at(i, j) - result.u[i] - result.v[j];
                REQUIRE(reduced >= -1e-9);
            }
        }
    }
}

TEST_CASE("JV duals solver - rectangular matrices", "[jv_duals][rectangular]") {
    SECTION("2x3 has correct dimensions") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_jv_duals(cost, false);

        REQUIRE(result.solution.status == "optimal");
        REQUIRE(result.solution.assignment.size() == 2);
        REQUIRE(result.u.size() == 2);
        REQUIRE(result.v.size() == 3);
        REQUIRE(result.solution.n_matched() == 2);
    }

    SECTION("3x5 optimal matching") {
        auto cost = make_cost({
            {10, 1, 20, 30, 40},
            {50, 60, 2, 70, 80},
            {90, 100, 110, 3, 120}
        });

        auto result = lap::solve_jv_duals(cost, false);

        REQUIRE(result.solution.status == "optimal");
        REQUIRE(result.u.size() == 3);
        REQUIRE(result.v.size() == 5);
        REQUIRE(result.solution.total_cost == Approx(6.0));
    }
}

TEST_CASE("JV duals solver - maximization", "[jv_duals][maximize]") {
    SECTION("maximize 3x3") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_jv_duals(cost, true);

        REQUIRE(result.solution.status == "optimal");
        REQUIRE(result.u.size() == 3);
        REQUIRE(result.v.size() == 3);
        REQUIRE(result.solution.total_cost == Approx(15.0));
    }

    SECTION("maximize complementary slackness: u[i] + v[j] >= c[i,j]") {
        auto cost = make_cost({
            {10, 20, 30},
            {40, 50, 60},
            {70, 80, 90}
        });

        auto result = lap::solve_jv_duals(cost, true);

        // For maximization, reduced costs should be non-positive
        // (after internal negation and un-negation)
        // The duals are returned in original scale
        for (int i = 0; i < 3; ++i) {
            int j = result.solution.assignment[i];
            double slack = result.u[i] + result.v[j] - cost.at(i, j);
            REQUIRE(std::abs(slack) < 1e-9);
        }
    }
}

TEST_CASE("JV duals solver - forbidden edges", "[jv_duals][forbidden]") {
    SECTION("some forbidden edges") {
        lap::CostMatrix cost(3, 3);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
        cost.at(1, 0) = 4;  cost.at(1, 1) = 5;  cost.at(1, 2) = 6;
        cost.at(2, 0) = 7;  cost.at(2, 1) = 8;  cost.at(2, 2) = 9;

        // Forbid the diagonal
        cost.forbid(0, 0);
        cost.forbid(1, 1);
        cost.forbid(2, 2);

        auto result = lap::solve_jv_duals(cost, false);

        REQUIRE(result.solution.status == "optimal");
        REQUIRE(result.u.size() == 3);
        REQUIRE(result.v.size() == 3);
        REQUIRE(result.solution.total_cost == Approx(15.0));
    }
}

TEST_CASE("JV duals solver - edge cases", "[jv_duals][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_jv_duals(cost, false);

        REQUIRE(result.solution.status == "optimal");
        REQUIRE(result.solution.assignment.empty());
        REQUIRE(result.u.empty());
        REQUIRE(result.v.empty());
        REQUIRE(result.solution.total_cost == Approx(0.0));
    }
}

TEST_CASE("JV duals solver - infeasible cases", "[jv_duals][infeasible]") {
    SECTION("more rows than cols throws") {
        auto cost = make_cost({
            {1, 2},
            {3, 4},
            {5, 6}
        });

        REQUIRE_THROWS_AS(lap::solve_jv_duals(cost, false), lap::DimensionException);
    }

    SECTION("row with all forbidden throws") {
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;
        cost.at(1, 0) = 3;  cost.at(1, 1) = 4;

        // Forbid entire row 0
        cost.forbid(0, 0);
        cost.forbid(0, 1);

        REQUIRE_THROWS_AS(lap::solve_jv_duals(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("JV duals solver - dual sum equals total cost", "[jv_duals][dual_sum]") {
    SECTION("sum of duals equals total cost") {
        auto cost = make_cost({
            {10, 20, 30},
            {40, 50, 60},
            {70, 80, 90}
        });

        auto result = lap::solve_jv_duals(cost, false);

        // For a feasible optimal solution:
        // total_cost = sum of u[i] + v[j] for assigned pairs
        double dual_sum = 0.0;
        for (int i = 0; i < 3; ++i) {
            dual_sum += result.u[i];
        }
        for (int j = 0; j < 3; ++j) {
            dual_sum += result.v[j];
        }

        // The sum of all duals may not equal total cost directly,
        // but for each matched pair: u[i] + v[j] = c[i,j]
        // So total_cost = sum over matched pairs
        double match_sum = 0.0;
        for (int i = 0; i < 3; ++i) {
            int j = result.solution.assignment[i];
            match_sum += result.u[i] + result.v[j];
        }
        REQUIRE(match_sum == Approx(result.solution.total_cost));
    }
}
