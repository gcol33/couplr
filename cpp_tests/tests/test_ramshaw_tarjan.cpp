// Test suite for pure C++ Ramshaw-Tarjan solver (optimized for rectangular)

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_ramshaw_tarjan.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("Ramshaw-Tarjan solver - basic square matrices", "[ramshaw_tarjan][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.total_cost == Approx(15.0));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(5.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("Ramshaw-Tarjan solver - rectangular matrices", "[ramshaw_tarjan][rectangular]") {
    // This is the main use case for Ramshaw-Tarjan
    SECTION("2x3 (more cols than rows)") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
        REQUIRE(result.total_cost == Approx(6.0));
    }

    SECTION("3x5 with clear optimum") {
        auto cost = make_cost({
            {10, 1, 20, 30, 40},
            {50, 60, 2, 70, 80},
            {90, 100, 110, 3, 120}
        });

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        REQUIRE(result.total_cost == Approx(6.0));
    }

    SECTION("2x10 highly rectangular") {
        lap::CostMatrix cost(2, 10);
        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 10; ++j) {
                cost.at(i, j) = static_cast<double>((i + 1) * (j + 1));
            }
        }

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
    }
}

TEST_CASE("Ramshaw-Tarjan solver - auto-transpose", "[ramshaw_tarjan][transpose]") {
    // Ramshaw-Tarjan should handle nrow > ncol by transposing
    SECTION("3x2 (more rows than cols) - should transpose internally") {
        auto cost = make_cost({
            {1, 2},
            {3, 4},
            {5, 6}
        });

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        // After transpose, we match 2 rows
        REQUIRE(result.n_matched() == 2);
    }
}

TEST_CASE("Ramshaw-Tarjan solver - maximization", "[ramshaw_tarjan][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_ramshaw_tarjan(cost, true);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("Ramshaw-Tarjan solver - forbidden edges", "[ramshaw_tarjan][forbidden]") {
    SECTION("some forbidden edges") {
        lap::CostMatrix cost(3, 3);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
        cost.at(1, 0) = 4;  cost.at(1, 1) = 5;  cost.at(1, 2) = 6;
        cost.at(2, 0) = 7;  cost.at(2, 1) = 8;  cost.at(2, 2) = 9;

        // Forbid the diagonal
        cost.forbid(0, 0);
        cost.forbid(1, 1);
        cost.forbid(2, 2);

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("Ramshaw-Tarjan solver - empty matrix", "[ramshaw_tarjan][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.empty());
    }
}

TEST_CASE("Ramshaw-Tarjan solver - infeasible cases", "[ramshaw_tarjan][infeasible]") {
    SECTION("row with all Inf values throws") {
        // Use actual infinity, not forbid() which uses BIG (finite)
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = std::numeric_limits<double>::infinity();
        cost.at(0, 1) = std::numeric_limits<double>::infinity();
        cost.at(1, 0) = 3;
        cost.at(1, 1) = 4;

        REQUIRE_THROWS_AS(lap::solve_ramshaw_tarjan(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("Ramshaw-Tarjan solver - assignment validity", "[ramshaw_tarjan][validity]") {
    SECTION("valid permutation") {
        auto cost = make_cost({
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12}
        });

        auto result = lap::solve_ramshaw_tarjan(cost, false);

        REQUIRE(result.assignment.size() == 3);

        std::vector<bool> used(4, false);
        for (int j : result.assignment) {
            REQUIRE(j >= 0);
            REQUIRE(j < 4);
            REQUIRE(!used[j]);
            used[j] = true;
        }
    }
}
