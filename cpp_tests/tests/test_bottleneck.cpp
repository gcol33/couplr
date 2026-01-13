// Test suite for pure C++ Bottleneck Assignment solver

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_bottleneck.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("Bottleneck solver - basic square matrices", "[bottleneck][basic]") {
    SECTION("3x3 matrix - minimize max cost") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_bottleneck(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        // Bottleneck finds matching minimizing the maximum edge
        // Optimal: (0,2)=3, (1,1)=5, (2,0)=7 -> max = 7
        // Or: (0,0)=1, (1,2)=6, (2,1)=8 -> max = 8
        // Or: (0,1)=2, (1,0)=4, (2,2)=9 -> max = 9
        // Best is 7
        REQUIRE(result.total_cost == Approx(7.0));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_bottleneck(cost, false);

        REQUIRE(result.status == "optimal");
        // Options: (0,0)+(1,1) -> max(1,4)=4, or (0,1)+(1,0) -> max(2,3)=3
        REQUIRE(result.total_cost == Approx(3.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_bottleneck(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("Bottleneck solver - rectangular matrices", "[bottleneck][rectangular]") {
    SECTION("2x3 (more cols than rows)") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_bottleneck(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
        // Best: (0,0)=1, (1,1)=5 -> max=5
        // Or: (0,1)=2, (1,0)=4 -> max=4
        REQUIRE(result.total_cost == Approx(4.0));
    }

    SECTION("3x5 with clear structure") {
        auto cost = make_cost({
            {10, 1, 20, 30, 40},
            {50, 60, 2, 70, 80},
            {90, 100, 110, 3, 120}
        });

        auto result = lap::solve_bottleneck(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        // Optimal picks columns with small values: max(1, 2, 3) = 3
        REQUIRE(result.total_cost == Approx(3.0));
    }
}

TEST_CASE("Bottleneck solver - maximization (maximin)", "[bottleneck][maximize]") {
    SECTION("3x3 maximize min edge") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_bottleneck(cost, true);

        REQUIRE(result.status == "optimal");
        // Maximin: maximize the minimum edge in matching
        // (0,2)=3, (1,1)=5, (2,0)=7 -> min = 3
        // (0,0)=1, (1,2)=6, (2,1)=8 -> min = 1
        // (0,1)=2, (1,0)=4, (2,2)=9 -> min = 2
        // Best is 3
        REQUIRE(result.total_cost == Approx(3.0));
    }
}

TEST_CASE("Bottleneck solver - uniform costs", "[bottleneck][uniform]") {
    SECTION("all equal costs") {
        auto cost = make_cost({
            {5, 5, 5},
            {5, 5, 5},
            {5, 5, 5}
        });

        auto result = lap::solve_bottleneck(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(5.0));
    }
}

TEST_CASE("Bottleneck solver - forbidden edges", "[bottleneck][forbidden]") {
    SECTION("some forbidden edges") {
        lap::CostMatrix cost(3, 3);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
        cost.at(1, 0) = 4;  cost.at(1, 1) = 5;  cost.at(1, 2) = 6;
        cost.at(2, 0) = 7;  cost.at(2, 1) = 8;  cost.at(2, 2) = 9;

        // Forbid the optimal (0,2), (1,1), (2,0)
        cost.forbid(0, 2);

        auto result = lap::solve_bottleneck(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
    }
}

TEST_CASE("Bottleneck solver - empty matrix", "[bottleneck][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_bottleneck(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.empty());
    }
}

TEST_CASE("Bottleneck solver - infeasible cases", "[bottleneck][infeasible]") {
    SECTION("more rows than cols throws") {
        auto cost = make_cost({
            {1, 2},
            {3, 4},
            {5, 6}
        });

        REQUIRE_THROWS_AS(lap::solve_bottleneck(cost, false), lap::DimensionException);
    }

    SECTION("row with all forbidden throws") {
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;
        cost.at(1, 0) = 3;  cost.at(1, 1) = 4;

        cost.forbid(0, 0);
        cost.forbid(0, 1);

        REQUIRE_THROWS_AS(lap::solve_bottleneck(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("Bottleneck solver - assignment validity", "[bottleneck][validity]") {
    SECTION("valid permutation") {
        auto cost = make_cost({
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12}
        });

        auto result = lap::solve_bottleneck(cost, false);

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
