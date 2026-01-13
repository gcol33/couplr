// Test suite for pure C++ Hopcroft-Karp solver (binary/uniform costs)

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_hk01.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("HK01 solver - uniform cost matrices", "[hk01][uniform]") {
    SECTION("3x3 all ones") {
        auto cost = make_cost({
            {1, 1, 1},
            {1, 1, 1},
            {1, 1, 1}
        });

        auto result = lap::solve_hk01(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.n_matched() == 3);
        REQUIRE(result.total_cost == Approx(3.0));
    }

    SECTION("2x2 uniform") {
        auto cost = make_cost({
            {5, 5},
            {5, 5}
        });

        auto result = lap::solve_hk01(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
        REQUIRE(result.total_cost == Approx(10.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_hk01(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("HK01 solver - binary {0,1} costs", "[hk01][binary]") {
    SECTION("3x3 binary") {
        auto cost = make_cost({
            {0, 1, 1},
            {1, 0, 1},
            {1, 1, 0}
        });

        auto result = lap::solve_hk01(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        // Optimal: diagonal with all 0s
        REQUIRE(result.total_cost == Approx(0.0));
    }

    SECTION("2x3 binary") {
        auto cost = make_cost({
            {0, 1, 1},
            {1, 0, 1}
        });

        auto result = lap::solve_hk01(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
        REQUIRE(result.total_cost == Approx(0.0));
    }
}

TEST_CASE("HK01 solver - rectangular matrices", "[hk01][rectangular]") {
    SECTION("2x4 uniform") {
        auto cost = make_cost({
            {1, 1, 1, 1},
            {1, 1, 1, 1}
        });

        auto result = lap::solve_hk01(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
        REQUIRE(result.total_cost == Approx(2.0));
    }
}

TEST_CASE("HK01 solver - forbidden edges", "[hk01][forbidden]") {
    SECTION("uniform with some forbidden") {
        lap::CostMatrix cost(3, 3);
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                cost.at(i, j) = 1.0;
            }
        }
        // Forbid diagonal
        cost.forbid(0, 0);
        cost.forbid(1, 1);
        cost.forbid(2, 2);

        auto result = lap::solve_hk01(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        REQUIRE(result.total_cost == Approx(3.0));
    }
}

TEST_CASE("HK01 solver - empty matrix", "[hk01][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_hk01(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.empty());
    }
}

TEST_CASE("HK01 solver - infeasible cases", "[hk01][infeasible]") {
    SECTION("more rows than cols throws") {
        auto cost = make_cost({
            {1, 1},
            {1, 1},
            {1, 1}
        });

        REQUIRE_THROWS_AS(lap::solve_hk01(cost, false), lap::DimensionException);
    }

    SECTION("row with all forbidden throws") {
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 1;
        cost.at(1, 0) = 1;  cost.at(1, 1) = 1;

        cost.forbid(0, 0);
        cost.forbid(0, 1);

        REQUIRE_THROWS_AS(lap::solve_hk01(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("HK01 solver - assignment validity", "[hk01][validity]") {
    SECTION("valid permutation") {
        auto cost = make_cost({
            {1, 1, 1, 1},
            {1, 1, 1, 1},
            {1, 1, 1, 1}
        });

        auto result = lap::solve_hk01(cost, false);

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
