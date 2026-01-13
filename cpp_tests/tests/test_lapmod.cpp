// Test suite for pure C++ LAPMOD solver (sparse matrices)

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_lapmod.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("LAPMOD solver - basic square matrices", "[lapmod][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_lapmod(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.total_cost == Approx(15.0));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_lapmod(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(5.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_lapmod(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("LAPMOD solver - sparse matrices", "[lapmod][sparse]") {
    SECTION("sparse 4x4 with many forbidden") {
        lap::CostMatrix cost(4, 4);
        // Set all to forbidden first
        for (int i = 0; i < 4; ++i) {
            for (int j = 0; j < 4; ++j) {
                cost.forbid(i, j);
            }
        }
        // Only allow diagonal + one off-diagonal per row
        cost.at(0, 0) = 1; cost.mask[0] = 1;
        cost.at(0, 1) = 10; cost.mask[1] = 1;
        cost.at(1, 1) = 2; cost.mask[5] = 1;
        cost.at(1, 2) = 10; cost.mask[6] = 1;
        cost.at(2, 2) = 3; cost.mask[10] = 1;
        cost.at(2, 3) = 10; cost.mask[11] = 1;
        cost.at(3, 3) = 4; cost.mask[15] = 1;
        cost.at(3, 0) = 10; cost.mask[12] = 1;

        auto result = lap::solve_lapmod(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 4);
        // Optimal is diagonal: 1+2+3+4 = 10
        REQUIRE(result.total_cost == Approx(10.0));
    }
}

TEST_CASE("LAPMOD solver - rectangular matrices", "[lapmod][rectangular]") {
    SECTION("2x3 (more cols than rows)") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_lapmod(cost, false);

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

        auto result = lap::solve_lapmod(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        REQUIRE(result.total_cost == Approx(6.0));
    }
}

TEST_CASE("LAPMOD solver - maximization", "[lapmod][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_lapmod(cost, true);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("LAPMOD solver - forbidden edges", "[lapmod][forbidden]") {
    SECTION("some forbidden edges") {
        lap::CostMatrix cost(3, 3);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
        cost.at(1, 0) = 4;  cost.at(1, 1) = 5;  cost.at(1, 2) = 6;
        cost.at(2, 0) = 7;  cost.at(2, 1) = 8;  cost.at(2, 2) = 9;

        // Forbid the diagonal
        cost.forbid(0, 0);
        cost.forbid(1, 1);
        cost.forbid(2, 2);

        auto result = lap::solve_lapmod(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("LAPMOD solver - empty matrix", "[lapmod][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_lapmod(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.empty());
    }
}

TEST_CASE("LAPMOD solver - infeasible cases", "[lapmod][infeasible]") {
    SECTION("more rows than cols throws") {
        auto cost = make_cost({
            {1, 2},
            {3, 4},
            {5, 6}
        });

        REQUIRE_THROWS_AS(lap::solve_lapmod(cost, false), lap::DimensionException);
    }

    SECTION("row with all forbidden throws") {
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;
        cost.at(1, 0) = 3;  cost.at(1, 1) = 4;

        cost.forbid(0, 0);
        cost.forbid(0, 1);

        REQUIRE_THROWS_AS(lap::solve_lapmod(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("LAPMOD solver - assignment validity", "[lapmod][validity]") {
    SECTION("valid permutation") {
        auto cost = make_cost({
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12}
        });

        auto result = lap::solve_lapmod(cost, false);

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
