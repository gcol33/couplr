// Test suite for pure C++ Network Simplex solver

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/network_simplex/solve_network_simplex.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("Network Simplex solver - basic square matrices", "[network_simplex][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_network_simplex(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.total_cost == Approx(15.0));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_network_simplex(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(5.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_network_simplex(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("Network Simplex solver - rectangular matrices", "[network_simplex][rectangular]") {
    SECTION("2x3 (more cols than rows)") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_network_simplex(cost, false);

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

        auto result = lap::solve_network_simplex(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        REQUIRE(result.total_cost == Approx(6.0));
    }
}

TEST_CASE("Network Simplex solver - maximization", "[network_simplex][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_network_simplex(cost, true);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("Network Simplex solver - forbidden edges", "[network_simplex][forbidden]") {
    SECTION("high cost edges treated as soft forbidden") {
        // Network Simplex doesn't support hard forbidden edges via BIG values
        // Instead, test with a matrix where some edges are much more expensive
        lap::CostMatrix cost(3, 3);
        cost.at(0, 0) = 100;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
        cost.at(1, 0) = 4;    cost.at(1, 1) = 100;  cost.at(1, 2) = 6;
        cost.at(2, 0) = 7;    cost.at(2, 1) = 8;  cost.at(2, 2) = 100;

        auto result = lap::solve_network_simplex(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        // High-cost edges should be avoided
        // Optimal is off-diagonal: 2+6+7=15 or 3+4+8=15
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("Network Simplex solver - empty matrix", "[network_simplex][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_network_simplex(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.empty());
    }
}

TEST_CASE("Network Simplex solver - infeasible cases", "[network_simplex][infeasible]") {
    SECTION("more rows than cols throws") {
        auto cost = make_cost({
            {1, 2},
            {3, 4},
            {5, 6}
        });

        REQUIRE_THROWS_AS(lap::solve_network_simplex(cost, false), lap::DimensionException);
    }

    SECTION("row with all forbidden throws") {
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;
        cost.at(1, 0) = 3;  cost.at(1, 1) = 4;

        cost.forbid(0, 0);
        cost.forbid(0, 1);

        REQUIRE_THROWS_AS(lap::solve_network_simplex(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("Network Simplex solver - larger problem", "[network_simplex][performance]") {
    SECTION("10x10 matrix") {
        lap::CostMatrix cost(10, 10);
        for (int i = 0; i < 10; ++i) {
            for (int j = 0; j < 10; ++j) {
                cost.at(i, j) = static_cast<double>((i + 1) * (j + 1));
            }
        }

        auto result = lap::solve_network_simplex(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 10);
    }
}

TEST_CASE("Network Simplex solver - assignment validity", "[network_simplex][validity]") {
    SECTION("valid permutation") {
        auto cost = make_cost({
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12}
        });

        auto result = lap::solve_network_simplex(cost, false);

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
