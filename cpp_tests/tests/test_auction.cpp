// Test suite for pure C++ Auction solvers

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_auction.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

// ============================================================================
// Basic Auction
// ============================================================================

TEST_CASE("Auction solver - basic square matrices", "[auction][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_auction(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.total_cost == Approx(15.0).margin(0.1));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_auction(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(5.0).margin(0.1));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_auction(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(42.0).margin(0.1));
    }
}

TEST_CASE("Auction solver - rectangular matrices", "[auction][rectangular]") {
    SECTION("2x3 (more cols than rows)") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_auction(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
        REQUIRE(result.total_cost == Approx(6.0).margin(0.1));
    }
}

TEST_CASE("Auction solver - maximization", "[auction][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_auction(cost, true);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0).margin(0.1));
    }
}

TEST_CASE("Auction solver - infeasible cases", "[auction][infeasible]") {
    SECTION("more rows than cols throws") {
        auto cost = make_cost({
            {1, 2},
            {3, 4},
            {5, 6}
        });

        REQUIRE_THROWS_AS(lap::solve_auction(cost, false), lap::DimensionException);
    }
}

// ============================================================================
// Scaled Auction
// ============================================================================

TEST_CASE("Auction scaled solver - basic", "[auction_scaled][basic]") {
    SECTION("3x3 matrix alpha7") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_auction_scaled(cost, false, "alpha7");

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0).margin(0.1));
    }

    SECTION("3x3 matrix pow2") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_auction_scaled(cost, false, "pow2");

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0).margin(0.1));
    }

    SECTION("3x3 matrix halves") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_auction_scaled(cost, false, "halves");

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0).margin(0.1));
    }
}

// ============================================================================
// Gauss-Seidel Auction
// ============================================================================

TEST_CASE("Auction GS solver - basic", "[auction_gs][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_auction_gs(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0).margin(0.1));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_auction_gs(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(5.0).margin(0.1));
    }
}

TEST_CASE("Auction GS solver - maximization", "[auction_gs][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_auction_gs(cost, true);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0).margin(0.1));
    }
}

// ============================================================================
// Auction with custom parameters
// ============================================================================

TEST_CASE("Auction scaled params - basic", "[auction_scaled_params][basic]") {
    SECTION("3x3 matrix with custom params") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_auction_scaled_params(cost, false, 1.0, 5.0, 1e-6);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0).margin(0.1));
    }
}
