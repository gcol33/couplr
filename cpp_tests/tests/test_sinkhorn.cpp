// Test suite for pure C++ Sinkhorn solver (entropy-regularized optimal transport)

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_sinkhorn.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("Sinkhorn solver - basic square matrices", "[sinkhorn][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_sinkhorn(cost, 10.0);

        REQUIRE(result.converged);
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.transport_matrix.size() == 3);
        REQUIRE(result.transport_matrix[0].size() == 3);
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_sinkhorn(cost, 10.0);

        REQUIRE(result.converged);
        REQUIRE(result.assignment.size() == 2);
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_sinkhorn(cost, 10.0);

        REQUIRE(result.converged);
        REQUIRE(result.assignment.size() == 1);
        REQUIRE(result.assignment[0] == 0);
    }
}

TEST_CASE("Sinkhorn solver - lambda parameter", "[sinkhorn][lambda]") {
    auto cost = make_cost({
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    });

    SECTION("moderate lambda = more discrete") {
        // Use moderate lambda (high values can cause numerical issues)
        auto result = lap::solve_sinkhorn(cost, 20.0, 1e-9, 2000);

        REQUIRE(result.converged);
        REQUIRE(result.lambda == 20.0);
        // Transport matrix should be closer to permutation
    }

    SECTION("low lambda = more spread") {
        auto result = lap::solve_sinkhorn(cost, 1.0);

        REQUIRE(result.converged);
        REQUIRE(result.lambda == 1.0);
        // Transport matrix should be more diffuse
    }
}

TEST_CASE("Sinkhorn solver - rectangular matrices", "[sinkhorn][rectangular]") {
    SECTION("2x3 (more cols than rows)") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_sinkhorn(cost, 10.0);

        REQUIRE(result.converged);
        REQUIRE(result.assignment.size() == 2);
        REQUIRE(result.transport_matrix.size() == 2);
        REQUIRE(result.transport_matrix[0].size() == 3);
    }
}

TEST_CASE("Sinkhorn solver - transport matrix properties", "[sinkhorn][transport]") {
    auto cost = make_cost({
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    });

    auto result = lap::solve_sinkhorn(cost, 10.0);

    SECTION("all values non-negative") {
        for (const auto& row : result.transport_matrix) {
            for (double val : row) {
                REQUIRE(val >= 0.0);
            }
        }
    }

    SECTION("rows sum to approximately 1/n") {
        for (const auto& row : result.transport_matrix) {
            double sum = 0;
            for (double val : row) {
                sum += val;
            }
            REQUIRE(sum == Approx(1.0 / 3.0).margin(0.01));
        }
    }

    SECTION("cols sum to approximately 1/m") {
        for (int j = 0; j < 3; ++j) {
            double sum = 0;
            for (int i = 0; i < 3; ++i) {
                sum += result.transport_matrix[i][j];
            }
            REQUIRE(sum == Approx(1.0 / 3.0).margin(0.01));
        }
    }
}

TEST_CASE("Sinkhorn solver - convergence", "[sinkhorn][convergence]") {
    auto cost = make_cost({
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    });

    SECTION("converges within iteration limit") {
        auto result = lap::solve_sinkhorn(cost, 10.0, 1e-9, 1000);

        REQUIRE(result.converged);
        REQUIRE(result.iterations > 0);
        REQUIRE(result.iterations <= 1000);
    }

    SECTION("tight tolerance still converges") {
        auto result = lap::solve_sinkhorn(cost, 10.0, 1e-12, 2000);

        REQUIRE(result.converged);
    }
}

TEST_CASE("Sinkhorn solver - sinkhorn_round", "[sinkhorn][round]") {
    auto cost = make_cost({
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    });

    auto result = lap::solve_sinkhorn(cost, 10.0);

    SECTION("rounded assignment is valid") {
        auto rounded = lap::sinkhorn_round(result.transport_matrix);

        REQUIRE(rounded.size() == 3);

        std::vector<bool> used(3, false);
        for (int j : rounded) {
            if (j >= 0) {
                REQUIRE(j < 3);
                REQUIRE(!used[j]);
                used[j] = true;
            }
        }
    }
}

TEST_CASE("Sinkhorn solver - empty matrix", "[sinkhorn][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_sinkhorn(cost, 10.0);

        REQUIRE(result.assignment.empty());
        REQUIRE(result.transport_matrix.empty());
    }
}

TEST_CASE("Sinkhorn solver - custom weights", "[sinkhorn][weights]") {
    auto cost = make_cost({
        {1, 2, 3},
        {4, 5, 6},
        {7, 8, 9}
    });

    SECTION("uniform weights (default)") {
        auto result = lap::solve_sinkhorn(cost, 10.0);
        REQUIRE(result.converged);
    }

    SECTION("custom row weights") {
        std::vector<double> r = {0.5, 0.3, 0.2};
        auto result = lap::solve_sinkhorn(cost, 10.0, 1e-9, 1000, r, {});
        REQUIRE(result.converged);
    }

    SECTION("custom both weights") {
        std::vector<double> r = {0.5, 0.3, 0.2};
        std::vector<double> c = {0.4, 0.4, 0.2};
        auto result = lap::solve_sinkhorn(cost, 10.0, 1e-9, 1000, r, c);
        REQUIRE(result.converged);
    }
}
