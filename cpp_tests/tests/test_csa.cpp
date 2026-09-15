// Test suite for pure C++ CSA (Goldberg-Kennedy Cost-Scaling Assignment) solver

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_csa.h"
#include "solvers/solve_jv.h"

#include <random>

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("CSA solver - basic square matrices", "[csa][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.total_cost == Approx(15.0));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(5.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("CSA solver - rectangular matrices", "[csa][rectangular]") {
    SECTION("2x3 (more cols than rows) is optimal") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 2);
        // Optimal: row0->col0 (1) + row1->col1 (5) = 6
        REQUIRE(result.total_cost == Approx(6.0));
    }

    SECTION("3x5 is optimal") {
        auto cost = make_cost({
            {10, 1, 20, 30, 40},
            {50, 60, 2, 70, 80},
            {90, 100, 110, 3, 120}
        });

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        // Optimal: 1 (row0,col1) + 2 (row1,col2) + 3 (row2,col3) = 6
        REQUIRE(result.total_cost == Approx(6.0));

        std::vector<bool> used(5, false);
        for (int j : result.assignment) {
            REQUIRE(j >= 0);
            REQUIRE(j < 5);
            REQUIRE(!used[j]);
            used[j] = true;
        }
    }
}

TEST_CASE("CSA solver - fractional costs are optimal", "[csa][fractional]") {
    // Epsilon-scaling termination only guarantees optimality for integer costs;
    // the repair after the bidding makes the result optimal on real costs. This
    // pins the case where competing assignments differ by less than 1e-6.
    SECTION("near-tied fractional 3x3") {
        auto cost = make_cost({
            {0.0000010, 0.0000020, 0.0000030},
            {0.0000020, 0.0000010, 0.0000030},
            {0.0000030, 0.0000020, 0.0000010}
        });

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        // Diagonal is the unique optimum: 1e-6 + 1e-6 + 1e-6 = 3e-6
        REQUIRE(result.total_cost == Approx(3e-6));
    }
}

TEST_CASE("CSA solver - maximization", "[csa][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_csa(cost, true);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("CSA solver - forbidden edges", "[csa][forbidden]") {
    SECTION("some forbidden edges") {
        lap::CostMatrix cost(3, 3);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
        cost.at(1, 0) = 4;  cost.at(1, 1) = 5;  cost.at(1, 2) = 6;
        cost.at(2, 0) = 7;  cost.at(2, 1) = 8;  cost.at(2, 2) = 9;

        // Forbid the diagonal
        cost.forbid(0, 0);
        cost.forbid(1, 1);
        cost.forbid(2, 2);

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("CSA solver - empty matrix", "[csa][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.empty());
    }
}

TEST_CASE("CSA solver - infeasible cases", "[csa][infeasible]") {
    SECTION("more rows than cols throws") {
        auto cost = make_cost({
            {1, 2},
            {3, 4},
            {5, 6}
        });

        REQUIRE_THROWS_AS(lap::solve_csa(cost, false), lap::DimensionException);
    }

    SECTION("row with all forbidden throws") {
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;
        cost.at(1, 0) = 3;  cost.at(1, 1) = 4;

        cost.forbid(0, 0);
        cost.forbid(0, 1);

        REQUIRE_THROWS_AS(lap::solve_csa(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("CSA solver - larger problem", "[csa][performance]") {
    SECTION("10x10 matrix") {
        lap::CostMatrix cost(10, 10);
        for (int i = 0; i < 10; ++i) {
            for (int j = 0; j < 10; ++j) {
                cost.at(i, j) = static_cast<double>((i + 1) * (j + 1));
            }
        }

        auto result = lap::solve_csa(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 10);
    }
}

TEST_CASE("CSA solver - assignment validity", "[csa][validity]") {
    SECTION("valid permutation") {
        auto cost = make_cost({
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12}
        });

        auto result = lap::solve_csa(cost, false);

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

// A matrix of uniform costs on [0, 1); `degree` > 0 allows only that many
// random columns per row plus the diagonal, which keeps it feasible.
static lap::CostMatrix random_cost(int n, int m, unsigned seed, int degree = 0) {
    std::mt19937 rng(seed);
    std::uniform_real_distribution<double> unit(0.0, 1.0);
    std::uniform_int_distribution<int> col(0, m - 1);
    lap::CostMatrix cost(n, m);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) cost.at(i, j) = unit(rng);
    }
    if (degree > 0) {
        for (int i = 0; i < n; ++i) {
            std::vector<bool> keep(m, false);
            keep[i] = true;
            for (int d = 0; d < degree; ++d) keep[col(rng)] = true;
            for (int j = 0; j < m; ++j) {
                if (!keep[j]) cost.forbid(i, j);
            }
        }
    }
    return cost;
}

static void require_same_bids_fewer_scans(const lap::CostMatrix& cost, bool maximize) {
    lap::EpsilonScalingStats kept, scanned;
    auto csa = lap::solve_csa(cost, maximize, &kept);

    lap::EpsilonScalingOptions full;
    full.alpha = 10.0;
    full.row_search = lap::RowSearch::FullScan;
    auto reference = lap::solve_epsilon_scaling(cost, maximize, full, &scanned);

    REQUIRE(csa.assignment == reference.assignment);
    REQUIRE(kept.bids == scanned.bids);
    REQUIRE(scanned.row_scans == scanned.bids);
    REQUIRE(kept.row_scans < scanned.row_scans);

    auto jv = lap::solve_jv(cost, maximize);
    REQUIRE(csa.total_cost == Approx(jv.total_cost).margin(1e-9));
}

TEST_CASE("CSA solver - fourth-best search bids as a full scan does", "[csa][fourth_best]") {
    // Prices only fall, so the two cheapest of the kept arcs are the row's two
    // cheapest while both sit at or below the fourth-smallest reduced cost of
    // the last scan. On costs without ties the double-pushes are then the same
    // ones a full scan makes, with fewer scans.
    SECTION("dense square") {
        require_same_bids_fewer_scans(random_cost(150, 150, 11), false);
    }
    SECTION("dense rectangular") {
        require_same_bids_fewer_scans(random_cost(60, 140, 12), false);
    }
    SECTION("maximize") {
        require_same_bids_fewer_scans(random_cost(120, 120, 13), true);
    }
    SECTION("sparse rows, some holding no more arcs than the cache keeps") {
        require_same_bids_fewer_scans(random_cost(200, 200, 14, 3), false);
    }
}

TEST_CASE("CSA solver - optimal on heavily tied integer costs", "[csa][fourth_best]") {
    // Ties at the fourth-smallest reduced cost can change which of two equally
    // cheap columns a row takes, so only the optimum is compared.
    std::mt19937 rng(15);
    std::uniform_int_distribution<int> small(0, 5);
    for (int n : {20, 80, 160}) {
        lap::CostMatrix cost(n, n);
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < n; ++j) cost.at(i, j) = small(rng);
        }
        auto csa = lap::solve_csa(cost, false);
        auto jv = lap::solve_jv(cost, false);
        REQUIRE(csa.total_cost == Approx(jv.total_cost).margin(1e-9));
    }
}
