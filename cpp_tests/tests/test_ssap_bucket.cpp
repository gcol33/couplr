// Test suite for pure C++ SSAP with Dial's bucket queue (integer costs)

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include <cmath>
#include <random>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_ssap_bucket.h"
#include "solvers/solve_bruteforce.h"

using Catch::Approx;

static lap::CostMatrix make_cost(std::initializer_list<std::initializer_list<double>> rows) {
    std::vector<std::vector<double>> data;
    for (const auto& row : rows) {
        data.push_back(std::vector<double>(row));
    }
    return lap::CostMatrix(data);
}

TEST_CASE("SSAP Bucket solver - basic square matrices", "[ssap_bucket][basic]") {
    SECTION("3x3 matrix") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_ssap_bucket(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        REQUIRE(result.total_cost == Approx(15.0));
    }

    SECTION("2x2 simple") {
        auto cost = make_cost({
            {1, 2},
            {3, 4}
        });

        auto result = lap::solve_ssap_bucket(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(5.0));
    }

    SECTION("1x1 trivial") {
        auto cost = make_cost({{42.0}});

        auto result = lap::solve_ssap_bucket(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(42.0));
    }
}

TEST_CASE("SSAP Bucket solver - integer costs", "[ssap_bucket][integer]") {
    // SSAP Bucket is optimized for integer costs
    SECTION("small integers") {
        auto cost = make_cost({
            {1, 2, 3},
            {2, 3, 4},
            {3, 4, 5}
        });

        auto result = lap::solve_ssap_bucket(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
    }

    SECTION("larger integers") {
        auto cost = make_cost({
            {100, 200, 300},
            {150, 250, 350},
            {200, 300, 400}
        });

        auto result = lap::solve_ssap_bucket(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
    }
}

TEST_CASE("SSAP Bucket solver - rectangular matrices", "[ssap_bucket][rectangular]") {
    SECTION("2x3 (more cols than rows)") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6}
        });

        auto result = lap::solve_ssap_bucket(cost, false);

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

        auto result = lap::solve_ssap_bucket(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.n_matched() == 3);
        REQUIRE(result.total_cost == Approx(6.0));
    }
}

TEST_CASE("SSAP Bucket solver - maximization", "[ssap_bucket][maximize]") {
    SECTION("3x3 maximize") {
        auto cost = make_cost({
            {1, 2, 3},
            {4, 5, 6},
            {7, 8, 9}
        });

        auto result = lap::solve_ssap_bucket(cost, true);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("SSAP Bucket solver - forbidden edges", "[ssap_bucket][forbidden]") {
    SECTION("some forbidden edges") {
        lap::CostMatrix cost(3, 3);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;  cost.at(0, 2) = 3;
        cost.at(1, 0) = 4;  cost.at(1, 1) = 5;  cost.at(1, 2) = 6;
        cost.at(2, 0) = 7;  cost.at(2, 1) = 8;  cost.at(2, 2) = 9;

        // Forbid the diagonal
        cost.forbid(0, 0);
        cost.forbid(1, 1);
        cost.forbid(2, 2);

        auto result = lap::solve_ssap_bucket(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.total_cost == Approx(15.0));
    }
}

TEST_CASE("SSAP Bucket solver - empty matrix", "[ssap_bucket][edge]") {
    SECTION("empty matrix") {
        lap::CostMatrix cost(0, 0);

        auto result = lap::solve_ssap_bucket(cost, false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.empty());
    }
}

TEST_CASE("SSAP Bucket solver - infeasible cases", "[ssap_bucket][infeasible]") {
    SECTION("row with all forbidden throws") {
        lap::CostMatrix cost(2, 2);
        cost.at(0, 0) = 1;  cost.at(0, 1) = 2;
        cost.at(1, 0) = 3;  cost.at(1, 1) = 4;

        cost.forbid(0, 0);
        cost.forbid(0, 1);

        REQUIRE_THROWS_AS(lap::solve_ssap_bucket(cost, false), lap::InfeasibleException);
    }
}

TEST_CASE("SSAP Bucket solver - assignment validity", "[ssap_bucket][validity]") {
    SECTION("valid permutation") {
        auto cost = make_cost({
            {1, 2, 3, 4},
            {5, 6, 7, 8},
            {9, 10, 11, 12}
        });

        auto result = lap::solve_ssap_bucket(cost, false);

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

TEST_CASE("SSAP Bucket solver - fractional precision never yields a wrong optimum",
          "[ssap_bucket][fractional][regression]") {
    // Regression for gcol33/couplr#19: find_scale_factor used to fall back to
    // scale 1000 + round, silently flipping the optimum on costs needing more
    // than three decimals (differential fuzz found true -2.622920 returned as
    // -2.621840). It must now either scale exactly or throw - never return a
    // rounded, wrong optimum.

    SECTION("six-decimal costs that scale exactly are solved, not rounded") {
        // Every entry is an exact multiple of 1e-6, so the power-of-ten search
        // must reach scale 1e6 instead of rounding at 1e3.
        auto cost = make_cost({
            {0.142857, 0.285714, 0.428571},
            {0.571428, 0.714285, 0.857142},
            {0.999999, 0.111111, 0.222222}
        });
        auto got = lap::solve_ssap_bucket(cost, false);
        auto truth = lap::solve_bruteforce(cost, false);
        REQUIRE(got.total_cost == Approx(truth.total_cost).margin(1e-9));
    }

    SECTION("differential fuzz: exact optimum or clean throw, never wrong") {
        std::mt19937 rng(0x55A9u);  // fixed seed: every failure reproducible
        std::uniform_int_distribution<int> ndist(2, 5);
        std::uniform_real_distribution<double> cdist(0.0, 3.0);
        std::bernoulli_distribution exact_p(0.5);

        for (int trial = 0; trial < 5000; ++trial) {
            int n = ndist(rng);
            int m = n + std::uniform_int_distribution<int>(0, 1)(rng);
            bool maximize = (trial % 2 == 0);
            bool make_exact = exact_p(rng);

            lap::CostMatrix c(n, m);
            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < m; ++j) {
                    double v = cdist(rng);
                    // Half the trials use exactly-representable six-decimal
                    // costs (scale 1e6 makes them integral); the rest use raw
                    // reals that generally admit no bounded 10^-k scaling.
                    if (make_exact) v = std::round(v * 1e6) / 1e6;
                    c.at(i, j) = v;
                }
            }

            double truth = lap::solve_bruteforce(c, maximize).total_cost;

            lap::LapResult got;
            bool threw = false;
            try { got = lap::solve_ssap_bucket(c, maximize); }
            catch (const lap::LapException&) { threw = true; }

            INFO("trial=" << trial << " n=" << n << " m=" << m
                 << " maximize=" << maximize << " make_exact=" << make_exact);

            if (make_exact) {
                // Exactly integer-scalable within budget: must solve, not reject.
                REQUIRE_FALSE(threw);
            }
            if (threw) continue;  // rejecting rather than rounding is allowed
            CHECK(got.total_cost == Approx(truth).margin(1e-9));
        }
    }
}
