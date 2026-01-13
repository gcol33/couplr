// Test suite for pure C++ line metric solver
// Tests the 1D assignment algorithm directly without Rcpp

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>
#include <catch2/matchers/catch_matchers_exception.hpp>

#include "core/lap_types.h"
#include "core/lap_error.h"
#include "solvers/solve_line_metric.h"

using Catch::Approx;

TEST_CASE("Line metric solver - square L1 matching", "[line_metric][L1][square]") {
    SECTION("simple 3x3 - already sorted") {
        std::vector<double> x = {1.0, 2.0, 3.0};
        std::vector<double> y = {1.5, 2.5, 3.5};

        auto result = lap::solve_line_metric(x, y, "L1", false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        // L1 cost: |1-1.5| + |2-2.5| + |3-3.5| = 0.5 + 0.5 + 0.5 = 1.5
        REQUIRE(result.total_cost == Approx(1.5));
    }

    SECTION("3x3 unsorted") {
        std::vector<double> x = {3.0, 1.0, 2.0};
        std::vector<double> y = {2.5, 3.5, 1.5};

        auto result = lap::solve_line_metric(x, y, "L1", false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        // Sorted: x = [1,2,3], y = [1.5,2.5,3.5]
        // L1 cost: 0.5 + 0.5 + 0.5 = 1.5
        REQUIRE(result.total_cost == Approx(1.5));
    }

    SECTION("1x1 trivial") {
        std::vector<double> x = {5.0};
        std::vector<double> y = {3.0};

        auto result = lap::solve_line_metric(x, y, "L1", false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 1);
        REQUIRE(result.assignment[0] == 0);
        REQUIRE(result.total_cost == Approx(2.0));
    }
}

TEST_CASE("Line metric solver - square L2 matching", "[line_metric][L2][square]") {
    SECTION("simple 3x3 - squared distances") {
        std::vector<double> x = {1.0, 2.0, 3.0};
        std::vector<double> y = {1.5, 2.5, 3.5};

        auto result = lap::solve_line_metric(x, y, "L2", false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        // L2 cost: 0.5^2 + 0.5^2 + 0.5^2 = 0.25 + 0.25 + 0.25 = 0.75
        REQUIRE(result.total_cost == Approx(0.75));
    }

    SECTION("L2 with larger differences") {
        std::vector<double> x = {0.0, 10.0};
        std::vector<double> y = {1.0, 11.0};

        auto result = lap::solve_line_metric(x, y, "L2", false);

        REQUIRE(result.status == "optimal");
        // L2: 1^2 + 1^2 = 2
        REQUIRE(result.total_cost == Approx(2.0));
    }
}

TEST_CASE("Line metric solver - rectangular cases", "[line_metric][rectangular]") {
    SECTION("2x3 - more y than x") {
        std::vector<double> x = {1.0, 3.0};
        std::vector<double> y = {1.0, 2.0, 3.0};

        auto result = lap::solve_line_metric(x, y, "L1", false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 2);
        // Optimal: match 1->1 (cost 0) and 3->3 (cost 0) = 0
        REQUIRE(result.total_cost == Approx(0.0));
    }

    SECTION("2x4 - skip middle elements") {
        std::vector<double> x = {0.0, 10.0};
        std::vector<double> y = {0.0, 3.0, 7.0, 10.0};

        auto result = lap::solve_line_metric(x, y, "L1", false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 2);
        // Optimal: match 0->0 (cost 0) and 10->10 (cost 0)
        REQUIRE(result.total_cost == Approx(0.0));
    }

    SECTION("3x5 rectangular L2") {
        std::vector<double> x = {1.0, 5.0, 9.0};
        std::vector<double> y = {1.0, 3.0, 5.0, 7.0, 9.0};

        auto result = lap::solve_line_metric(x, y, "L2", false);

        REQUIRE(result.status == "optimal");
        REQUIRE(result.assignment.size() == 3);
        // Optimal: 1->1, 5->5, 9->9, total cost = 0
        REQUIRE(result.total_cost == Approx(0.0));
    }
}

TEST_CASE("Line metric solver - maximization", "[line_metric][maximize]") {
    SECTION("maximize L1") {
        std::vector<double> x = {1.0, 2.0};
        std::vector<double> y = {1.0, 2.0};

        auto result = lap::solve_line_metric(x, y, "L1", true);

        REQUIRE(result.status == "optimal");
        // For maximize, cost is negated: sorted match gives 0, so max = -0 = 0
        // Actually maximize returns the negated cost, so it should still be 0
        REQUIRE(result.total_cost == Approx(0.0));
    }
}

TEST_CASE("Line metric solver - cost type parsing", "[line_metric][cost_type]") {
    std::vector<double> x = {1.0};
    std::vector<double> y = {2.0};

    SECTION("L1 variants") {
        REQUIRE(lap::solve_line_metric(x, y, "L1", false).total_cost == Approx(1.0));
        REQUIRE(lap::solve_line_metric(x, y, "l1", false).total_cost == Approx(1.0));
        REQUIRE(lap::solve_line_metric(x, y, "abs", false).total_cost == Approx(1.0));
        REQUIRE(lap::solve_line_metric(x, y, "manhattan", false).total_cost == Approx(1.0));
    }

    SECTION("L2 variants") {
        REQUIRE(lap::solve_line_metric(x, y, "L2", false).total_cost == Approx(1.0));
        REQUIRE(lap::solve_line_metric(x, y, "l2", false).total_cost == Approx(1.0));
        REQUIRE(lap::solve_line_metric(x, y, "sq", false).total_cost == Approx(1.0));
        REQUIRE(lap::solve_line_metric(x, y, "squared", false).total_cost == Approx(1.0));
        REQUIRE(lap::solve_line_metric(x, y, "quadratic", false).total_cost == Approx(1.0));
    }

    SECTION("invalid cost type throws") {
        REQUIRE_THROWS(lap::solve_line_metric(x, y, "invalid", false));
    }
}

TEST_CASE("Line metric solver - edge cases", "[line_metric][edge]") {
    SECTION("n > m throws") {
        std::vector<double> x = {1.0, 2.0, 3.0};
        std::vector<double> y = {1.0, 2.0};

        REQUIRE_THROWS_AS(lap::solve_line_metric(x, y, "L1", false), lap::DimensionException);
    }

    SECTION("empty x throws") {
        std::vector<double> x = {};
        std::vector<double> y = {1.0};

        REQUIRE_THROWS_AS(lap::solve_line_metric(x, y, "L1", false), lap::DimensionException);
    }

    SECTION("empty y throws") {
        std::vector<double> x = {1.0};
        std::vector<double> y = {};

        REQUIRE_THROWS_AS(lap::solve_line_metric(x, y, "L1", false), lap::DimensionException);
    }
}

TEST_CASE("Line metric solver - assignment validity", "[line_metric][validity]") {
    SECTION("assignment is valid - square case") {
        std::vector<double> x = {5.0, 1.0, 3.0, 2.0};
        std::vector<double> y = {4.0, 2.0, 6.0, 0.0};

        auto result = lap::solve_line_metric(x, y, "L1", false);

        REQUIRE(result.assignment.size() == 4);

        // Check all columns are valid
        for (int j : result.assignment) {
            REQUIRE(j >= 0);
            REQUIRE(j < 4);
        }

        // Check no column is used twice
        std::vector<bool> used(4, false);
        for (int j : result.assignment) {
            REQUIRE(!used[j]);
            used[j] = true;
        }
    }

    SECTION("assignment is valid - rectangular case") {
        std::vector<double> x = {1.0, 3.0};
        std::vector<double> y = {0.0, 1.0, 2.0, 3.0, 4.0};

        auto result = lap::solve_line_metric(x, y, "L1", false);

        REQUIRE(result.assignment.size() == 2);

        // Check all columns are valid
        for (int j : result.assignment) {
            REQUIRE(j >= 0);
            REQUIRE(j < 5);
        }

        // Check no column is used twice
        std::vector<bool> used(5, false);
        for (int j : result.assignment) {
            REQUIRE(!used[j]);
            used[j] = true;
        }
    }
}
