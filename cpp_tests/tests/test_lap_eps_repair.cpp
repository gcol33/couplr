// cpp_tests/tests/test_lap_eps_repair.cpp
// Completing an epsilon-optimal assignment, against enumeration.
//
// The repair is exercised directly, from assignments and prices chosen to be
// wrong, so that negative-cycle cancellation runs on every case instead of only
// when an auction happens to stop suboptimally.

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

#include "core/lap_types.h"
#include "core/lap_exact.h"
#include "core/lap_eps_repair.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <numeric>
#include <random>
#include <vector>

using lap::exact::sum_round_up;

namespace {

struct Dense {
    lap::CostMatrix cost;
    std::vector<int64_t> row_ptr;
    std::vector<int> cols;
};

Dense make_dense(const std::vector<std::vector<double>>& rows) {
    Dense d{lap::CostMatrix(rows), {}, {}};
    const int n = static_cast<int>(rows.size());
    d.row_ptr.push_back(0);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < n; ++j) d.cols.push_back(j);
        d.row_ptr.push_back(static_cast<int64_t>(d.cols.size()));
    }
    return d;
}

double total(const lap::CostMatrix& c, const std::vector<int>& col_of_row) {
    double s = 0.0;
    for (size_t i = 0; i < col_of_row.size(); ++i) s += c.at(i, col_of_row[i]);
    return s;
}

double brute_force_min(const lap::CostMatrix& c, int n) {
    std::vector<int> perm(n);
    std::iota(perm.begin(), perm.end(), 0);
    double best = std::numeric_limits<double>::infinity();
    do {
        best = std::min(best, total(c, perm));
    } while (std::next_permutation(perm.begin(), perm.end()));
    return best;
}

struct Repaired {
    std::vector<int> col_of_row;
    std::vector<double> price;
};

Repaired repair(const Dense& d, std::vector<int> col_of_row, std::vector<double> price) {
    const int n = static_cast<int>(col_of_row.size());
    std::vector<int> row_of_col(n);
    for (int i = 0; i < n; ++i) row_of_col[col_of_row[i]] = i;
    lap::detail::repair_eps_optimal(d.cost, d.row_ptr, d.cols, row_of_col, col_of_row, price);
    for (int j = 0; j < n; ++j) REQUIRE(col_of_row[row_of_col[j]] == j);
    return {col_of_row, price};
}

// Complementary slackness with u_i = c_ik - p_k: every reduced cost
// c_ij - c_ik + p_k - p_j is non-negative up to the rounding of its own
// evaluation.
void require_potentials_certify(const Dense& d, const Repaired& r) {
    const int n = static_cast<int>(r.col_of_row.size());
    for (int i = 0; i < n; ++i) {
        const int k = r.col_of_row[i];
        for (int j = 0; j < n; ++j) {
            const double cij = d.cost.at(i, j);
            const double cik = d.cost.at(i, k);
            const double reduced = ((cij - cik) + r.price[k]) - r.price[j];
            const double rounding = 8.0 * std::numeric_limits<double>::epsilon() *
                (std::fabs(cij) + std::fabs(cik) + std::fabs(r.price[j]) + std::fabs(r.price[k]));
            REQUIRE(reduced >= -rounding);
        }
    }
}

}  // namespace

TEST_CASE("sum_round_up never falls below the exact sum", "[exact][repair]") {
    const double tiny = std::ldexp(1.0, -60);
    CHECK(sum_round_up(1.0, tiny) == std::nextafter(1.0, 2.0));
    CHECK(sum_round_up(1.0, -tiny) == 1.0);
    CHECK(sum_round_up(0.5, 0.25) == 0.75);
    CHECK(sum_round_up(-3.0, 3.0) == 0.0);
    CHECK(std::isinf(sum_round_up(std::numeric_limits<double>::max(),
                                  std::numeric_limits<double>::max())));
}

TEST_CASE("repair cancels a negative cycle the starting assignment sits on", "[repair]") {
    // The identity costs 3; the rotation 0->1, 1->2, 2->0 costs 0.
    auto d = make_dense({{1, 0, 5},
                         {5, 1, 0},
                         {0, 5, 1}});
    auto r = repair(d, {0, 1, 2}, {0.0, 0.0, 0.0});
    CHECK(total(d.cost, r.col_of_row) == 0.0);
    require_potentials_certify(d, r);
}

TEST_CASE("repair leaves an optimal assignment and fixes only its prices", "[repair]") {
    auto d = make_dense({{4, 1, 3},
                         {2, 0, 5},
                         {3, 2, 2}});
    const double opt = brute_force_min(d.cost, 3);
    auto r = repair(d, {1, 0, 2}, {0.25, -0.5, 0.125});
    CHECK(total(d.cost, r.col_of_row) == opt);
    require_potentials_certify(d, r);
}

TEST_CASE("repair terminates on a matrix whose every assignment ties", "[repair]") {
    std::vector<std::vector<double>> rows(6, std::vector<double>(6, 0.1));
    auto d = make_dense(rows);
    auto r = repair(d, {5, 4, 3, 2, 1, 0}, {0.3, 0.1, 0.7, 0.2, 0.9, 0.4});
    CHECK(total(d.cost, r.col_of_row) == Catch::Approx(0.6));
    require_potentials_certify(d, r);
}

TEST_CASE("repair reaches the enumerated optimum from arbitrary starts", "[repair]") {
    std::mt19937_64 rng(20260915);
    std::uniform_real_distribution<double> unif(0.0, 1.0);
    std::lognormal_distribution<double> heavy(0.0, 3.0);

    for (int trial = 0; trial < 200; ++trial) {
        const int n = 2 + trial % 6;
        std::vector<std::vector<double>> rows(n, std::vector<double>(n));
        const double scale = (trial % 2 == 0) ? 1.0 : 1e-9;
        for (auto& row : rows) {
            for (auto& c : row) c = (trial % 2 == 0) ? unif(rng) : heavy(rng) * scale;
        }
        auto d = make_dense(rows);

        std::vector<int> start(n);
        std::iota(start.begin(), start.end(), 0);
        std::shuffle(start.begin(), start.end(), rng);
        std::vector<double> price(n);
        for (auto& p : price) p = (unif(rng) - 0.5) * scale;

        auto r = repair(d, start, price);
        INFO("trial " << trial << ", n = " << n);
        CHECK(total(d.cost, r.col_of_row) == brute_force_min(d.cost, n));
        require_potentials_certify(d, r);
    }
}
