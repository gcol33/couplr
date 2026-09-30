// cpp_tests/tests/test_lap_exact_potentials.cpp
// Expansions, and the exact potentials recovered from an optimal primal.
//
// Doubles of the form k * 2^e with e spread over sixty binades are exact on a
// grid of 2^-20, and scaling by 2^20 turns every sum of them into an integer a
// 128-bit type holds, which settles each value and sign independently of the
// arithmetic under test. The spread is what makes the sums need more than one
// double: a value of 2^60 plus one of 2^-20 has no double representation.

#include <catch2/catch_test_macros.hpp>

#include "core/lap_exact.h"
#include "core/lap_exact_potentials.h"
#include "core/lap_types.h"
#include "solvers/solve_jv_duals.h"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <random>
#include <vector>

namespace ex = lap::exact;

namespace {

using i128 = __int128;

constexpr int kGridBits = 20;

// A double on the grid, and the integer it is after scaling by 2^20.
struct Gridded {
    double value;
    i128 scaled;
};

Gridded draw(std::mt19937_64& rng) {
    std::uniform_int_distribution<int64_t> mant(-(int64_t{1} << 20), int64_t{1} << 20);
    std::uniform_int_distribution<int> expo(-kGridBits, 40);
    const int64_t k = mant(rng);
    const int e = expo(rng);
    Gridded g;
    g.value = std::ldexp(static_cast<double>(k), e);
    g.scaled = static_cast<i128>(k) << (e + kGridBits);
    return g;
}

// The exact value of an expansion of gridded components, scaled to an integer.
i128 scaled_value(const ex::Expansion& e) {
    i128 total = 0;
    for (double x : e) {
        int exp2 = 0;
        const double m = std::frexp(x, &exp2);
        const int64_t mant = static_cast<int64_t>(std::ldexp(m, 53));
        const int shift = exp2 - 53 + kGridBits;
        total += shift >= 0 ? (static_cast<i128>(mant) << shift)
                            : (static_cast<i128>(mant) >> (-shift));
    }
    return total;
}

int sign_of(i128 x) { return (x > 0) - (x < 0); }

bool non_overlapping_increasing(const ex::Expansion& e) {
    for (std::size_t k = 0; k < e.size(); ++k) {
        if (e[k] == 0.0) return false;
        if (k > 0 && std::fabs(e[k]) <= std::fabs(e[k - 1])) return false;
    }
    return true;
}

lap::CostMatrix random_distances(int n, int m, uint64_t seed) {
    std::mt19937_64 rng(seed);
    std::normal_distribution<double> z(0.0, 1.0);
    std::vector<std::vector<double>> a(static_cast<std::size_t>(n)), b(static_cast<std::size_t>(m));
    for (auto& p : a) p = {z(rng), z(rng), z(rng)};
    for (auto& p : b) p = {z(rng), z(rng), z(rng)};
    std::vector<std::vector<double>> c(static_cast<std::size_t>(n), std::vector<double>(static_cast<std::size_t>(m)));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            double s = 0.0;
            for (int d = 0; d < 3; ++d) {
                const double t = a[static_cast<std::size_t>(i)][static_cast<std::size_t>(d)] -
                                 b[static_cast<std::size_t>(j)][static_cast<std::size_t>(d)];
                s += t * t;
            }
            c[static_cast<std::size_t>(i)][static_cast<std::size_t>(j)] = std::sqrt(s);
        }
    }
    return lap::CostMatrix(c);
}

}  // namespace

TEST_CASE("an expansion holds a sum no double can", "[exact][expansion]") {
    std::mt19937_64 rng(20260929u);
    for (int trial = 0; trial < 5000; ++trial) {
        ex::Expansion acc;
        ex::Expansion next;
        i128 truth = 0;
        const int terms = 1 + trial % 12;
        for (int t = 0; t < terms; ++t) {
            const Gridded g = draw(rng);
            ex::grow_expansion(acc, g.value, next);
            acc.swap(next);
            truth += g.scaled;
        }
        ex::compress(acc);

        REQUIRE(non_overlapping_increasing(acc));
        REQUIRE(scaled_value(acc) == truth);
        REQUIRE(ex::sign(acc) == sign_of(truth));
    }
}

TEST_CASE("expansion sums and differences stay exact", "[exact][expansion]") {
    std::mt19937_64 rng(7u);
    for (int trial = 0; trial < 3000; ++trial) {
        ex::Expansion e;
        ex::Expansion f;
        ex::Expansion next;
        i128 te = 0;
        i128 tf = 0;
        for (int t = 0; t < 4; ++t) {
            const Gridded g = draw(rng);
            ex::grow_expansion(e, g.value, next);
            e.swap(next);
            te += g.scaled;
            const Gridded h = draw(rng);
            ex::grow_expansion(f, h.value, next);
            f.swap(next);
            tf += h.scaled;
        }
        const Gridded a = draw(rng);
        const Gridded b = draw(rng);

        REQUIRE(scaled_value(ex::expansion_sum(e, f)) == te + tf);
        REQUIRE(scaled_value(ex::expansion_sum(e, ex::negated(f))) == te - tf);
        REQUIRE(scaled_value(ex::add_difference(e, a.value, b.value)) ==
                te + a.scaled - b.scaled);
    }
}

TEST_CASE("the expansion reduced-cost sign agrees with integer arithmetic",
          "[exact][expansion][sign]") {
    std::mt19937_64 rng(11u);
    for (int trial = 0; trial < 5000; ++trial) {
        ex::Expansion u;
        ex::Expansion v;
        ex::Expansion next;
        i128 tu = 0;
        i128 tv = 0;
        for (int t = 0; t < 3; ++t) {
            const Gridded g = draw(rng);
            ex::grow_expansion(u, g.value, next);
            u.swap(next);
            tu += g.scaled;
            const Gridded h = draw(rng);
            ex::grow_expansion(v, h.value, next);
            v.swap(next);
            tv += h.scaled;
        }
        ex::compress(u);
        ex::compress(v);
        // Half the trials put c exactly on u + v, which is the tie the filter
        // cannot settle and the expansion has to.
        const Gridded draw_c = draw(rng);
        const bool tie = (trial % 2 == 0) && ex::expansion_sum(u, v).size() == 1;
        const double c = tie ? ex::expansion_sum(u, v)[0] : draw_c.value;
        const i128 tc = tie ? tu + tv : draw_c.scaled;

        const int got = ex::sign_reduced_cost(c, u, ex::approximate(u), v, ex::approximate(v));
        REQUIRE(got == sign_of(tc - tu - tv));
    }
}

TEST_CASE("recovered potentials certify an optimal matching exactly",
          "[exact][recover]") {
    for (uint64_t seed = 1; seed <= 20; ++seed) {
        const int n = 12 + static_cast<int>(seed % 5);
        const int m = n + static_cast<int>(seed % 3) * 4;
        const lap::CostMatrix cost = random_distances(n, m, seed);
        const lap::DualResult solved = lap::solve_jv_duals(cost, false);

        const ex::AssignmentPotentials pots = ex::recover_assignment_potentials(
            cost, solved.solution.assignment, solved.v);
        REQUIRE(pots.ok);

        const ex::ExpansionCheck check = ex::check_expansion_duals(
            cost, solved.solution.assignment, pots.u, pots.v);
        REQUIRE(check.holds());
    }
}

TEST_CASE("a matching that is not optimal has no exact potentials",
          "[exact][recover]") {
    for (uint64_t seed = 1; seed <= 20; ++seed) {
        const lap::CostMatrix cost = random_distances(10, 14, seed);
        const lap::DualResult solved = lap::solve_jv_duals(cost, false);

        // Swapping two rows' columns keeps a valid matching and, on generic
        // distances, raises its cost: an alternating cycle of negative weight.
        std::vector<int> worse = solved.solution.assignment;
        std::swap(worse[0], worse[1]);
        const ex::AssignmentPotentials pots =
            ex::recover_assignment_potentials(cost, worse, solved.v);
        REQUIRE_FALSE(pots.ok);

        // Moving a row onto a free column it prices worse leaves the free
        // column it gave up at a negative distance: an improving path.
        std::vector<char> used(14, 0);
        for (int j : solved.solution.assignment) used[static_cast<std::size_t>(j)] = 1;
        int spare = 0;
        while (used[static_cast<std::size_t>(spare)]) ++spare;
        std::vector<int> moved = solved.solution.assignment;
        for (int i = 0; i < 10; ++i) {
            if (cost.at(i, spare) > cost.at(i, moved[static_cast<std::size_t>(i)])) {
                moved[static_cast<std::size_t>(i)] = spare;
                break;
            }
        }
        if (moved != solved.solution.assignment) {
            REQUIRE_FALSE(ex::recover_assignment_potentials(cost, moved, solved.v).ok);
        }
    }
}

TEST_CASE("shortest paths report a negative cycle whatever the hint",
          "[exact][paths]") {
    // 0 -> 1 -> 2 -> 0 with total weight -1, reached from the root.
    const auto arcs = [](int64_t k, auto&& emit) {
        if (k == 0) emit(1, ex::DoubleDifference{1.0, 0.0, 10});
        if (k == 1) emit(2, ex::DoubleDifference{1.0, 0.0, 11});
        if (k == 2) emit(0, ex::DoubleDifference{0.0, 3.0, 12});
    };
    REQUIRE_FALSE(ex::shortest_paths(3, {}, arcs).ok);
    REQUIRE_FALSE(ex::shortest_paths(3, {5.0, -2.0, 0.25}, arcs).ok);

    // The same cycle at total weight +1 has distances. Every node is joined to
    // the root at zero, so d(1) = d(2) = 0 and the arc 2 -> 0 of weight -1
    // puts d(0) at -1.
    const auto positive = [](int64_t k, auto&& emit) {
        if (k == 0) emit(1, ex::DoubleDifference{1.0, 0.0});
        if (k == 1) emit(2, ex::DoubleDifference{1.0, 0.0});
        if (k == 2) emit(0, ex::DoubleDifference{0.0, 1.0});
    };
    const ex::ShortestPaths p = ex::shortest_paths(3, {}, positive);
    REQUIRE(p.ok);
    REQUIRE(ex::approximate(p.dist[0]).value == -1.0);
    REQUIRE(ex::sign(p.dist[1]) == 0);
    REQUIRE(ex::sign(p.dist[2]) == 0);
}

TEST_CASE("a search asked for the cycle names its arcs in order",
          "[exact][paths]") {
    const auto arcs = [](int64_t k, auto&& emit) {
        if (k == 0) emit(1, ex::DoubleDifference{1.0, 0.0, 10});
        if (k == 1) emit(2, ex::DoubleDifference{1.0, 0.0, 11});
        if (k == 2) emit(0, ex::DoubleDifference{0.0, 3.0, 12});
    };
    const ex::ShortestPaths p = ex::shortest_paths(3, {}, arcs, true);
    REQUIRE_FALSE(p.ok);
    REQUIRE(p.cycle.size() == 3);
    // Any rotation of 10, 11, 12 runs the cycle in its own order.
    const auto at = std::find(p.cycle.begin(), p.cycle.end(), int64_t{10});
    REQUIRE(at != p.cycle.end());
    const std::size_t s = static_cast<std::size_t>(at - p.cycle.begin());
    REQUIRE(p.cycle[(s + 1) % 3] == 11);
    REQUIRE(p.cycle[(s + 2) % 3] == 12);
}

TEST_CASE("zero-weight arcs between zero labels are no improvement",
          "[exact][paths]") {
    // A zero-weight 2-cycle between every pair of four nodes: optimal
    // distances are all zero, and no negative cycle exists.
    const auto arcs = [](int64_t k, auto&& emit) {
        for (int64_t j = 0; j < 4; ++j) {
            if (j != k) emit(j, ex::DoubleDifference{0.0, 0.0});
        }
    };
    for (bool want : {false, true}) {
        const ex::ShortestPaths p = ex::shortest_paths(4, {}, arcs, want);
        REQUIRE(p.ok);
        REQUIRE(p.cycle.empty());
        for (const auto& d : p.dist) REQUIRE(ex::sign(d) == 0);
    }
}

TEST_CASE("products, directed rounding and ceilings are exact",
          "[exact][arith]") {
    const double a = 1.0 + std::ldexp(1.0, -30);
    const ex::Expansion p = ex::product(a, a);
    REQUIRE(ex::compare(p, a * a) == 1);
    REQUIRE(ex::round_down(p) == a * a);
    REQUIRE(ex::round_up(p) == std::nextafter(a * a, 2.0));

    const ex::Expansion s = ex::scale_expansion(p, 3.0);
    REQUIRE(ex::compare(s, ex::expansion_sum(ex::expansion_sum(p, p), p)) == 0);

    ex::Expansion six{6.0};
    REQUIRE(ex::ceil_quotient(six, 0.0, 3.0) == 2.0);
    REQUIRE(ex::ceil_quotient(six, std::ldexp(1.0, -50), 3.0) == 3.0);
    REQUIRE(ex::ceil_quotient(six, -std::ldexp(1.0, -50), 3.0) == 2.0);
}
