// cpp_tests/tests/test_lap_exact.cpp
// The exact sign of c - u - v, against arithmetic that cannot round.
//
// Values on a grid of 2^-20 are exactly representable, and scaling them by
// 2^20 turns the question into one about integers, which settles it
// independently of the predicate under test. The cases where the double
// evaluation is not merely imprecise but wrong are checked separately, since
// those are the ones the predicate exists for.

#include <catch2/catch_test_macros.hpp>

#include "core/lap_exact.h"

#include <cmath>
#include <cstdint>
#include <random>

using lap::exact::sign_reduced_cost;
using lap::exact::two_sum;

namespace {

constexpr int kGridBits = 20;

double on_grid(int64_t k) {
    return std::ldexp(static_cast<double>(k), -kGridBits);
}

int sign_of(int64_t x) { return (x > 0) - (x < 0); }

}  // namespace

TEST_CASE("two_sum splits an addition without losing anything", "[exact][two_sum]") {
    SECTION("the error term is what rounding dropped") {
        // 1 + 2^-60 rounds to 1, and the dropped bit is the error term.
        const double a = 1.0;
        const double b = std::ldexp(1.0, -60);
        double err = 0.0;
        const double s = two_sum(a, b, err);

        REQUIRE(s == 1.0);
        REQUIRE(err == b);
    }

    SECTION("an exact addition leaves no error") {
        double err = 1.0;
        const double s = two_sum(0.5, 0.25, err);

        REQUIRE(s == 0.75);
        REQUIRE(err == 0.0);
    }
}

TEST_CASE("the exact sign agrees with integer arithmetic on a dyadic grid",
          "[exact][sign]") {
    std::mt19937_64 rng(20260825u);
    std::uniform_int_distribution<int64_t> pick(-1000000, 1000000);

    for (int trial = 0; trial < 20000; ++trial) {
        const int64_t kc = pick(rng);
        const int64_t ku = pick(rng);
        const int64_t kv = pick(rng);

        const int expected = sign_of(kc - ku - kv);
        const int got = sign_reduced_cost(on_grid(kc), on_grid(ku), on_grid(kv));

        REQUIRE(got == expected);
    }
}

TEST_CASE("ties on the grid are reported as ties", "[exact][sign]") {
    std::mt19937_64 rng(11u);
    std::uniform_int_distribution<int64_t> pick(-1000000, 1000000);

    for (int trial = 0; trial < 5000; ++trial) {
        const int64_t ku = pick(rng);
        const int64_t kv = pick(rng);
        // A cost placed exactly at u + v, which is what a tight matched arc is.
        REQUIRE(sign_reduced_cost(on_grid(ku + kv), on_grid(ku), on_grid(kv)) == 0);
    }
}

TEST_CASE("the exact sign disagrees with the double evaluation where it must",
          "[exact][sign]") {
    SECTION("a positive difference the double evaluation reads as zero") {
        const double c = std::ldexp(1.0, -60);
        const double u = 1.0;
        const double v = -1.0;

        // Both roundings land on values that cancel, so the naive expression
        // returns zero on a pair whose reduced cost is positive.
        REQUIRE(((c - u) - v) == 0.0);
        REQUIRE(sign_reduced_cost(c, u, v) == 1);
    }

    SECTION("a negative difference the double evaluation reads as zero") {
        const double c = -std::ldexp(1.0, -60);
        const double u = 1.0;
        const double v = -1.0;

        REQUIRE(((c - u) - v) == 0.0);
        REQUIRE(sign_reduced_cost(c, u, v) == -1);
    }

    SECTION("a cost placed at the rounded sum of its duals is not tight") {
        // fl(0.1 + 0.2) is not 0.1 + 0.2, so this arc is tight by
        // construction in double arithmetic and is not tight in the reals.
        // What separates the two is the rounding of that addition, and the
        // predicate reports the difference rather than the intent.
        const double u = 0.1;
        const double v = 0.2;
        const double c = u + v;

        REQUIRE(sign_reduced_cost(c, u, v) == 1);
        REQUIRE(sign_reduced_cost(u + v, u, v) == sign_reduced_cost(c, u, v));
    }

    SECTION("an arc tight in the reals is tight to the double evaluation too") {
        // When c is exactly u + v, each subtraction has a representable exact
        // result, so the naive expression cannot go wrong. The tolerance a
        // verifier would otherwise need is not for these arcs; it is for the
        // ones whose duals are optimal only up to the rounding of the
        // arithmetic that produced them.
        const double u = 0.5;
        const double v = std::ldexp(1.0, -53);
        const double c = u + v;

        REQUIRE(((c - u) - v) == 0.0);
        REQUIRE(sign_reduced_cost(c, u, v) == 0);
    }
}

TEST_CASE("the exact sign holds across magnitudes", "[exact][sign]") {
    std::mt19937_64 rng(7u);
    std::uniform_int_distribution<int64_t> pick(-1000000, 1000000);
    std::uniform_int_distribution<int> shift(-40, 40);

    for (int trial = 0; trial < 20000; ++trial) {
        const int64_t kc = pick(rng);
        const int64_t ku = pick(rng);
        const int64_t kv = pick(rng);
        // One common power of two scales all three, so the sign is unchanged
        // and the integer comparison still decides it.
        const int e = shift(rng);

        const int expected = sign_of(kc - ku - kv);
        const int got = sign_reduced_cost(std::ldexp(on_grid(kc), e),
                                          std::ldexp(on_grid(ku), e),
                                          std::ldexp(on_grid(kv), e));

        REQUIRE(got == expected);
    }
}
