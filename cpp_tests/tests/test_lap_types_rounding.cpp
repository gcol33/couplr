// The outward rounding steps every pruning bound is widened by. They replace
// std::nextafter towards +/-inf and must return the same bits for every double,
// so each is compared bit for bit against the library over the special values
// and over random bit patterns.

#include <catch2/catch_test_macros.hpp>

#include "core/lap_types.h"

#include <cmath>
#include <cstdint>
#include <cstring>
#include <limits>
#include <random>
#include <vector>

namespace {

std::uint64_t bits_of(double x) {
    std::uint64_t b;
    std::memcpy(&b, &x, sizeof b);
    return b;
}

double from_bits(std::uint64_t b) {
    double x;
    std::memcpy(&x, &b, sizeof x);
    return x;
}

void require_same_as_library(double x) {
    const double inf = std::numeric_limits<double>::infinity();
    const double up = std::nextafter(x, inf);
    const double down = std::nextafter(x, -inf);
    if (std::isnan(x)) {
        REQUIRE(std::isnan(lap::detail::next_up(x)));
        REQUIRE(std::isnan(lap::detail::next_down(x)));
        return;
    }
    INFO("x bits " << bits_of(x));
    REQUIRE(bits_of(lap::detail::next_up(x)) == bits_of(up));
    REQUIRE(bits_of(lap::detail::next_down(x)) == bits_of(down));
}

}  // namespace

TEST_CASE("next_up and next_down match std::nextafter on the special values",
          "[lap_types][rounding]") {
    using L = std::numeric_limits<double>;
    const std::vector<double> specials = {
        0.0, -0.0, L::denorm_min(), -L::denorm_min(), L::min(), -L::min(),
        L::max(), -L::max(), L::infinity(), -L::infinity(), L::quiet_NaN(),
        1.0, -1.0, 2.0, -2.0, 0.5, -0.5, L::epsilon(), -L::epsilon(),
        from_bits(0x000FFFFFFFFFFFFFull), from_bits(0x800FFFFFFFFFFFFFull),
        from_bits(0x0010000000000000ull), from_bits(0x8010000000000000ull),
        std::nextafter(1.0, 0.0), std::nextafter(-1.0, 0.0)};
    for (double x : specials) require_same_as_library(x);
}

TEST_CASE("next_up and next_down match std::nextafter on random doubles",
          "[lap_types][rounding]") {
    std::mt19937_64 rng(20260913u);
    for (int t = 0; t < 1000000; ++t) require_same_as_library(from_bits(rng()));
}
