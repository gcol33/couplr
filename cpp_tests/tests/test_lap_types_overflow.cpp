// Regression test for the lap::CostMatrix flat-index int overflow bug.
//
// Prior to the fix, CostMatrix::at()/allowed()/forbid() computed the flat
// row-major index as `i * ncol + j` in plain `int` arithmetic, which silently
// wraps (undefined behavior) once nrow*ncol exceeds INT_MAX (~46,341 square).
// This test exercises lap::flat_index() directly at boundary values without
// allocating an overflow-sized matrix (46,341^2 doubles alone is ~17GB).

#include <catch2/catch_test_macros.hpp>

#include "core/lap_types.h"

#include <cstdint>
#include <limits>

TEST_CASE("flat_index computes past INT_MAX without wrapping", "[lap_types][overflow]") {
    SECTION("classic 32-bit overflow point: 50000 x 50000") {
        // 50000 * 50000 = 2.5e9, which exceeds INT_MAX (~2.147e9). Under plain
        // `int` arithmetic this wraps to a negative/wrong value; in int64_t it
        // must be exact.
        const int64_t ncol = 50000;
        const int64_t i = 49999;
        const int64_t j = 1;
        const int64_t expected = i * ncol + j;  // computed in int64_t here too,
                                                 // but this file is the one
                                                 // place that's intentional.
        REQUIRE(lap::flat_index(i, j, ncol) == expected);
        REQUIRE(expected > static_cast<int64_t>(std::numeric_limits<int32_t>::max()));
    }

    SECTION("does not match the wrapped 32-bit value") {
        const int64_t ncol = 50000;
        const int64_t i = 49999;
        const int64_t j = 1;

        // Simulate the old buggy computation using unsigned 32-bit arithmetic,
        // which wraps modulo 2^32 by well-defined rules (unlike signed
        // overflow, which is UB) -- this reproduces what the plain `int`
        // computation did on the common two's-complement platforms this
        // package ships on, without invoking UB in the test itself.
        const uint32_t ncol32 = static_cast<uint32_t>(ncol);
        const uint32_t i32 = static_cast<uint32_t>(i);
        const uint32_t j32 = static_cast<uint32_t>(j);
        const uint32_t wrapped = i32 * ncol32 + j32;

        const int64_t correct = lap::flat_index(i, j, ncol);
        REQUIRE(static_cast<int64_t>(static_cast<int32_t>(wrapped)) != correct);
    }

    SECTION("small in-range matrices still compute the familiar formula") {
        REQUIRE(lap::flat_index(0, 0, 10) == 0);
        REQUIRE(lap::flat_index(1, 0, 10) == 10);
        REQUIRE(lap::flat_index(2, 3, 10) == 23);
        REQUIRE(lap::flat_index(9, 9, 10) == 99);
    }
}

TEST_CASE("CostMatrix at/allowed/forbid stay consistent with flat_index", "[lap_types][overflow]") {
    // A small, ordinary matrix -- this exercises the accessor methods
    // end-to-end (not just the free function) without allocating anything
    // overflow-scale.
    lap::CostMatrix cm(4, 5);

    for (int64_t i = 0; i < cm.nrow; ++i) {
        for (int64_t j = 0; j < cm.ncol; ++j) {
            cm.at(i, j) = static_cast<double>(lap::flat_index(i, j, cm.ncol));
        }
    }

    for (int64_t i = 0; i < cm.nrow; ++i) {
        for (int64_t j = 0; j < cm.ncol; ++j) {
            REQUIRE(cm.at(i, j) == static_cast<double>(lap::flat_index(i, j, cm.ncol)));
            REQUIRE(cm.allowed(i, j));
        }
    }

    cm.forbid(1, 2);
    REQUIRE_FALSE(cm.allowed(1, 2));
    REQUIRE(cm.at(1, 2) == lap::BIG);
}
