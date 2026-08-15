// Test suite for the shared shape precondition in core/lap_error.h

#include <catch2/catch_test_macros.hpp>

#include <string>

#include "core/lap_error.h"

TEST_CASE("Shape precondition - square and wider problems pass", "[error]") {
    REQUIRE_NOTHROW(lap::require_rows_fit_cols(0, 0));
    REQUIRE_NOTHROW(lap::require_rows_fit_cols(3, 3));
    REQUIRE_NOTHROW(lap::require_rows_fit_cols(2, 5));
}

TEST_CASE("Shape precondition - a taller problem is a dimension error", "[error]") {
    REQUIRE_THROWS_AS(lap::require_rows_fit_cols(3, 2), lap::DimensionException);

    // The type does not cross the Rcpp boundary, which re-raises what() alone,
    // so the message is what tells a caller which condition fired.
    std::string msg;
    try {
        lap::require_rows_fit_cols(3, 2);
    } catch (const lap::LapException& e) {
        msg = e.what();
    }
    REQUIRE(msg == "solver requires nrow <= ncol; got 3 rows and 2 columns");
}
