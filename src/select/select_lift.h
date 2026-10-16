// src/select/select_lift.h
// The strongest distance cut a pinned solve's potentials admit.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// A pinned solve of selection S returns per-unit values U and V with
//
//     d_ij - U_i - V_j >= 0   on every admissible pair,
//
// tight on the pairs it matched, so that dist(S') >= sum_{S'} U + sum_{S'} V
// for every selection S' with a perfect matching, with equality at S. The
// values of the units outside S are whatever the solve's potentials happened
// to be there, which is valid and usually far below what validity allows:
// such a unit is barred from the solve, so nothing in it pushes its potential
// up, and a cut that charges it little lets the search swap it in for free.
//
// Raising an outside unit's value as far as validity allows is a pass over
// its pairs. A left unit i outside S takes
//
//     U_i = min_j (d_ij - V_j)
//
// over every admissible j, and then a right unit j outside S takes
//
//     V_j = min_i (d_ij - U_i)
//
// over every admissible i, the left values already raised. Every pair keeps
// d_ij - U_i - V_j >= 0: an outside left unit's pairs by its own minimum, an
// outside right unit's by its, and a pair of two inside units is untouched.
// The units inside S keep their values, so the cut stays tight at S.
//
// The new values are doubles rounded toward minus infinity past the rounding
// of the subtraction and the error of the potentials they are read against,
// so each is at most the exact minimum and the cut stays valid exactly. A unit
// with no admissible pair keeps its value: no pair constrains it.
#pragma once

#include "../core/lap_cost_source.h"
#include "../core/lap_exact.h"

#include <cfloat>
#include <cmath>
#include <cstdint>
#include <limits>
#include <vector>

namespace lap {
namespace select {

// Lower bound on the exact value of a - V, given V's rounding and its error.
inline double difference_down(double a, const exact::Approximation& v) {
    const double t = a - v.value;
    const double slack = 2.0 * DBL_EPSILON * std::fabs(t) + v.error + 4.9e-324;
    return std::nextafter(t - slack, -std::numeric_limits<double>::infinity());
}

template <class Source>
void lift_outside(const Source& src, double shift, const std::vector<uint8_t>& in_left,
                  const std::vector<uint8_t>& in_right, std::vector<exact::Expansion>& U,
                  std::vector<exact::Expansion>& V) {
    const int64_t nl = src.nrow;
    const int64_t nr = src.ncol;
    const double inf = std::numeric_limits<double>::infinity();

    std::vector<exact::Approximation> va(static_cast<std::size_t>(nr));
    for (int64_t j = 0; j < nr; ++j) va[static_cast<std::size_t>(j)] = exact::approximate(V[static_cast<std::size_t>(j)]);
    for (int64_t i = 0; i < nl; ++i) {
        if (in_left[static_cast<std::size_t>(i)]) continue;
        double best = inf;
        for (int64_t j = 0; j < nr; ++j) {
            double c = 0.0;
            if (!cost_if_allowed(src, i, j, c)) continue;
            const double base = c - shift;
            best = std::min(best, difference_down(base, va[static_cast<std::size_t>(j)]));
        }
        if (best < inf) U[static_cast<std::size_t>(i)] = best == 0.0 ? exact::Expansion() : exact::Expansion{best};
    }

    std::vector<exact::Approximation> ua(static_cast<std::size_t>(nl));
    for (int64_t i = 0; i < nl; ++i) ua[static_cast<std::size_t>(i)] = exact::approximate(U[static_cast<std::size_t>(i)]);
    for (int64_t j = 0; j < nr; ++j) {
        if (in_right[static_cast<std::size_t>(j)]) continue;
        double best = inf;
        for (int64_t i = 0; i < nl; ++i) {
            double c = 0.0;
            if (!cost_if_allowed(src, i, j, c)) continue;
            const double base = c - shift;
            best = std::min(best, difference_down(base, ua[static_cast<std::size_t>(i)]));
        }
        if (best < inf) V[static_cast<std::size_t>(j)] = best == 0.0 ? exact::Expansion() : exact::Expansion{best};
    }
}

}  // namespace select
}  // namespace lap
