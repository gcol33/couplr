// src/core/lap_exact.h
// Exact sign of c - u - v over IEEE doubles, with no tolerance anywhere.
// Pure C++, no Rcpp, so the C++ test harness reaches it as well as the
// certificate does.
//
// Every condition the optimality certificate checks reduces to one question
// asked of three numbers: is c_ij - u_i - v_j positive, zero, or negative. A
// double is a rational number, p / 2^k with p and k integers, so that question
// has an exact answer. A verifier deciding it within a band around zero is
// making a statement about a neighbourhood of the instance rather than about
// the instance: at tolerance eps it accepts potentials that violate dual
// feasibility by eps on any pair and miss tightness by eps on any matched arc,
// which bounds the matching's excess over the optimum by about
// 2 (nrow + ncol) eps rather than pinning it at zero.
//
// Setting the band to zero and reading the sign of the double evaluation is
// not the fix, because that sign can be wrong. With c = 2^-60, u = 1 and
// v = -1 the first subtraction rounds to -1 and the expression returns 0 on a
// pair whose reduced cost is positive; at c = -2^-60 it returns 0 on a pair
// whose reduced cost is negative, which is a dual infeasibility a zero-band
// check would accept.
//
// The band goes away once the expression is evaluated exactly. Knuth's two-sum
// splits a rounded addition into the sum plus the part that rounding lost, both
// doubles, and both together equal to the true value. Applying it twice writes
// c - u - v as a non-overlapping expansion of three doubles whose sum is the
// exact difference; the sign of that sum is the sign of its most significant
// non-zero component. The technique and the non-overlap property are
// Shewchuk's (1997, "Adaptive Precision Floating-Point Arithmetic and Fast
// Robust Geometric Predicates").
//
// The expansion costs about ten flops against two, so it runs behind a filter:
// a rounding-error bound on the double evaluation decides the pairs that are
// clear of zero, and only pairs inside the bound are expanded. On a dense scan
// almost every pair is clear, and the pairs that are not are the tight ones,
// which are the ones worth the arithmetic.
#pragma once

#include <cfloat>
#include <cmath>

namespace lap {
namespace exact {

// Knuth's two-sum. Returns fl(a + b) and writes into `err` the part the
// rounding lost, so that a + b == result + err holds exactly in the reals. No
// assumption about the relative magnitude of a and b, and no branch. Exact
// whenever the addition does not overflow.
inline double two_sum(double a, double b, double& err) {
    const double s = a + b;
    const double b_virtual = s - a;
    err = (a - (s - b_virtual)) + (b - b_virtual);
    return s;
}

// Sign of c - u - v, exactly: -1, 0 or +1.
//
// The filter is a bound on the error of the two roundings in fl(fl(c - u) - v).
// Each is relative to the magnitude of its own operands, so their sum is
// bounded by a small multiple of the unit roundoff times |c| + |u| + |v|; the
// multiple below is generous, which costs a few expansions on pairs that did
// not need one and never returns a sign the expansion would disagree with.
//
// Values large enough to overflow the double evaluation take the sign of the
// overflowed sum, which is the sign of the dominant term and is the answer the
// expansion would reach if it could represent it.
inline int sign_reduced_cost(double c, double u, double v) {
    const double approx = (c - u) - v;
    const double magnitude = std::fabs(c) + std::fabs(u) + std::fabs(v);
    const double bound = 4.0 * DBL_EPSILON * magnitude;

    if (approx > bound) return 1;
    if (approx < -bound) return -1;
    if (!std::isfinite(approx)) return approx > 0.0 ? 1 : -1;

    // Grow the one-component expansion [c] by -u, then the two-component
    // result by -v. Shewchuk's grow_expansion: the running sum absorbs each
    // term and the piece rounding lost is set aside as a component of lower
    // magnitude. What comes out is non-overlapping and ordered, so the first
    // non-zero component from the top carries the sign of the whole.
    double low_u = 0.0;
    const double high_u = two_sum(-u, c, low_u);

    double low_a = 0.0;
    double running = two_sum(-v, low_u, low_a);
    double low_b = 0.0;
    running = two_sum(running, high_u, low_b);

    if (running != 0.0) return running > 0.0 ? 1 : -1;
    if (low_b != 0.0) return low_b > 0.0 ? 1 : -1;
    if (low_a != 0.0) return low_a > 0.0 ? 1 : -1;
    return 0;
}

}  // namespace exact
}  // namespace lap
