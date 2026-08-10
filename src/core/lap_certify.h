// src/core/lap_certify.h
// Optimality certificate for the rectangular linear assignment problem.
// Pure C++ types and templates - NO Rcpp dependencies (same rule as
// lap_types.h), so the checks run from the C++ test harness as well as from
// the Rcpp wrappers.
//
// Everything here is a minimization. The LP checked is
//
//     min  sum_ij c_ij x_ij
//     s.t. sum_j x_ij  = 1   for every row i
//          sum_i x_ij <= 1   for every column j
//          x >= 0
//
// whose dual is
//
//     max  sum_i u_i + sum_j v_j
//     s.t. u_i + v_j <= c_ij   for every admissible pair (i, j)
//          v_j <= 0            when ncol > nrow
//
// The sign condition is exactly as conditional as that line says. For any
// feasible assignment M', cost(M') >= sum_i u_i + sum_{j used by M'} v_j, and
// the dual objective sums v over ALL columns, so the bound holds only if the
// columns M' leaves out contribute nothing positive. When ncol > nrow every
// column is one M' could leave out, which forces v_j <= 0 throughout. When
// ncol == nrow every feasible assignment uses every column, the two sums are
// the same sum, and v is unrestricted in sign -- the column constraint is an
// equality there, not an inequality. Jonker-Volgenant returns duals of exactly
// that shape (see the rectangular warm-start gate in jv_core.cpp:55-71), so
// imposing v_j <= 0 on a square problem rejects correct duals.
//
// A maximization instance is certified by negating the costs and both dual
// vectors and running exactly these conditions; that sign change belongs to
// the caller, which is why no template here takes a `maximize` flag.
//
// The templates take any cost source exposing `at(int64_t, int64_t) const`,
// `allowed(int64_t, int64_t) const`, and the public data members `nrow` and
// `ncol`: lap::CostMatrix, lap::LazyCostMatrix, lap::PaddedCostView<Base>.
// Forbidden cells read as lap::BIG (1e100), not as Inf or NA, so every scan
// gates on allowed(i, j) before touching at(i, j) -- a single unguarded BIG
// poisons a minimum or a sum beyond recovery.
#pragma once

#include <vector>
#include <limits>
#include <cmath>
#include <cstdint>
#include <algorithm>
#include <cstddef>

namespace lap {

namespace detail {

// Neumaier compensated summation: `comp` accumulates the low-order bits lost
// at each addition, and is folded back in once at the end. The certificate's
// whole purpose is that a discrepancy of a fraction of a percent between the
// primal and dual objectives is visible, and naive summation over 10^5 terms
// loses digits at exactly that scale. Neumaier's variant, unlike plain Kahan,
// is also correct when the running sum is smaller in magnitude than the
// incoming term, which happens whenever costs straddle zero.
inline void compensated_add(double& sum, double& comp, double x) {
    const double t = sum + x;
    if (std::abs(sum) >= std::abs(x)) {
        comp += (sum - t) + x;
    } else {
        comp += (x - t) + sum;
    }
    sum = t;
}

struct CompensatedSum {
    double sum = 0.0;
    double comp = 0.0;

    void add(double x) { compensated_add(sum, comp, x); }
    double value() const { return sum + comp; }
};

}  // namespace detail

// Result of a full scan of the reduced costs cbar_ij = c_ij - u_i - v_j over
// admissible pairs. Certification asks whether the minimum is >= -tol; a
// pricing loop asks for the argmin and the violation count. One scan, both
// questions.
struct ReducedCostScan {
    double  min_reduced_cost = std::numeric_limits<double>::infinity();
    int64_t arg_i = -1;             // 0-based argmin, -1 when no admissible pair exists
    int64_t arg_j = -1;
    int64_t n_violations = 0;       // admissible pairs with cbar < -tol
    int64_t n_admissible = 0;       // admissible pairs scanned
};

// Full certificate for a candidate matching and a candidate pair of dual
// vectors. The duals are an input to a check, never an answer: garbage duals
// fail, so accepting duals from the solver that produced the matching is
// sound.
struct CertificateReport {
    // primal
    bool    primal_feasible = false;
    int64_t n_rows = 0;
    int64_t n_cols = 0;
    int64_t n_matched = 0;
    int64_t n_duplicate_cols = 0;    // columns claimed by more than one row
    int64_t n_forbidden_matched = 0; // matched pairs where allowed(i,j) is false
    int64_t n_out_of_range = 0;      // match entries outside [-1, ncol-1]
    double  primal_objective = std::numeric_limits<double>::quiet_NaN();

    // dual
    bool    dual_feasible = false;
    double  min_reduced_cost = std::numeric_limits<double>::infinity();
    int64_t worst_i = -1;
    int64_t worst_j = -1;
    double  max_v = std::numeric_limits<double>::quiet_NaN();
    double  dual_objective = std::numeric_limits<double>::quiet_NaN();

    // complementary slackness
    bool    cs_matched_tight = false;
    double  max_matched_slack = std::numeric_limits<double>::quiet_NaN();
    bool    cs_unmatched_free = false;
    double  max_v_unmatched = std::numeric_limits<double>::quiet_NaN();
    bool    complementary_slackness = false;

    // conclusion
    double  duality_gap = std::numeric_limits<double>::quiet_NaN();
    bool    certified_optimal = false;
    double  tolerance = 0.0;
};

// min over admissible (i, j) of c_ij - u_i - v_j, with its argmin and the
// number of pairs violating dual feasibility by more than tol.
//
// Returns the empty scan (infinite minimum, argmin -1) on an empty problem or
// on a dual vector whose length does not match the source, rather than
// reading out of bounds.
template <class Source>
ReducedCostScan scan_reduced_costs(const Source& src,
                                   const std::vector<double>& u,
                                   const std::vector<double>& v,
                                   double tol) {
    ReducedCostScan out;

    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;
    if (nrow <= 0 || ncol <= 0) return out;
    if (static_cast<int64_t>(u.size()) != nrow) return out;
    if (static_cast<int64_t>(v.size()) != ncol) return out;

    for (int64_t i = 0; i < nrow; ++i) {
        const double ui = u[static_cast<std::size_t>(i)];
        for (int64_t j = 0; j < ncol; ++j) {
            // Forbidden arcs carry BIG, not Inf: reading one without this
            // guard drags the minimum to +1e100 and the argmin with it.
            if (!src.allowed(i, j)) continue;
            const double cbar = src.at(i, j) - ui - v[static_cast<std::size_t>(j)];
            ++out.n_admissible;
            if (cbar < out.min_reduced_cost) {
                out.min_reduced_cost = cbar;
                out.arg_i = i;
                out.arg_j = j;
            }
            if (cbar < -tol) ++out.n_violations;
        }
    }

    return out;
}

// Certify `match` (0-based column per row, -1 unmatched, length src.nrow)
// against the duals `u` (length src.nrow) and `v` (length src.ncol).
//
// Four groups of conditions:
//   1. primal feasibility  - every column claimed at most once, every matched
//      pair admissible, every entry in range;
//   2. dual feasibility    - min_ij cbar_ij >= -tol over admissible pairs, and
//      max_j v_j <= tol;
//   3. complementary slackness, both halves - matched arcs tight, and
//      |v_j| <= tol on every column no row matched;
//   4. objective equality  - |primal - dual| within a magnitude-scaled
//      tolerance.
//
// Condition 3's second half is not redundant. A matching can be perfect,
// dual-feasible everywhere, tight on every matched arc, and carry a dual
// bound equal to the true optimum while its primal cost sits above the
// optimum, when a column is left unmatched holding v_j < 0. Checking only
// dual feasibility and matched-arc tightness certifies that wrong answer.
//
// Returns a report with primal_feasible = false on an empty problem or on any
// length mismatch, rather than reading out of bounds.
template <class Source>
CertificateReport certify_assignment(const Source& src,
                                     const std::vector<int>& match,
                                     const std::vector<double>& u,
                                     const std::vector<double>& v,
                                     double tol) {
    CertificateReport rep;
    rep.tolerance = tol;

    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;
    rep.n_rows = nrow;
    rep.n_cols = ncol;

    if (nrow <= 0 || ncol <= 0) return rep;
    if (static_cast<int64_t>(match.size()) != nrow) return rep;
    if (static_cast<int64_t>(u.size()) != nrow) return rep;
    if (static_cast<int64_t>(v.size()) != ncol) return rep;

    // ---- primal feasibility ----
    std::vector<int64_t> col_claims(static_cast<std::size_t>(ncol), 0);
    for (int64_t i = 0; i < nrow; ++i) {
        const int64_t j = static_cast<int64_t>(match[static_cast<std::size_t>(i)]);
        if (j < -1 || j >= ncol) {
            ++rep.n_out_of_range;
            continue;
        }
        if (j < 0) continue;  // row deliberately unmatched
        ++rep.n_matched;
        ++col_claims[static_cast<std::size_t>(j)];
        if (!src.allowed(i, j)) ++rep.n_forbidden_matched;
    }
    for (int64_t j = 0; j < ncol; ++j) {
        if (col_claims[static_cast<std::size_t>(j)] > 1) ++rep.n_duplicate_cols;
    }
    rep.primal_feasible = (rep.n_out_of_range == 0) &&
                          (rep.n_duplicate_cols == 0) &&
                          (rep.n_forbidden_matched == 0);

    // The primal objective is only meaningful once the matching is a matching:
    // summing over a duplicated column or a forbidden arc produces a number
    // that invites comparison with the dual bound but does not correspond to
    // any feasible solution.
    if (rep.primal_feasible) {
        detail::CompensatedSum primal;
        for (int64_t i = 0; i < nrow; ++i) {
            const int64_t j = static_cast<int64_t>(match[static_cast<std::size_t>(i)]);
            if (j < 0) continue;
            primal.add(src.at(i, j));
        }
        rep.primal_objective = primal.value();
    }

    // ---- dual feasibility ----
    const ReducedCostScan scan = scan_reduced_costs(src, u, v, tol);
    rep.min_reduced_cost = scan.min_reduced_cost;
    rep.worst_i = scan.arg_i;
    rep.worst_j = scan.arg_j;

    double max_v = -std::numeric_limits<double>::infinity();
    for (int64_t j = 0; j < ncol; ++j) {
        max_v = std::max(max_v, v[static_cast<std::size_t>(j)]);
    }
    rep.max_v = max_v;
    // The sign condition applies only where a column can go unmatched; see the
    // derivation at the top of this file.
    const bool sign_condition_applies = (ncol > nrow);
    rep.dual_feasible = (rep.min_reduced_cost >= -tol) &&
                        (!sign_condition_applies || rep.max_v <= tol);

    // Summed over ALL rows and ALL columns, unmatched columns included. That
    // is what turns a nonzero v_j on a freed column into a visible duality
    // gap instead of a term quietly dropped from the bound.
    detail::CompensatedSum dual;
    for (int64_t i = 0; i < nrow; ++i) dual.add(u[static_cast<std::size_t>(i)]);
    for (int64_t j = 0; j < ncol; ++j) dual.add(v[static_cast<std::size_t>(j)]);
    rep.dual_objective = dual.value();

    // ---- complementary slackness ----
    double max_matched_slack = 0.0;
    for (int64_t i = 0; i < nrow; ++i) {
        const int64_t j = static_cast<int64_t>(match[static_cast<std::size_t>(i)]);
        if (j < 0 || j >= ncol) continue;
        // A forbidden matched arc is already counted in n_forbidden_matched
        // and has made the primal infeasible; its at() is BIG, so folding it
        // into the slack would report 1e100 instead of the real worst arc.
        if (!src.allowed(i, j)) continue;
        const double slack = std::abs(src.at(i, j) -
                                      u[static_cast<std::size_t>(i)] -
                                      v[static_cast<std::size_t>(j)]);
        if (slack > max_matched_slack) max_matched_slack = slack;
    }
    rep.max_matched_slack = max_matched_slack;
    rep.cs_matched_tight = (max_matched_slack <= tol);

    double max_v_unmatched = 0.0;
    for (int64_t j = 0; j < ncol; ++j) {
        if (col_claims[static_cast<std::size_t>(j)] != 0) continue;
        const double a = std::abs(v[static_cast<std::size_t>(j)]);
        if (a > max_v_unmatched) max_v_unmatched = a;
    }
    rep.max_v_unmatched = max_v_unmatched;
    rep.cs_unmatched_free = (max_v_unmatched <= tol);

    rep.complementary_slackness = rep.cs_matched_tight && rep.cs_unmatched_free;

    // ---- conclusion ----
    rep.duality_gap = rep.primal_objective - rep.dual_objective;

    // The gap tolerance scales with the magnitude of the objective. Both sums
    // carry a relative rounding error of order eps per term, so an absolute
    // tol on a sum of 5*10^4 terms is below the representable resolution of
    // the sum itself and no correct solution could ever meet it. Compensated
    // summation buys back the accumulation error, not the fact that the
    // objective's own last bits are worth |objective| * eps.
    const double tol_gap = tol * std::max(1.0, std::abs(rep.primal_objective));
    rep.certified_optimal = rep.primal_feasible &&
                            rep.dual_feasible &&
                            rep.complementary_slackness &&
                            (std::abs(rep.duality_gap) <= tol_gap);

    return rep;
}

}  // namespace lap
