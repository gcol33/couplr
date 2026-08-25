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

#include "lap_cost_source.h"
#include "lap_exact.h"
#include "lap_neighbours.h"

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

    // Set by a scan asked for the exact sign of every pair it visited, which
    // is what an exact certificate needs and what a pricing loop does not pay
    // for. A scan that was not asked leaves `exact_checked` false, and a
    // certificate built on it reports its conclusion in double arithmetic.
    bool    exact_checked = false;
    int64_t n_exact_violations = 0; // admissible pairs with c - u - v < 0 exactly
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

    // exact arithmetic
    //
    // The same conditions decided with no tolerance at all. `all_rows_matched`
    // stands where objective equality stands in the numerical check: given
    // dual feasibility, tight matched arcs and free unmatched columns, the two
    // objectives are equal exactly when every row is matched, so the equality
    // is a consequence rather than a fourth thing to measure.
    bool    all_rows_matched = false;
    bool    exact_dual_feasible = false;
    bool    exact_cs_matched_tight = false;
    bool    exact_cs_unmatched_free = false;
    bool    exact_certificate = false;
    bool    exact_available = false;  // every condition was asked exactly
    int64_t n_exact_violations = 0;   // admissible pairs with c - u - v < 0
    int64_t n_exact_untight = 0;      // matched pairs with c - u - v != 0

    // conclusion
    double  duality_gap = std::numeric_limits<double>::quiet_NaN();
    bool    certified_optimal = false;
    bool    conclusion_is_exact = false;  // which arithmetic the conclusion is in
    double  tolerance = 0.0;
};

// Which arithmetic the conclusion is taken in.
//
// Auto reports the exact conclusion when the exact conditions hold and the
// numerical one otherwise, which is the strongest statement the instance
// supports: the exact conditions imply the numerical ones at any non-negative
// tolerance, so Auto never certifies anything Double would refuse.
//
// Exact refuses to fall back, and is the mode to ask for when the point is the
// strength of the proof rather than the answer.
enum class Arithmetic { Auto, Exact, Double };

// min over admissible (i, j) of c_ij - u_i - v_j, with its argmin and the
// number of pairs violating dual feasibility by more than tol.
//
// With `exact` set the scan also counts the pairs whose reduced cost is
// negative in exact arithmetic, which is the count an exact certificate reads
// and is a different question from the count against tol: a pair at -1e-17
// violates exactly and does not violate at 1e-9, and a pair the double
// evaluation puts at 0 may be negative. The reported minimum stays the double
// evaluation, which is what a reader wants to see and what a pricing loop
// compares against; the exact count is what the conclusion rests on.
//
// Returns the empty scan (infinite minimum, argmin -1) on an empty problem or
// on a dual vector whose length does not match the source, rather than
// reading out of bounds.
template <class Source>
ReducedCostScan scan_reduced_costs(const Source& src,
                                   const std::vector<double>& u,
                                   const std::vector<double>& v,
                                   double tol,
                                   bool exact = false) {
    ReducedCostScan out;
    out.exact_checked = exact;

    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;
    if (nrow <= 0 || ncol <= 0) return out;
    if (static_cast<int64_t>(u.size()) != nrow) return out;
    if (static_cast<int64_t>(v.size()) != ncol) return out;

    // Forbidden arcs carry BIG, not Inf: reading one unguarded drags the
    // minimum to +1e100 and the argmin with it. for_each_admissible() is that
    // guard and the cost read as one question, so a source that computes its
    // costs answers it once, and a source naming where its pairs are is walked
    // over them rather than over its grid.
    for (int64_t i = 0; i < nrow; ++i) {
        const double ui = u[static_cast<std::size_t>(i)];
        for_each_admissible(src, i, [&](int64_t j, double c) {
            const double cbar = c - ui - v[static_cast<std::size_t>(j)];
            ++out.n_admissible;
            if (cbar < out.min_reduced_cost) {
                out.min_reduced_cost = cbar;
                out.arg_i = i;
                out.arg_j = j;
            }
            if (cbar < -tol) ++out.n_violations;
            if (exact &&
                exact::sign_reduced_cost(c, ui, v[static_cast<std::size_t>(j)]) < 0) {
                ++out.n_exact_violations;
            }
            return true;
        });
    }

    return out;
}

// Two scans over disjoint sets of pairs, read as one scan over their union.
//
// This is what lets a scan be assembled from the passes that can afford it
// rather than from one sweep of the grid: an edge-generation loop knows the
// minimum over the pairs its master holds and the minimum over the pairs it
// omits, and their union is every admissible pair of the complete problem.
//
// Ties keep the earlier argmin in (i, j) order, which is the one a single
// ascending scan of the same pairs would have reported.
inline ReducedCostScan merge_scans(const ReducedCostScan& a, const ReducedCostScan& b) {
    ReducedCostScan out;
    out.n_admissible = a.n_admissible + b.n_admissible;
    out.n_violations = a.n_violations + b.n_violations;
    // The union was checked exactly only if both halves were. A half that was
    // not carries no exact count, and adding its zero to the other half's
    // would report the union as exactly feasible on the strength of pairs
    // nobody asked the question of.
    out.exact_checked = a.exact_checked && b.exact_checked;
    out.n_exact_violations = a.n_exact_violations + b.n_exact_violations;

    const bool b_wins = b.min_reduced_cost < a.min_reduced_cost ||
                        (b.min_reduced_cost == a.min_reduced_cost && a.arg_i < 0) ||
                        (b.min_reduced_cost == a.min_reduced_cost && b.arg_i >= 0 &&
                         (b.arg_i < a.arg_i || (b.arg_i == a.arg_i && b.arg_j < a.arg_j)));
    const ReducedCostScan& best = b_wins ? b : a;
    out.min_reduced_cost = best.min_reduced_cost;
    out.arg_i = best.arg_i;
    out.arg_j = best.arg_j;
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
// The same four are also decided with no tolerance at all, and the report
// carries both readings. Groups 2 and 3 come down to the sign of
// c_ij - u_i - v_j, which lap_exact.h evaluates exactly; group 4 is not
// measured exactly but derived, since tight matched arcs, free unmatched
// columns and a matched row count equal to nrow make the two objectives the
// same sum. `mode` decides which reading the conclusion is taken from.
//
// Condition 3's second half is not redundant. A matching can be perfect,
// dual-feasible everywhere, tight on every matched arc, and carry a dual
// bound equal to the true optimum while its primal cost sits above the
// optimum, when a column is left unmatched holding v_j < 0. Checking only
// dual feasibility and matched-arc tightness certifies that wrong answer.
//
// Returns a report with primal_feasible = false on an empty problem or on any
// length mismatch, rather than reading out of bounds.
namespace detail {

// `supplied` is the reduced-cost scan when the caller already has one and null
// when the certificate has to take it itself. Everything else about the check
// is the same either way, so it is one body: a second copy of the conclusion
// would be free to drift from this one, and the conclusion is the whole point.
template <class Source>
CertificateReport certify_assignment_impl(const Source& src,
                                          const std::vector<int>& match,
                                          const std::vector<double>& u,
                                          const std::vector<double>& v,
                                          double tol,
                                          const ReducedCostScan* supplied,
                                          Arithmetic mode) {
    CertificateReport rep;
    rep.tolerance = tol;
    const bool want_exact = (mode != Arithmetic::Double);

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
    const ReducedCostScan scan = (supplied != nullptr)
        ? *supplied
        : scan_reduced_costs(src, u, v, tol, want_exact);
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

    // The same condition with the band around zero removed. The sign condition
    // is one double against zero and is already exact; what needed the
    // expansion is the reduced cost of every admissible pair, which the scan
    // has counted.
    rep.n_exact_violations = scan.n_exact_violations;
    rep.exact_dual_feasible = scan.exact_checked &&
                              (scan.n_exact_violations == 0) &&
                              (!sign_condition_applies || rep.max_v <= 0.0);

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
        double c = 0.0;
        if (!cost_if_allowed(src, i, j, c)) continue;
        const double ui = u[static_cast<std::size_t>(i)];
        const double vj = v[static_cast<std::size_t>(j)];
        const double slack = std::abs(c - ui - vj);
        if (slack > max_matched_slack) max_matched_slack = slack;
        if (want_exact && exact::sign_reduced_cost(c, ui, vj) != 0) {
            ++rep.n_exact_untight;
        }
    }
    rep.max_matched_slack = max_matched_slack;
    rep.cs_matched_tight = (max_matched_slack <= tol);
    rep.exact_cs_matched_tight = want_exact && (rep.n_exact_untight == 0);

    double max_v_unmatched = 0.0;
    for (int64_t j = 0; j < ncol; ++j) {
        if (col_claims[static_cast<std::size_t>(j)] != 0) continue;
        const double a = std::abs(v[static_cast<std::size_t>(j)]);
        if (a > max_v_unmatched) max_v_unmatched = a;
    }
    rep.max_v_unmatched = max_v_unmatched;
    rep.cs_unmatched_free = (max_v_unmatched <= tol);
    rep.exact_cs_unmatched_free = (max_v_unmatched == 0.0);

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
    const bool numerical = rep.primal_feasible &&
                           rep.dual_feasible &&
                           rep.complementary_slackness &&
                           (std::abs(rep.duality_gap) <= tol_gap);

    // The exact conclusion asks for a matched row count instead of an
    // objective comparison. Tight matched arcs make the primal cost the sum of
    // u over matched rows plus v over used columns; free unmatched columns
    // make the second sum the sum of v over all columns; every row matched
    // makes the first the sum of u over all rows. Those three together are the
    // dual objective, so the gap is zero and no tolerance decides it. The gap
    // is still computed and reported, as the independent numerical
    // cross-check it is.
    rep.all_rows_matched = (rep.n_matched == nrow);
    rep.exact_available = want_exact && scan.exact_checked;
    rep.exact_certificate = rep.exact_available &&
                            rep.primal_feasible &&
                            rep.all_rows_matched &&
                            rep.exact_dual_feasible &&
                            rep.exact_cs_matched_tight &&
                            rep.exact_cs_unmatched_free;

    switch (mode) {
        case Arithmetic::Exact:
            rep.certified_optimal = rep.exact_certificate;
            rep.conclusion_is_exact = true;
            break;
        case Arithmetic::Double:
            rep.certified_optimal = numerical;
            rep.conclusion_is_exact = false;
            break;
        case Arithmetic::Auto:
        default:
            rep.certified_optimal = rep.exact_certificate || numerical;
            rep.conclusion_is_exact = rep.exact_certificate;
            break;
    }

    return rep;
}

}  // namespace detail

template <class Source>
CertificateReport certify_assignment(const Source& src,
                                     const std::vector<int>& match,
                                     const std::vector<double>& u,
                                     const std::vector<double>& v,
                                     double tol,
                                     Arithmetic mode = Arithmetic::Auto) {
    return detail::certify_assignment_impl(src, match, u, v, tol, nullptr, mode);
}

// The same certificate against a scan the caller already holds.
//
// The scan is the only part of the check that costs a pass over the pairs; the
// rest is O(nrow + ncol) over the matching and the duals. A caller that reached
// the minimum some other way -- an edge-generation loop proves it in two halves,
// one over the pairs its master holds and one over the pairs it omits -- has
// the expensive half already and would otherwise pay for it twice.
//
// The scan has to cover every admissible pair of `src`. One that covers fewer
// certifies a smaller problem than the one named here.
template <class Source>
CertificateReport certify_assignment(const Source& src,
                                     const std::vector<int>& match,
                                     const std::vector<double>& u,
                                     const std::vector<double>& v,
                                     double tol,
                                     const ReducedCostScan& scan,
                                     Arithmetic mode = Arithmetic::Auto) {
    return detail::certify_assignment_impl(src, match, u, v, tol, &scan, mode);
}

}  // namespace lap
