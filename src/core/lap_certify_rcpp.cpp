// src/core/lap_certify_rcpp.cpp
// R bindings for the optimality certificate in lap_certify.h.
//
// The templates in lap_certify.h are pure minimization. Everything that has to
// know about `maximize` lives here: the cost entries and both dual vectors are
// negated on the way in, and the three quantities that carry the cost unit --
// primal_objective, dual_objective, duality_gap -- are negated back on the way
// out. Feasibility flags, slackness measures and reduced costs are properties
// of the internal minimization and are reported unflipped.

#include <Rcpp.h>

#include <string>
#include <vector>
#include <cstddef>

#include "lap_certify.h"
#include "lap_types.h"
#include "lap_lazy_types.h"
#include "lap_cost_view.h"
#include "lap_error.h"
#include "lap_rcpp_convert.h"
#include "lap_utils.h"
#include "lap_utils_rcpp.h"

namespace {

// Named-list builder. Rcpp::List::create tops out at 20 arguments and
// CertificateReport has 22 fields, so the list is sized once and filled by
// index.
class ListBuilder {
public:
    explicit ListBuilder(R_xlen_t n) : values_(n), names_(n), k_(0) {}

    void add_flag(const char* name, bool x) {
        values_[k_] = Rcpp::wrap(x);
        names_[k_] = name;
        ++k_;
    }

    void add_number(const char* name, double x) {
        values_[k_] = Rcpp::wrap(x);
        names_[k_] = name;
        ++k_;
    }

    // Counts and indices are int64_t in C++ and are handed to R as doubles.
    // R has no 64-bit integer type, and n_admissible on a 50,000 square
    // problem is 2.5e9, well past what an R integer can hold.
    void add_count(const char* name, int64_t x) {
        values_[k_] = Rcpp::wrap(static_cast<double>(x));
        names_[k_] = name;
        ++k_;
    }

    Rcpp::List finish() {
        values_.attr("names") = names_;
        return values_;
    }

private:
    Rcpp::List values_;
    Rcpp::CharacterVector names_;
    R_xlen_t k_;
};

Rcpp::List scan_to_list(const lap::ReducedCostScan& scan) {
    ListBuilder out(5);

    out.add_number("min_reduced_cost", scan.min_reduced_cost);
    // 0-based argmin, -1 for none, handed to R as 1-based with 0 for none.
    out.add_count("arg_i", scan.arg_i >= 0 ? scan.arg_i + 1 : 0);
    out.add_count("arg_j", scan.arg_j >= 0 ? scan.arg_j + 1 : 0);
    out.add_count("n_violations", scan.n_violations);
    out.add_count("n_admissible", scan.n_admissible);

    return out.finish();
}

// Build a dense lap::CostMatrix from an R matrix, as a solver would read it.
//
// A cell is admissible when it is finite and below lap::BIG. Finiteness is the
// rule rcpp_to_cost_matrix() applies (NA, NaN and +/-Inf are forbidden, which
// is also what prepare_cost_matrix_impl() marks); the sentinel clause is
// forbid_sentinel_costs(), which catches a matrix that has already been through
// prepare_for_solve() and mirrors R's own feasibility rule
// `is.finite(cost) & cost < BIG_COST` in matching_constraints.R. It runs before
// the negation, where the sentinel is still the largest value in the matrix.
//
// prepare_for_solve() then stores forbidden cells as BIG rather than as the
// incoming NA or Inf, so a caller reaching past allowed() meets the package's
// internal sentinel instead of a NaN.
lap::CostMatrix cost_matrix_for_certify(const Rcpp::NumericMatrix& cost, bool negate) {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::forbid_sentinel_costs(cm);
    return lap::prepare_for_solve(cm, negate);
}

// 1-based R match vector (0 = unmatched) to the 0-based convention of
// lap::LapResult::assignment (-1 = unmatched). NA is treated as unmatched, the
// same way compute_total_cost() treats it. A negative entry is left negative
// and out of range so that certify_assignment() reports it in n_out_of_range
// instead of silently reading it as unmatched.
std::vector<int> match_to_zero_based(const Rcpp::IntegerVector& match) {
    std::vector<int> out(static_cast<std::size_t>(match.size()));
    for (R_xlen_t i = 0; i < match.size(); ++i) {
        const int m = match[i];
        if (Rcpp::IntegerVector::is_na(m) || m == 0) {
            out[static_cast<std::size_t>(i)] = -1;
        } else {
            out[static_cast<std::size_t>(i)] = m - 1;
        }
    }
    return out;
}

// The mode arrives as the string R validated, so an unknown one is a bug in
// the wrapper rather than a user error, and defaulting it would hide that.
lap::Arithmetic arithmetic_from_string(const std::string& name) {
    if (name == "exact") return lap::Arithmetic::Exact;
    if (name == "double") return lap::Arithmetic::Double;
    if (name == "auto") return lap::Arithmetic::Auto;
    Rcpp::stop("unknown arithmetic mode: " + name);
}

std::vector<double> duals_to_internal(const Rcpp::NumericVector& x, bool negate) {
    std::vector<double> out(static_cast<std::size_t>(x.size()));
    for (R_xlen_t k = 0; k < x.size(); ++k) {
        out[static_cast<std::size_t>(k)] = negate ? -x[k] : x[k];
    }
    return out;
}

}  // namespace

Rcpp::List certificate_report_to_list(const lap::CertificateReport& rep) {
    ListBuilder out(31);

    out.add_flag("primal_feasible", rep.primal_feasible);
    out.add_count("n_rows", rep.n_rows);
    out.add_count("n_cols", rep.n_cols);
    out.add_count("n_matched", rep.n_matched);
    out.add_count("n_duplicate_cols", rep.n_duplicate_cols);
    out.add_count("n_forbidden_matched", rep.n_forbidden_matched);
    out.add_count("n_out_of_range", rep.n_out_of_range);
    out.add_number("primal_objective", rep.primal_objective);

    out.add_flag("dual_feasible", rep.dual_feasible);
    out.add_number("min_reduced_cost", rep.min_reduced_cost);
    // 0-based, -1 when no admissible pair exists, as they come out of the scan.
    out.add_count("worst_i", rep.worst_i);
    out.add_count("worst_j", rep.worst_j);
    out.add_number("max_v", rep.max_v);
    out.add_number("dual_objective", rep.dual_objective);

    out.add_flag("cs_matched_tight", rep.cs_matched_tight);
    out.add_number("max_matched_slack", rep.max_matched_slack);
    out.add_flag("cs_unmatched_free", rep.cs_unmatched_free);
    out.add_number("max_v_unmatched", rep.max_v_unmatched);
    out.add_flag("complementary_slackness", rep.complementary_slackness);

    out.add_flag("all_rows_matched", rep.all_rows_matched);
    out.add_flag("exact_dual_feasible", rep.exact_dual_feasible);
    out.add_flag("exact_cs_matched_tight", rep.exact_cs_matched_tight);
    out.add_flag("exact_cs_unmatched_free", rep.exact_cs_unmatched_free);
    out.add_flag("exact_certificate", rep.exact_certificate);
    out.add_flag("exact_available", rep.exact_available);
    out.add_count("n_exact_violations", rep.n_exact_violations);
    out.add_count("n_exact_untight", rep.n_exact_untight);

    out.add_number("duality_gap", rep.duality_gap);
    out.add_flag("certified_optimal", rep.certified_optimal);
    out.add_flag("conclusion_is_exact", rep.conclusion_is_exact);
    out.add_number("tolerance", rep.tolerance);

    return out.finish();
}

// The three fields carrying the cost unit go back in the caller's sign; the
// duals were negated on the way in, so a maximize instance was certified
// against -c and reports -objective.
void restore_certificate_sign(lap::CertificateReport& rep, bool maximize) {
    if (!maximize) return;
    rep.primal_objective = -rep.primal_objective;
    rep.dual_objective = -rep.dual_objective;
    rep.duality_gap = -rep.duality_gap;
}

Rcpp::List certify_dense_impl(Rcpp::NumericMatrix cost, Rcpp::IntegerVector match,
                              Rcpp::NumericVector u, Rcpp::NumericVector v,
                              bool maximize, double tol, std::string arithmetic) {
    try {
        const lap::CostMatrix cm = cost_matrix_for_certify(cost, maximize);
        const std::vector<int> m0 = match_to_zero_based(match);
        const std::vector<double> u0 = duals_to_internal(u, maximize);
        const std::vector<double> v0 = duals_to_internal(v, maximize);

        lap::CertificateReport rep = lap::certify_assignment(
            cm, m0, u0, v0, tol, arithmetic_from_string(arithmetic));
        restore_certificate_sign(rep, maximize);

        return certificate_report_to_list(rep);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}

Rcpp::List certify_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                             std::string distance,
                             Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                             double max_distance, Rcpp::List calipers,
                             Rcpp::CharacterVector vars,
                             Rcpp::IntegerVector match, Rcpp::NumericVector u,
                             Rcpp::NumericVector v, bool maximize, double tol, std::string arithmetic) {
    try {
        // The inverse covariance is only read for Mahalanobis; every other
        // metric passes NULL, matching lazy_cost_spec_inv_cov() in
        // R/matching_lazy.R and the signature of cpp_lap_solve_jv_lazy(). A
        // 0 x 0 matrix means the same thing.
        Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov_arg = R_NilValue;
        if (inv_cov.isNotNull()) {
            Rcpp::NumericMatrix ic(inv_cov.get());
            if (ic.nrow() > 0 && ic.ncol() > 0) inv_cov_arg = inv_cov;
        }

        // The lazy source bakes the maximize negation into at() itself
        // (negate = maximize), so it is already the internal minimization.
        const lap::LazyCostMatrix cm = rcpp_to_lazy_cost_matrix(
            left_mat, right_mat, distance, inv_cov_arg, max_distance,
            calipers, vars, maximize);

        const std::vector<int> m0 = match_to_zero_based(match);
        const std::vector<double> u0 = duals_to_internal(u, maximize);
        const std::vector<double> v0 = duals_to_internal(v, maximize);

        lap::CertificateReport rep = lap::certify_assignment(
            cm, m0, u0, v0, tol, arithmetic_from_string(arithmetic));
        restore_certificate_sign(rep, maximize);

        return certificate_report_to_list(rep);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}

Rcpp::List scan_reduced_costs_impl(Rcpp::NumericMatrix cost, Rcpp::NumericVector u,
                                   Rcpp::NumericVector v, double tol) {
    try {
        // No maximize flag: the scan reports the reduced costs of the matrix
        // and duals it is handed, in the sign they arrive in.
        const lap::CostMatrix cm = cost_matrix_for_certify(cost, false);
        const std::vector<double> u0(u.begin(), u.end());
        const std::vector<double> v0(v.begin(), v.end());

        return scan_to_list(lap::scan_reduced_costs(cm, u0, v0, tol));

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}

// Compile-time smoke: instantiate both templates against every cost source the
// package can hand them, so a change to at()/allowed()/nrow/ncol on any of the
// three is a build error here rather than a runtime surprise in whichever
// wrapper happens to touch that source first.
namespace lap {

template ReducedCostScan scan_reduced_costs<CostMatrix>(
    const CostMatrix&, const std::vector<double>&, const std::vector<double>&, double, bool);
template ReducedCostScan scan_reduced_costs<LazyCostMatrix>(
    const LazyCostMatrix&, const std::vector<double>&, const std::vector<double>&, double, bool);
template ReducedCostScan scan_reduced_costs<PaddedCostView<CostMatrix> >(
    const PaddedCostView<CostMatrix>&, const std::vector<double>&,
    const std::vector<double>&, double, bool);

template CertificateReport certify_assignment<CostMatrix>(
    const CostMatrix&, const std::vector<int>&, const std::vector<double>&,
    const std::vector<double>&, double, Arithmetic);
template CertificateReport certify_assignment<LazyCostMatrix>(
    const LazyCostMatrix&, const std::vector<int>&, const std::vector<double>&,
    const std::vector<double>&, double, Arithmetic);
template CertificateReport certify_assignment<PaddedCostView<CostMatrix> >(
    const PaddedCostView<CostMatrix>&, const std::vector<int>&,
    const std::vector<double>&, const std::vector<double>&, double, Arithmetic);

}  // namespace lap
