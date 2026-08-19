// src/flow/flow_implicit_rcpp.cpp
// R bindings for the edge-generation loop in flow_implicit.h.
//
// The loop crosses to R once, not once per round. It owns the FlowProblem, the
// candidate set and the cost source, none of which has an R representation, so
// what a caller states is the problem -- a cost source and the knobs the search
// takes -- and what comes back is the answer, the certificate assembled for the
// complete implicit problem, and the record of what the search cost.
//
// Two entry points, one body. The loop is templated on the cost-source concept
// and both sources satisfy it: a lazy specification, which is the shape the
// memory saving is for, and a materialized matrix, which is what lets a caller
// hold the loop's answer against the dense solve of the same numbers.
//
// The templates are pure minimization, so `maximize` is handled the way
// lap_certify_rcpp.cpp handles it: the source is negated at construction, and
// the quantities carrying the cost unit -- the total, the duals, the per-round
// master cost, and the certificate's two objectives and gap -- are negated back
// on the way out.

#include <Rcpp.h>

#include "../core/lap_certify.h"
#include "../core/lap_error.h"
#include "../core/lap_lazy_types.h"
#include "../core/lap_rcpp_convert.h"
#include "../core/lap_types.h"
#include "../core/lap_utils.h"
#include "../core/lap_utils_rcpp.h"
#include "flow_candidates.h"
#include "flow_compile.h"
#include "flow_implicit.h"
#include "flow_implicit_rcpp.h"
#include "flow_oracle.h"
#include "flow_problem.h"

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

// The pieces the design path reports too. Declared in flow_implicit_rcpp.h.

int64_t implicit_knob_from_r(double v, const char* what) {
    if (ISNAN(v) || !R_finite(v)) {
        Rcpp::stop("edge generation: %s is not a finite number", what);
    }
    if (v != std::floor(v)) {
        Rcpp::stop("edge generation: %s is not a whole number", what);
    }
    return static_cast<int64_t>(v);
}

lap::ImplicitOptions implicit_options_from_r(double keep_per_row, double width,
                                             double tol, double max_rounds,
                                             bool certify) {
    lap::ImplicitOptions opts;
    opts.keep_per_row =
        static_cast<int>(implicit_knob_from_r(keep_per_row, "keep_per_row"));
    opts.width      = static_cast<int>(implicit_knob_from_r(width, "width"));
    opts.max_rounds = implicit_knob_from_r(max_rounds, "max_rounds");
    opts.tol        = tol;
    opts.certify    = certify;
    return opts;
}

Rcpp::IntegerVector implicit_match_to_r(const std::vector<int>& match, int64_t nrow) {
    Rcpp::IntegerVector out(static_cast<R_xlen_t>(nrow));
    for (R_xlen_t i = 0; i < out.size(); ++i) out[i] = 0;
    for (std::size_t i = 0; i < match.size(); ++i) {
        out[static_cast<R_xlen_t>(i)] = (match[i] >= 0) ? (match[i] + 1) : 0;
    }
    return out;
}

Rcpp::List implicit_rounds_to_r(const std::vector<lap::ImplicitRound>& rounds,
                                bool maximize) {
    const R_xlen_t n = static_cast<R_xlen_t>(rounds.size());

    Rcpp::NumericVector   round(n), candidate_pairs(n), block_arcs(n);
    Rcpp::NumericVector   flow_sent(n), flow_required(n), master_cost(n);
    Rcpp::NumericVector   master_seconds(n), min_reduced_cost(n), matched_slack(n);
    Rcpp::NumericVector   n_violators(n), n_evaluated(n), pairs_added(n);
    Rcpp::NumericVector   arcs_added(n), pricing_seconds(n);
    Rcpp::CharacterVector kind(n), master_status(n);

    for (R_xlen_t k = 0; k < n; ++k) {
        const lap::ImplicitRound& r = rounds[static_cast<std::size_t>(k)];
        round[k]            = static_cast<double>(r.round);
        kind[k]             = (r.kind == lap::ImplicitRound::Kind::priced) ? "priced"
                                                                          : "reseeded";
        master_status[k]    = r.master_status;
        candidate_pairs[k]  = static_cast<double>(r.candidate_pairs);
        block_arcs[k]       = static_cast<double>(r.block_arcs);
        flow_sent[k]        = static_cast<double>(r.flow_sent);
        flow_required[k]    = static_cast<double>(r.flow_required);
        master_cost[k]      = maximize ? -r.master_cost : r.master_cost;
        master_seconds[k]   = r.master_seconds;
        min_reduced_cost[k] = r.min_reduced_cost;
        n_violators[k]      = static_cast<double>(r.n_violators);
        n_evaluated[k]      = static_cast<double>(r.n_evaluated);
        pairs_added[k]      = static_cast<double>(r.pairs_added);
        arcs_added[k]       = static_cast<double>(r.arcs_added);
        pricing_seconds[k]  = r.pricing_seconds;
        matched_slack[k]    = r.matched_slack;
    }

    return Rcpp::List::create(
        Rcpp::Named("round") = round,
        Rcpp::Named("kind") = kind,
        Rcpp::Named("master_status") = master_status,
        Rcpp::Named("candidate_pairs") = candidate_pairs,
        Rcpp::Named("block_arcs") = block_arcs,
        Rcpp::Named("flow_sent") = flow_sent,
        Rcpp::Named("flow_required") = flow_required,
        Rcpp::Named("master_cost") = master_cost,
        Rcpp::Named("master_seconds") = master_seconds,
        Rcpp::Named("min_reduced_cost") = min_reduced_cost,
        Rcpp::Named("n_violators") = n_violators,
        Rcpp::Named("n_evaluated") = n_evaluated,
        Rcpp::Named("pairs_added") = pairs_added,
        Rcpp::Named("arcs_added") = arcs_added,
        Rcpp::Named("pricing_seconds") = pricing_seconds,
        Rcpp::Named("matched_slack") = matched_slack);
}

namespace {

Rcpp::NumericVector duals_to_r(const std::vector<double>& x, bool maximize) {
    Rcpp::NumericVector out(static_cast<R_xlen_t>(x.size()));
    for (std::size_t k = 0; k < x.size(); ++k) {
        out[static_cast<R_xlen_t>(k)] = maximize ? -x[k] : x[k];
    }
    return out;
}

Rcpp::List result_to_r(const lap::ImplicitResult& res, int64_t nrow, bool maximize) {
    const Rcpp::IntegerVector match = implicit_match_to_r(res.match, nrow);

    // A defaulted report has no rows, and a real one always has at least one,
    // so this is the question "was a certificate assembled" without a second
    // field carrying the answer.
    Rcpp::RObject certificate = R_NilValue;
    if (res.certificate.n_rows > 0) {
        lap::CertificateReport rep = res.certificate;
        restore_certificate_sign(rep, maximize);
        certificate = certificate_report_to_list(rep);
    }

    Rcpp::RObject witness = R_NilValue;
    if (res.status == "infeasible") {
        witness = hall_witness_to_list(res.witness);
    }

    return Rcpp::List::create(
        Rcpp::Named("match") = match,
        Rcpp::Named("total_cost") = maximize ? -res.total_cost : res.total_cost,
        Rcpp::Named("status") = res.status,
        Rcpp::Named("u") = duals_to_r(res.u, maximize),
        Rcpp::Named("v") = duals_to_r(res.v, maximize),
        Rcpp::Named("certificate") = certificate,
        Rcpp::Named("certified") = res.certified,
        Rcpp::Named("candidate_edges") = static_cast<double>(res.candidate_edges),
        Rcpp::Named("possible_edges") = static_cast<double>(res.possible_edges),
        Rcpp::Named("edges_evaluated") = static_cast<double>(res.edges_evaluated),
        Rcpp::Named("n_rounds") = static_cast<double>(res.rounds.size()),
        Rcpp::Named("rounds") = implicit_rounds_to_r(res.rounds, maximize),
        Rcpp::Named("witness") = witness,
        Rcpp::Named("witness_certified") = res.witness_certified);
}

// The body both entry points share: compile the one-to-one design over the
// source, start from an empty candidate set, and run the loop. The design is
// compiled here rather than in R because a compiled problem is a C++ object,
// and it is compiled over an oracle wrapping the same source the loop prices
// with, so the master and the pricer read one set of costs.
template <class Source>
Rcpp::List run_implicit(const Source& src, const lap::ImplicitOptions& opts,
                        bool maximize) {
    lap::require_rows_fit_cols(static_cast<int>(src.nrow), static_cast<int>(src.ncol));

    lap::SourceOracle<Source> oracle(src);
    lap::CompiledDesign design =
        lap::compile_one_to_one(oracle, std::vector<lap::CategoryConstraint>());
    lap::CandidateSet cand(src.nrow, src.ncol);

    const lap::ImplicitResult res =
        lap::solve_implicit_assignment(src, design.problem, cand, opts);

    return result_to_r(res, src.nrow, maximize);
}

}  // namespace

Rcpp::List implicit_dense_impl(Rcpp::NumericMatrix cost, bool maximize,
                               double keep_per_row, double width, double tol,
                               double max_rounds, bool certify) {
    try {
        // The matrix a solver reads: NA, Inf and the forbidden sentinel are no
        // edge, and the costs are negated under maximize. The loop reads its
        // source through at()/allowed() like any other solver, so it is handed
        // the same prepared matrix.
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
        lap::forbid_sentinel_costs(cm);
        const lap::CostMatrix work = lap::prepare_for_solve(cm, maximize);

        return run_implicit(work, implicit_options_from_r(keep_per_row, width, tol,
                                                          max_rounds, certify),
                            maximize);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}

Rcpp::List implicit_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              std::string distance,
                              Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers,
                              Rcpp::CharacterVector vars, bool maximize,
                              double keep_per_row, double width, double tol,
                              double max_rounds, bool certify) {
    try {
        // Only Mahalanobis reads an inverse covariance; every other metric
        // passes NULL, and a 0 x 0 matrix says the same thing. Same handling as
        // certify_lazy_impl(), for the same reason.
        Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov_arg = R_NilValue;
        if (inv_cov.isNotNull()) {
            Rcpp::NumericMatrix ic(inv_cov.get());
            if (ic.nrow() > 0 && ic.ncol() > 0) inv_cov_arg = inv_cov;
        }

        // The lazy source bakes the calipers, the max_distance cut and the
        // maximize negation into at()/allowed() at construction, so it is
        // already the internal minimization.
        const lap::LazyCostMatrix cm = rcpp_to_lazy_cost_matrix(
            left_mat, right_mat, distance, inv_cov_arg, max_distance, calipers,
            vars, maximize);

        return run_implicit(cm, implicit_options_from_r(keep_per_row, width, tol,
                                                        max_rounds, certify),
                            maximize);

    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }

    return Rcpp::List();
}
