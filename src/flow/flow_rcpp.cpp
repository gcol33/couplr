// src/flow/flow_rcpp.cpp
// R bindings for the flow model.
//
// The three steps of the module are bound separately and compose in R the way
// they compose in C++: a design compiles to a problem, the problem solves, and
// the flow and the potentials it produced are handed back to the certificate as
// an input to a check. Binding the composite instead would put the one place
// optimality is decided out of reach of R.
//
// A compiled design crosses the boundary as an explicit arc list, so the block
// is expanded here rather than in R: expansion reads the cost source through a
// virtual call per pair, which is a C++ loop and not an R one.
//
// Capacities and supplies are int64_t in the model and R has no 64-bit integer
// type, so they cross as doubles. Every count a matching design produces is far
// below 2^53, where a double is exact. The one value that is not is
// FLOW_INF_CAP, which crosses as Inf and is restored on the way back in.

#include <Rcpp.h>

#include "flow_certify.h"
#include "flow_compile.h"
#include "flow_oracle.h"
#include "flow_problem.h"
#include "flow_solve.h"

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <string>
#include <vector>

namespace {

// Largest integer a double holds exactly. A count above it has already lost its
// low bits by the time it arrives, so it is rejected rather than rounded into a
// different problem.
constexpr double FLOW_R_EXACT_MAX = 9007199254740992.0;  // 2^53

int64_t count_from_r(double v, const char* what) {
    if (ISNAN(v)) {
        Rcpp::stop("flow model: %s is NA", what);
    }
    if (v == R_PosInf) return lap::FLOW_INF_CAP;
    if (v == R_NegInf || std::abs(v) > FLOW_R_EXACT_MAX) {
        Rcpp::stop("flow model: %s is outside the range a double counts exactly",
                   what);
    }
    if (v != std::floor(v)) {
        Rcpp::stop("flow model: %s is not a whole number", what);
    }
    return static_cast<int64_t>(v);
}

double count_to_r(int64_t v) {
    return (v >= lap::FLOW_INF_CAP) ? R_PosInf : static_cast<double>(v);
}

// The caller's cost matrix read where it lies. A compiler asks a cost source
// for one cell at a time and expansion visits each cell once, so copying the
// matrix into a lap::CostMatrix first would allocate a second copy of the
// caller's largest object to answer questions the original already answers.
//
// A pair is admissible when its distance is a number. R's NA arrives as a NaN
// payload and a caliper writes Inf, so one test covers both, and it is the
// same test expand_blocks() applies to the value it reads.
class RMatrixSource final : public lap::CostOracle {
public:
    explicit RMatrixSource(const Rcpp::NumericMatrix& mat) : mat_(mat) {}

    double at(int64_t i, int64_t j) const override {
        return mat_(static_cast<R_xlen_t>(i), static_cast<R_xlen_t>(j));
    }
    bool allowed(int64_t i, int64_t j) const override {
        return std::isfinite(at(i, j));
    }
    int64_t nrow() const override { return mat_.nrow(); }
    int64_t ncol() const override { return mat_.ncol(); }

private:
    const Rcpp::NumericMatrix& mat_;
};

// Node ids are 1-based in R and 0-based in the model, which is the only
// translation the arc columns need.
lap::FlowProblem problem_from_r(int n_nodes,
                                const Rcpp::NumericVector& supply,
                                const Rcpp::IntegerVector& tail,
                                const Rcpp::IntegerVector& head,
                                const Rcpp::NumericVector& lower,
                                const Rcpp::NumericVector& upper,
                                const Rcpp::NumericVector& cost) {
    const R_xlen_t n_arcs = tail.size();
    if (head.size() != n_arcs || lower.size() != n_arcs ||
        upper.size() != n_arcs || cost.size() != n_arcs) {
        Rcpp::stop("flow model: the arc columns have different lengths");
    }
    if (n_nodes < 0) {
        Rcpp::stop("flow model: n_nodes is negative");
    }
    if (supply.size() != n_nodes) {
        Rcpp::stop("flow model: supply has %d entries for %d nodes",
                   static_cast<int>(supply.size()), n_nodes);
    }

    lap::FlowProblem prob;
    prob.n_nodes = n_nodes;
    prob.supply.resize(static_cast<std::size_t>(n_nodes));
    for (int v = 0; v < n_nodes; ++v) {
        prob.supply[static_cast<std::size_t>(v)] =
            count_from_r(supply[v], "a supply");
    }

    prob.arcs.reserve(static_cast<std::size_t>(n_arcs));
    for (R_xlen_t a = 0; a < n_arcs; ++a) {
        if (tail[a] == NA_INTEGER || head[a] == NA_INTEGER) {
            Rcpp::stop("flow model: an arc endpoint is NA");
        }
        prob.arcs.emplace_back(static_cast<int32_t>(tail[a] - 1),
                               static_cast<int32_t>(head[a] - 1),
                               count_from_r(lower[a], "an arc lower bound"),
                               count_from_r(upper[a], "an arc upper bound"),
                               cost[a]);
    }

    // There are no blocks, so the arc array is already the whole problem and the
    // certificate may read it as an expanded one.
    prob.expanded = true;
    return prob;
}

Rcpp::List certificate_to_r(const lap::FlowCertificate& cert) {
    return Rcpp::List::create(
        Rcpp::Named("primal_feasible") = cert.primal_feasible,
        Rcpp::Named("n_capacity_violations") =
            static_cast<double>(cert.n_capacity_violations),
        Rcpp::Named("n_conservation_violations") =
            static_cast<double>(cert.n_conservation_violations),
        Rcpp::Named("max_conservation_error") = cert.max_conservation_error,
        Rcpp::Named("primal_objective") = cert.primal_objective,
        Rcpp::Named("dual_feasible") = cert.dual_feasible,
        Rcpp::Named("min_residual_reduced_cost") = cert.min_residual_reduced_cost,
        Rcpp::Named("worst_arc") = static_cast<double>(cert.worst_arc),
        Rcpp::Named("complementary_slackness") = cert.complementary_slackness,
        Rcpp::Named("n_cs_violations") = static_cast<double>(cert.n_cs_violations),
        Rcpp::Named("dual_objective") = cert.dual_objective,
        Rcpp::Named("duality_gap") = cert.duality_gap,
        Rcpp::Named("certified_optimal") = cert.certified_optimal,
        Rcpp::Named("tolerance") = cert.tolerance);
}

}  // namespace

Rcpp::List flow_solve_impl(int n_nodes,
                           Rcpp::NumericVector supply,
                           Rcpp::IntegerVector tail,
                           Rcpp::IntegerVector head,
                           Rcpp::NumericVector lower,
                           Rcpp::NumericVector upper,
                           Rcpp::NumericVector cost,
                           double tol,
                           double relax_eps,
                           double max_augmentations,
                           bool return_potentials) {
    lap::FlowProblem prob =
        problem_from_r(n_nodes, supply, tail, head, lower, upper, cost);

    lap::FlowOptions opts;
    opts.tol = tol;
    opts.relax_eps = relax_eps;
    opts.max_augmentations = count_from_r(max_augmentations, "max_augmentations");
    opts.return_potentials = return_potentials;

    const lap::FlowResult res = lap::solve_min_cost_flow(prob, opts);

    Rcpp::NumericVector flow(static_cast<R_xlen_t>(res.flow.size()));
    for (std::size_t a = 0; a < res.flow.size(); ++a) {
        flow[static_cast<R_xlen_t>(a)] = static_cast<double>(res.flow[a]);
    }
    Rcpp::NumericVector potential(static_cast<R_xlen_t>(res.potential.size()));
    for (std::size_t v = 0; v < res.potential.size(); ++v) {
        potential[static_cast<R_xlen_t>(v)] = res.potential[v];
    }

    return Rcpp::List::create(
        Rcpp::Named("flow") = flow,
        Rcpp::Named("potential") = potential,
        Rcpp::Named("total_cost") = res.total_cost,
        Rcpp::Named("flow_sent") = static_cast<double>(res.flow_sent),
        Rcpp::Named("flow_required") = static_cast<double>(res.flow_required),
        Rcpp::Named("status") = res.status,
        Rcpp::Named("n_augmentations") = static_cast<double>(res.n_augmentations));
}

Rcpp::List flow_certify_impl(int n_nodes,
                             Rcpp::NumericVector supply,
                             Rcpp::IntegerVector tail,
                             Rcpp::IntegerVector head,
                             Rcpp::NumericVector lower,
                             Rcpp::NumericVector upper,
                             Rcpp::NumericVector cost,
                             Rcpp::NumericVector flow,
                             Rcpp::NumericVector potential,
                             double tol) {
    const lap::FlowProblem prob =
        problem_from_r(n_nodes, supply, tail, head, lower, upper, cost);

    // A flow that is not integral, or that carries a count no int64_t holds, is
    // not a flow this LP has a primal for. It fails the certificate rather than
    // being rounded into one that passes.
    std::vector<int64_t> f;
    f.reserve(static_cast<std::size_t>(flow.size()));
    bool integral = true;
    for (R_xlen_t a = 0; a < flow.size(); ++a) {
        const double v = flow[a];
        if (ISNAN(v) || std::abs(v) > FLOW_R_EXACT_MAX || v != std::floor(v)) {
            integral = false;
            break;
        }
        f.push_back(static_cast<int64_t>(v));
    }
    if (!integral) {
        lap::FlowCertificate rep;
        rep.tolerance = tol;
        return certificate_to_r(rep);
    }

    std::vector<double> pi(potential.begin(), potential.end());
    return certificate_to_r(lap::certify_flow(prob, f, pi, tol));
}

Rcpp::List flow_compile_full_match_impl(Rcpp::NumericMatrix cost,
                                        double min_controls,
                                        double max_controls) {
    const RMatrixSource source(cost);
    lap::CompiledFullMatch design = lap::compile_full_matching(
        source,
        count_from_r(min_controls, "min_controls"),
        count_from_r(max_controls, "max_controls"),
        std::vector<lap::CategoryConstraint>());

    Rcpp::List shape = Rcpp::List::create(
        Rcpp::Named("transposed") = design.transposed,
        Rcpp::Named("n_centres") = count_to_r(design.n_centres),
        Rcpp::Named("n_units") = count_to_r(design.n_units),
        Rcpp::Named("min_controls") = count_to_r(design.min_controls),
        Rcpp::Named("max_capacity") = count_to_r(design.max_capacity),
        Rcpp::Named("flow_required") = count_to_r(design.design.flow_required));

    if (!design.bounds_feasible) {
        return Rcpp::List::create(
            Rcpp::Named("bounds_feasible") = false,
            Rcpp::Named("reason") = design.reason,
            Rcpp::Named("shape") = shape);
    }

    lap::FlowProblem& prob = design.design.problem;
    lap::expand_blocks(prob);

    const R_xlen_t n_arcs = static_cast<R_xlen_t>(prob.arcs.size());
    Rcpp::IntegerVector tail(n_arcs);
    Rcpp::IntegerVector head(n_arcs);
    Rcpp::NumericVector lower(n_arcs);
    Rcpp::NumericVector upper(n_arcs);
    Rcpp::NumericVector arc_cost(n_arcs);
    for (R_xlen_t a = 0; a < n_arcs; ++a) {
        const lap::FlowArc& arc = prob.arcs[static_cast<std::size_t>(a)];
        tail[a] = arc.tail + 1;
        head[a] = arc.head + 1;
        lower[a] = count_to_r(arc.lower);
        upper[a] = count_to_r(arc.upper);
        arc_cost[a] = arc.cost;
    }

    Rcpp::NumericVector supply(prob.n_nodes);
    for (int v = 0; v < prob.n_nodes; ++v) {
        supply[v] = static_cast<double>(prob.supply[static_cast<std::size_t>(v)]);
    }

    const lap::BlockArcRange& blk = prob.block_arcs.at(0);
    Rcpp::IntegerVector block_row(static_cast<R_xlen_t>(blk.n_arcs));
    Rcpp::IntegerVector block_col(static_cast<R_xlen_t>(blk.n_arcs));
    for (int64_t k = 0; k < blk.n_arcs; ++k) {
        block_row[static_cast<R_xlen_t>(k)] =
            blk.rc[static_cast<std::size_t>(k)].first + 1;
        block_col[static_cast<R_xlen_t>(k)] =
            blk.rc[static_cast<std::size_t>(k)].second + 1;
    }

    return Rcpp::List::create(
        Rcpp::Named("bounds_feasible") = true,
        Rcpp::Named("reason") = design.reason,
        Rcpp::Named("shape") = shape,
        Rcpp::Named("problem") = Rcpp::List::create(
            Rcpp::Named("n_nodes") = static_cast<int>(prob.n_nodes),
            Rcpp::Named("supply") = supply,
            Rcpp::Named("tail") = tail,
            Rcpp::Named("head") = head,
            Rcpp::Named("lower") = lower,
            Rcpp::Named("upper") = upper,
            Rcpp::Named("cost") = arc_cost),
        Rcpp::Named("block") = Rcpp::List::create(
            Rcpp::Named("first_arc") = static_cast<double>(blk.first_arc + 1),
            Rcpp::Named("n_arcs") = static_cast<double>(blk.n_arcs),
            Rcpp::Named("row") = block_row,
            Rcpp::Named("col") = block_col),
        Rcpp::Named("layout") = Rcpp::List::create(
            Rcpp::Named("source_node") = static_cast<int>(lap::FLOW_SOURCE) + 1,
            Rcpp::Named("sink_node") = static_cast<int>(lap::FLOW_SINK) + 1,
            Rcpp::Named("row_base") = static_cast<int>(design.design.row_base) + 1,
            Rcpp::Named("n_rows") = static_cast<int>(design.design.n_rows),
            Rcpp::Named("col_base") = static_cast<int>(design.design.col_base) + 1,
            Rcpp::Named("n_cols") = static_cast<int>(design.design.n_cols)));
}
