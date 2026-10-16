// src/select/select_rcpp.cpp
// R bindings for the unit-selection branch and bound of select_bb.h.
//
// A row crosses from R as list(coef, lo, hi, hard): `coef` an n x K matrix
// whose rows sum exactly to the coefficients, so a coefficient that is not a
// double travels as the components of its expansion (one column suffices for
// a row of doubles). The oracle is an R function of the candidate point that
// answers list(feasible, value, rows), `value` a numeric vector summing exactly
// to the objective there, or NULL to have it read off the costs.
#include <Rcpp.h>

#include "../core/lap_exact.h"
#include "../core/lap_rcpp_convert.h"
#include "../core/lap_utils.h"
#include "../core/lap_utils_rcpp.h"
#include "select_bb.h"
#include "select_lift.h"

#include <cmath>
#include <string>
#include <vector>

namespace {

using lap::exact::Expansion;
using lap::select::RowSpec;

Expansion expansion_from_components(const double* x, R_xlen_t k, R_xlen_t stride) {
    Expansion acc;
    Expansion next;
    for (R_xlen_t c = 0; c < k; ++c) {
        const double v = x[c * stride];
        if (!std::isfinite(v)) Rcpp::stop("select: a row coefficient is not finite");
        if (v == 0.0) continue;
        lap::exact::grow_expansion(acc, v, next);
        acc.swap(next);
    }
    lap::exact::compress(acc);
    return acc;
}

RowSpec row_from_r(const Rcpp::List& row, int64_t n) {
    RowSpec spec;
    Rcpp::NumericMatrix coef = Rcpp::as<Rcpp::NumericMatrix>(row["coef"]);
    if (coef.nrow() != n) {
        Rcpp::stop("select: a row has %d coefficients for %d columns",
                   static_cast<int>(coef.nrow()), static_cast<int>(n));
    }
    spec.coef.resize(static_cast<std::size_t>(n));
    const double* data = coef.begin();
    for (int64_t j = 0; j < n; ++j) {
        spec.coef[static_cast<std::size_t>(j)] =
            expansion_from_components(data + j, coef.ncol(), coef.nrow());
    }
    spec.lo = Rcpp::as<double>(row["lo"]);
    spec.hi = Rcpp::as<double>(row["hi"]);
    spec.hard = Rcpp::as<bool>(row["hard"]);
    if (std::isnan(spec.lo) || std::isnan(spec.hi) || spec.lo > spec.hi) {
        Rcpp::stop("select: a row's bounds are not an interval");
    }
    return spec;
}

Expansion expansion_from_r(SEXP value) {
    Rcpp::NumericVector v(value);
    return expansion_from_components(v.begin(), v.size(), 1);
}

Rcpp::NumericVector expansion_to_r_vec(const Expansion& e) {
    return Rcpp::NumericVector(e.begin(), e.end());
}

const char* stop_name(lap::select::Stop s) {
    switch (s) {
        case lap::select::Stop::optimality: return "optimality";
        case lap::select::Stop::node_limit: return "node_limit";
        case lap::select::Stop::time_limit: return "time_limit";
        case lap::select::Stop::interrupt:  return "interrupt";
    }
    return "optimality";
}

}  // namespace

Rcpp::List select_solve_impl(Rcpp::NumericVector c, Rcpp::NumericVector lower,
                             Rcpp::NumericVector upper, Rcpp::LogicalVector integer,
                             Rcpp::List rows, int theta, bool integral_objective, bool perturb,
                             Rcpp::Nullable<Rcpp::List> incumbent,
                             Rcpp::Nullable<Rcpp::Function> oracle,
                             double node_limit, double time_limit,
                             Rcpp::Nullable<Rcpp::Function> should_stop,
                             Rcpp::Nullable<Rcpp::Function> separator) {
    const int64_t n = c.size();
    if (lower.size() != n || upper.size() != n || integer.size() != n) {
        Rcpp::stop("select: the column vectors have different lengths");
    }
    lap::select::SelectProblem prob(n);
    prob.c.assign(c.begin(), c.end());
    prob.lower.assign(lower.begin(), lower.end());
    prob.upper.assign(upper.begin(), upper.end());
    prob.integer.resize(static_cast<std::size_t>(n));
    for (int64_t j = 0; j < n; ++j) {
        const std::size_t sj = static_cast<std::size_t>(j);
        if (!std::isfinite(prob.c[sj])) Rcpp::stop("select: a cost is not finite");
        if (std::isnan(prob.lower[sj]) || std::isnan(prob.upper[sj]) ||
            prob.lower[sj] > prob.upper[sj]) {
            Rcpp::stop("select: column %d has no interval of values", static_cast<int>(j + 1));
        }
        prob.integer[sj] = integer[static_cast<R_xlen_t>(j)] == TRUE ? 1 : 0;
    }
    prob.theta = theta > 0 ? theta - 1 : -1;
    prob.integral_objective = integral_objective;
    prob.perturb = perturb;
    for (R_xlen_t r = 0; r < rows.size(); ++r) {
        prob.rows.add(row_from_r(Rcpp::as<Rcpp::List>(rows[r]), n));
    }

    bool interrupted = false;
    lap::select::Oracle check;
    if (oracle.isNotNull()) {
        Rcpp::Function fn(oracle.get());
        check = [fn, n, &interrupted](const std::vector<double>& x) {
            lap::select::Verdict v;
            if (interrupted) return v;
            SEXP res;
            try {
                res = fn(Rcpp::NumericVector(x.begin(), x.end()));
            } catch (Rcpp::internal::InterruptedException&) {
                interrupted = true;
                return v;
            }
            Rcpp::List out(res);
            v.feasible = Rcpp::as<bool>(out["feasible"]);
            SEXP value = out["value"];
            if (value != R_NilValue) {
                v.has_value = true;
                v.value = expansion_from_r(value);
            }
            SEXP added = out["rows"];
            if (added != R_NilValue) {
                Rcpp::List lst(added);
                for (R_xlen_t k = 0; k < lst.size(); ++k) {
                    v.rows.push_back(row_from_r(Rcpp::as<Rcpp::List>(lst[k]), n));
                }
            }
            return v;
        };
    }

    lap::select::Limits limits;
    if (std::isfinite(node_limit) && node_limit >= 0) {
        limits.node_limit = static_cast<int64_t>(node_limit);
    }
    limits.time_limit = time_limit;
    Rcpp::Function* stop_fn = nullptr;
    std::unique_ptr<Rcpp::Function> stop_holder;
    if (should_stop.isNotNull()) {
        stop_holder.reset(new Rcpp::Function(should_stop.get()));
        stop_fn = stop_holder.get();
    }
    limits.stop = [&interrupted, stop_fn](const lap::select::Progress& p) {
        if (interrupted) return true;
        try {
            Rcpp::checkUserInterrupt();
        } catch (Rcpp::internal::InterruptedException&) {
            interrupted = true;
            return true;
        }
        if (!stop_fn) return false;
        Rcpp::List state = Rcpp::List::create(
            Rcpp::Named("n_nodes") = static_cast<double>(p.n_nodes),
            Rcpp::Named("elapsed") = p.elapsed,
            Rcpp::Named("incumbent") = p.incumbent,
            Rcpp::Named("bound") = p.bound);
        try {
            return Rcpp::as<bool>((*stop_fn)(state));
        } catch (Rcpp::internal::InterruptedException&) {
            interrupted = true;
            return true;
        }
    };

    lap::select::Separator separate;
    if (separator.isNotNull()) {
        Rcpp::Function sep(separator.get());
        separate = [sep, n, &interrupted](const std::vector<double>& x, int64_t depth) {
            std::vector<RowSpec> rows_out;
            if (interrupted) return rows_out;
            SEXP added;
            try {
                added = sep(Rcpp::NumericVector(x.begin(), x.begin() + n),
                            static_cast<double>(depth));
            } catch (Rcpp::internal::InterruptedException&) {
                interrupted = true;
                return rows_out;
            }
            if (added == R_NilValue) return rows_out;
            Rcpp::List lst(added);
            for (R_xlen_t k = 0; k < lst.size(); ++k) {
                rows_out.push_back(row_from_r(Rcpp::as<Rcpp::List>(lst[k]), n));
            }
            return rows_out;
        };
    }
    lap::select::BranchAndBound bb(prob, check, limits, separate);
    if (incumbent.isNotNull()) {
        Rcpp::List inc(incumbent.get());
        Rcpp::NumericVector x = inc["x"];
        if (x.size() != n) Rcpp::stop("select: the incumbent has the wrong length");
        bb.set_incumbent(std::vector<double>(x.begin(), x.end()), expansion_from_r(inc["value"]));
    }
    const lap::select::SearchResult res = bb.run();

    return Rcpp::List::create(
        Rcpp::Named("stopped_on") = std::string(stop_name(res.stopped)),
        Rcpp::Named("has_incumbent") = res.has_incumbent,
        Rcpp::Named("x") = Rcpp::NumericVector(res.x.begin(), res.x.end()),
        Rcpp::Named("value") = expansion_to_r_vec(res.value),
        Rcpp::Named("bound") = res.bound,
        Rcpp::Named("bound_certified") = res.bound_certified,
        Rcpp::Named("n_nodes") = static_cast<double>(res.n_nodes),
        Rcpp::Named("lp_iterations") = static_cast<double>(res.lp_iterations),
        Rcpp::Named("rows_added") = static_cast<double>(res.rows_added),
        Rcpp::Named("interrupted") = interrupted);
}

// The distance cut of a pinned solve, lifted on the units outside the
// selection (select_lift.h), over a dense cost matrix whose non-finite cells
// are forbidden. `u` and `v` cross as expansion matrices, as they came from
// the solve; `in_left` and `in_right` mark the selection.
Rcpp::List select_lift_dense_impl(Rcpp::NumericMatrix cost, double shift,
                                  Rcpp::LogicalVector in_left, Rcpp::LogicalVector in_right,
                                  Rcpp::NumericMatrix u, Rcpp::NumericMatrix v) {
    lap::CostMatrix cm = rcpp_to_cost_matrix(cost);
    lap::forbid_sentinel_costs(cm);
    std::vector<lap::exact::Expansion> U = expansions_from_r(u, false);
    std::vector<lap::exact::Expansion> V = expansions_from_r(v, false);
    std::vector<uint8_t> il(in_left.begin(), in_left.end());
    std::vector<uint8_t> ir(in_right.begin(), in_right.end());
    if (static_cast<int64_t>(U.size()) != cm.nrow || static_cast<int64_t>(V.size()) != cm.ncol ||
        static_cast<int64_t>(il.size()) != cm.nrow || static_cast<int64_t>(ir.size()) != cm.ncol) {
        Rcpp::stop("select lift: the potentials do not fit the cost matrix");
    }
    lap::select::lift_outside(cm, shift, il, ir, U, V);
    return Rcpp::List::create(Rcpp::Named("u") = expansions_to_r(U),
                              Rcpp::Named("v") = expansions_to_r(V));
}
