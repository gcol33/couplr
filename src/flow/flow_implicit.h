// src/flow/flow_implicit.h
// The outer loop: an assignment over a complete implicit graph, solved by
// generating the pairs it turns out to need.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The problem is stated over every pair of a cost source and solved over a
// fraction of them. Seven steps:
//
//   1. Give every row some admissible columns to start from.
//   2. Solve that restricted master.
//   3. Read its potentials as assignment duals u, v, on the tight face the
//      assignment LP asks for rather than the one min-cost flow settles for.
//   4. Price the pairs the master does not hold: cbar_ij = c_ij - u_i - v_j.
//   5. Add the pairs that price below -tol, and only those.
//   6. Warm start from the incumbent flow and re-solve.
//   7. Repeat until nothing prices below -tol.
//
// At that point the duals are feasible for every pair of the complete implicit
// problem and the restricted answer is optimal for it. That is what separates
// this from approximate k-nearest matching, which solves the same restricted
// master and stops at step 2.
//
// The loop lives in C++ because it owns the FlowProblem, the candidate set and
// the cost source, none of which have an R representation, and because it
// crosses to R once rather than once per round.
//
// **Feasibility is a different question and gets a different phase.** A master
// that comes back short of the required flow has no dual solution to price
// with, so a round that falls short hands over to flow_feasibility.h: Hall's
// witness names the rows that could not be matched and the re-seed aims at the
// columns that can repair them, or reports that no arc set over this source
// admits a complete matching and says why.
//
// Scope is the unit-capacity bipartite assignment: one block, every row
// carrying one unit, every column at most one. That is the design Hall's
// condition answers feasibility for and the LP lap_certify.h certifies.
#pragma once

#include "../core/lap_certify.h"
#include "../core/lap_cost_source.h"
#include "../core/lap_error.h"
#include "../core/lap_hall.h"
#include "flow_candidates.h"
#include "flow_certify.h"
#include "flow_feasibility.h"
#include "flow_pricing.h"
#include "flow_problem.h"
#include "flow_solve.h"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <utility>
#include <vector>

namespace lap {

struct ImplicitOptions {
    // Violators a row contributes per pricing round. At least one, because a
    // round that adds nothing prices the same pairs again next round.
    int keep_per_row = 5;

    // Columns the first feasibility round gives each deficient row, doubled by
    // every round that has to run again. It is also the seed: a loop started
    // from an empty candidate set spends its first round with every row
    // deficient, which is the k-nearest seed taken over the whole source.
    int width = 5;

    // Zero threshold for pricing and for the certificate. A pair prices out at
    // cbar < -tol, and the loop stops when none does.
    double tol = 1e-9;

    // Guard, not a convergence bound. Each round adds at least one pair the
    // master did not hold, so the loop terminates on its own after at most
    // nrow * ncol rounds; a run that reaches this limit has found something
    // worth looking at rather than a problem that needs more rounds.
    int64_t max_rounds = 60;

    // Whether to assemble the certificate on termination. The pricing round is
    // run either way -- it is what stops the loop -- so what this buys is the
    // other half of the scan, over the pairs the master holds, and the
    // conclusion drawn from the two together. Off, the answer carries the
    // status the master terminated on and no proof.
    bool certify = true;

    // Handed to every master solve. return_potentials is forced on: the duals
    // are what the next round prices with.
    FlowOptions flow;
};

// One round of the loop: the master it solved, and what it did about the
// answer. This is what edges_evaluated is computed from, and it is the per-step
// state a trace layer reads, so it is a record rather than a private counter.
struct ImplicitRound {
    enum class Kind {
        priced,    // the master was feasible and its omitted pairs were priced
        reseeded   // the master came back short and the deficient rows grew
    };

    int64_t round = 0;
    Kind    kind  = Kind::priced;

    // The master this round solved.
    std::string master_status;
    int64_t candidate_pairs = 0;   // pairs the candidate set held
    int64_t block_arcs      = 0;   // of those, the ones that became arcs
    int64_t flow_sent       = 0;
    int64_t flow_required   = 0;
    double  master_cost     = 0.0;
    double  master_seconds  = 0.0;

    // What the round then did. `min_reduced_cost` is over the omitted pairs and
    // stays infinite on a reseed round, which prices nothing.
    double  min_reduced_cost = std::numeric_limits<double>::infinity();
    int64_t n_violators      = 0;
    int64_t n_evaluated      = 0;   // pairs this round computed a cost for
    int64_t pairs_added      = 0;
    int64_t arcs_added       = 0;
    double  pricing_seconds  = 0.0;

    // How far the master's own potentials sat off the tight face before
    // tighten_matched_duals() put them back on it. A cold master leaves it at
    // rounding; a warm one is free to leave it anywhere, which is what the
    // projection is there for.
    double matched_slack = 0.0;
};

struct ImplicitResult {
    // From solver_status_values(). The loop either matches every row, reports
    // "infeasible" with the witness saying no arc set over this source could,
    // or hits a limit: its own max_rounds, or a master that ran out of
    // augmentations, both reported as "iteration_limit".
    std::string status = "infeasible";

    // One column per row, -1 unmatched, and the duals the last master produced.
    // Empty when no master ever reached a matching.
    std::vector<int>    match;
    std::vector<double> u;
    std::vector<double> v;
    double total_cost = 0.0;

    // The certificate for the COMPLETE implicit problem, assembled from the
    // scan over the master's own pairs and the scan over the pairs it omits.
    // Meaningful when the status is "optimal".
    CertificateReport certificate;
    bool certified = false;

    std::vector<ImplicitRound> rounds;

    // What the search cost, in the terms the public surface reports.
    int64_t candidate_edges = 0;   // pairs the candidate set ended up holding
    int64_t possible_edges  = 0;   // nrow * ncol
    int64_t edges_evaluated = 0;   // pairs a cost was computed for, all rounds

    // Why no arc set over this source admits a complete matching, and the
    // witness re-checked against the full source. Populated exactly when the
    // status is "infeasible".
    DeficiencySet witness;
    bool witness_certified = false;
};

namespace implicit_detail {

using Clock = std::chrono::steady_clock;

inline double seconds_since(const Clock::time_point& t0) {
    return std::chrono::duration<double>(Clock::now() - t0).count();
}

// The pricing round read as a scan of the omitted pairs, which is the half of
// the complete scan the certificate cannot take for itself.
inline ReducedCostScan omitted_scan(const BlockPricing& priced) {
    ReducedCostScan scan;
    scan.min_reduced_cost = priced.min_reduced_cost;
    scan.arg_i = priced.arg_i;
    scan.arg_j = priced.arg_j;
    scan.n_violations = priced.n_violators;
    scan.n_admissible = priced.n_evaluated;
    return scan;
}

// Whether any matched pair prices strictly below zero against `u` and `v`,
// which is the state tighten_matched_duals() exists to leave behind.
template <class Source>
double matched_slack(const Source& src, const std::vector<int>& match,
                     const std::vector<double>& u, const std::vector<double>& v) {
    double worst = 0.0;
    for (std::size_t i = 0; i < match.size(); ++i) {
        const int j = match[i];
        if (j < 0) continue;
        double c = 0.0;
        if (!cost_if_allowed(src, static_cast<int64_t>(i), static_cast<int64_t>(j), c)) {
            continue;
        }
        const double cbar = c - u[i] - v[static_cast<std::size_t>(j)];
        if (std::abs(cbar) > worst) worst = std::abs(cbar);
    }
    return worst;
}

template <class Source>
void require_shape(const Source& src, const FlowProblem& prob,
                   const CandidateSet& cand, const ImplicitOptions& opts) {
    if (src.nrow <= 0 || src.ncol <= 0) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: source is " +
                            std::to_string(src.nrow) + " x " + std::to_string(src.ncol) +
                            ", which names no pair to generate");
    }
    if (prob.blocks.size() != 1u) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: " +
                            std::to_string(prob.blocks.size()) +
                            " bipartite blocks, and the loop prices one");
    }
    const BipartiteBlock& blk = prob.blocks[0];
    if (blk.costs == nullptr) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: the block has no cost source");
    }
    if (blk.costs->nrow() != src.nrow || blk.costs->ncol() != src.ncol) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: the block is " +
                            std::to_string(blk.costs->nrow()) + " x " +
                            std::to_string(blk.costs->ncol()) + ", the source is " +
                            std::to_string(src.nrow) + " x " + std::to_string(src.ncol));
    }
    if (blk.lower != 0 || blk.upper != 1) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: block arcs are bounded [" +
                            std::to_string(blk.lower) + ", " + std::to_string(blk.upper) +
                            "], and an assignment's pairs carry at most one unit");
    }
    if (prob.expanded) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: the problem is already "
                            "expanded, so its arc set is not the candidate set's");
    }
    if (cand.nrow() != src.nrow || cand.ncol() != src.ncol) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: candidate set is " +
                            std::to_string(cand.nrow()) + " x " +
                            std::to_string(cand.ncol()) + ", source is " +
                            std::to_string(src.nrow) + " x " + std::to_string(src.ncol));
    }
    // Hall's condition answers feasibility for a row-perfect matching, so the
    // required flow has to be one unit per row. A design asking for fewer is a
    // different feasibility question and is not this loop's. The units are
    // counted rather than read off one node, because a compiler is free to
    // inject them at the auxiliary source or at the row nodes themselves.
    int64_t injected = 0;
    for (int64_t s : prob.supply) {
        if (s > 0) injected += s;
    }
    if (injected != src.nrow) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: the problem injects " +
                            std::to_string(injected) + " units for " +
                            std::to_string(src.nrow) + " rows, and the loop matches "
                            "every row");
    }
    if (opts.keep_per_row < 1) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: keep_per_row " +
                            std::to_string(opts.keep_per_row) +
                            " adds no pair, so a pricing round cannot change the "
                            "master it priced");
    }
    if (opts.width < 1) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: width " +
                            std::to_string(opts.width) +
                            " adds no column to a deficient row");
    }
    if (opts.max_rounds < 1) {
        LAP_THROW_DIMENSION("solve_implicit_assignment: max_rounds " +
                            std::to_string(opts.max_rounds) + " runs no round");
    }
}

}  // namespace implicit_detail

// Put every matched pair back on the tight face: u_i := c_i,match(i) - v_match(i).
//
// The flow model gives a block arc the upper bound of one unit that the row's
// own supply already implies, and the assignment LP has no such bound -- its
// dual asks u_i + v_j <= c_ij on every admissible pair, matched pairs included.
// A min-cost flow is optimal with an arc at its upper bound priced strictly
// below zero, so the two agree on the primal and not on the duals, and a warm
// start reaches that state on purpose: the slackness repair pushes a newly
// added arc to its upper bound, and an augmentation that never touches it again
// leaves it there. Solved cold, every matched arc entered the flow on a
// shortest path at a reduced cost of zero and the question does not arise.
//
// A matched arc prices at or below zero, so this only ever lowers u_i, and
// lowering u_i raises every reduced cost in row i: a dual point that was
// feasible stays feasible, the matched pairs become tight, and the dual
// objective meets the primal rather than overshooting it. That is a projection
// onto the duals the LP asks for, not a repair of a wrong answer -- an
// unmatched row and a pair the source forbids are left alone, and what they
// leave behind is what certify_assignment() then refuses.
template <class Source>
void tighten_matched_duals(const Source& src, const std::vector<int>& match,
                           std::vector<double>& u, const std::vector<double>& v) {
    for (std::size_t i = 0; i < match.size(); ++i) {
        const int j = match[i];
        if (j < 0) continue;
        double c = 0.0;
        if (!cost_if_allowed(src, static_cast<int64_t>(i), static_cast<int64_t>(j), c)) {
            continue;
        }
        u[i] = c - v[static_cast<std::size_t>(j)];
    }
}

// Solve the assignment over the complete implicit problem `src` describes, by
// growing `cand` until the pairs it holds carry an optimal solution for all of
// them.
//
// `prob` is a compiled, unexpanded problem holding one unit-capacity bipartite
// block backed by `src`, with one unit of supply per row. It is expanded over
// the candidate set here, and it is left holding the last master solved -- its
// arcs, its flow and its potentials -- so a caller can read the solution back
// through the block's own (i, j) metadata.
//
// `cand` carries whatever seed the caller has. An empty set is a valid start:
// the first master places nothing, and the feasibility phase seeds every row
// with its `width` cheapest admissible columns. That seed is a scan of the
// whole source, which is the cost a metric tree exists to remove.
template <class Source>
ImplicitResult solve_implicit_assignment(const Source& src,
                                         FlowProblem& prob,
                                         CandidateSet& cand,
                                         const ImplicitOptions& opts = ImplicitOptions()) {
    using implicit_detail::Clock;
    using implicit_detail::seconds_since;

    implicit_detail::require_shape(src, prob, cand, opts);

    ImplicitResult out;
    out.possible_edges = src.nrow * src.ncol;

    FlowOptions flow_opts = opts.flow;
    flow_opts.return_potentials = true;

    expand_block_subset(prob, 0, cand);

    // The ladder, over the deficient rows alone. A feasibility round scans
    // every column of every row it is called for whatever width it keeps, so a
    // deficiency that survives a round is cheaper to attack wider than to
    // attack again at the same width.
    int width = opts.width;

    bool decided = false;
    for (int64_t round = 1; round <= opts.max_rounds; ++round) {
        ImplicitRound rec;
        rec.round = round;
        rec.candidate_pairs = cand.n_arcs();
        rec.block_arcs = prob.block_arcs[0].n_arcs;

        const Clock::time_point t_master = Clock::now();
        const FlowResult master = solve_min_cost_flow(prob, flow_opts);
        rec.master_seconds = seconds_since(t_master);
        rec.master_status  = master.status;
        rec.flow_sent      = master.flow_sent;
        rec.flow_required  = master.flow_required;
        rec.master_cost    = master.total_cost;

        // A master that ran out of augmentations has not answered the question,
        // so nothing downstream can be read off it.
        if (master.status == "iteration_limit") {
            rec.kind = ImplicitRound::Kind::priced;
            out.rounds.push_back(rec);
            out.status = master.status;
            decided = true;
            break;
        }

        if (master.flow_sent < master.flow_required) {
            rec.kind = ImplicitRound::Kind::reseeded;

            const Clock::time_point t_seed = Clock::now();
            FeasibilityRound seeded = feasibility_round(src, cand, width);
            rec.pricing_seconds = seconds_since(t_seed);
            rec.n_evaluated = seeded.n_evaluated;

            if (seeded.status == FeasibilityRound::Status::infeasible) {
                out.rounds.push_back(rec);
                out.status = "infeasible";
                out.witness = std::move(seeded.witness);
                out.witness_certified = seeded.certified;
                decided = true;
                break;
            }
            if (seeded.status == FeasibilityRound::Status::feasible) {
                // Hall says the restricted arcs admit a row-perfect matching
                // and the master placed fewer units than it has rows. Both read
                // the same arc set, so one of them is wrong about it, and that
                // is not a statement about the candidate set to carry on from.
                LAP_THROW("solve_implicit_assignment: the master placed " +
                          std::to_string(master.flow_sent) + " of " +
                          std::to_string(master.flow_required) +
                          " units over an arc set Hall's condition calls feasible");
            }

            prob.warm_flow      = master.flow;
            prob.warm_potential = master.potential;
            rec.pairs_added = static_cast<int64_t>(seeded.added.size());
            rec.arcs_added  = add_block_arcs(prob, 0, seeded.added);
            out.rounds.push_back(rec);
            width = static_cast<int>(
                std::min<int64_t>(static_cast<int64_t>(width) * 2, src.ncol));
            continue;
        }

        // The master is feasible, so its potentials are duals to price with,
        // once they are on the face the assignment LP asks for.
        AssignmentDuals duals = map_assignment_duals(
            prob, layout_of(prob, 0), master.flow, master.potential,
            AssignmentEquality::Rows);
        if (!duals.ok()) {
            LAP_THROW("solve_implicit_assignment: the master placed its required "
                      "flow over unit-capacity arcs and the flow does not read "
                      "back as an assignment");
        }
        rec.matched_slack =
            implicit_detail::matched_slack(src, duals.match, duals.u, duals.v);
        tighten_matched_duals(src, duals.match, duals.u, duals.v);

        const Clock::time_point t_price = Clock::now();
        const BlockPricing priced =
            price_block(src, duals.u, duals.v, cand, opts.keep_per_row, opts.tol);
        rec.pricing_seconds  = seconds_since(t_price);
        rec.min_reduced_cost = priced.min_reduced_cost;
        rec.n_violators      = priced.n_violators;
        rec.n_evaluated      = priced.n_evaluated;

        if (priced.n_violators == 0) {
            // Nothing the master omits prices below -tol, so the duals are
            // feasible for every pair of the complete problem. The certificate
            // is that scan and a scan over the master's own pairs, which costs
            // the candidates rather than the grid.
            if (opts.certify) {
                const ReducedCostScan held = scan_reduced_costs(
                    CandidateGraph<Source>(src, cand), duals.u, duals.v, opts.tol);
                cand.note_evaluated(held.n_admissible);
                rec.n_evaluated += held.n_admissible;

                out.certificate = certify_assignment(
                    src, duals.match, duals.u, duals.v, opts.tol,
                    merge_scans(held, implicit_detail::omitted_scan(priced)));
                out.certified = out.certificate.certified_optimal;
            }
            out.status = master.status;
            out.match = duals.match;
            out.u = duals.u;
            out.v = duals.v;
            out.total_cost = master.total_cost;
            out.rounds.push_back(rec);
            decided = true;
            break;
        }

        prob.warm_flow      = master.flow;
        prob.warm_potential = master.potential;
        const std::vector<CandidateSet::Pair> added =
            cand.add_pairs(violator_pairs(priced.violators));
        rec.pairs_added = static_cast<int64_t>(added.size());
        rec.arcs_added  = add_block_arcs(prob, 0, added);
        out.rounds.push_back(rec);
    }

    if (!decided) out.status = "iteration_limit";

    out.candidate_edges = cand.n_arcs();
    out.edges_evaluated = cand.edges_evaluated();
    return out;
}

}  // namespace lap
