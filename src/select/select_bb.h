// src/select/select_bb.h
// Branch and bound over 0/1 unit-selection programmes, with lazy rows.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The search minimizes c'x over the integer points of a few exactly held rows.
// Every node solves its linear relaxation with the dual simplex of
// select_lp.h, warm from its parent's basis, and reads a lower bound from the
// duals by weak duality (select_bound.h), so a node is pruned on a number that
// bounds it whatever the simplex's accuracy was. The incumbent is held exactly.
//
// Some rows are not known in advance. Matchability of a selection is a Hall
// condition with exponentially many inequalities, and the distance a selection
// costs is the value of an assignment problem over it; both are stated through
// an oracle that is shown every integral relaxed optimum and answers with the
// rows that point violates, the exact value of the objective there, or both.
// Rows the oracle returns are valid for every integer point, so they are added
// to the problem itself and every open node inherits them.
//
// Columns are branched on by pseudocost, and a column whose pseudocost has not
// been seen often enough is strong-branched first. A column whose move to its
// other bound would lift the bound past the incumbent is fixed where it is
// (reduced-cost fixing): at the root for the whole search, which restarts the
// root relaxation over the smaller problem, and elsewhere for the subtree.
//
// A node is closed when its bound reaches the incumbent. With an integral
// objective the comparison is against the incumbent less one, since no integer
// point strictly between them exists. A proxy objective - a continuous column
// theta bounded below by cuts - is compared exactly, and at an integral point
// whose own cut is tight a dual vector satisfying complementary slackness
// exactly is built from the simplex's, which proves the bound equal to the
// value at that point with no tolerance.
#pragma once

#include "../core/lap_exact.h"
#include "select_bound.h"
#include "select_local.h"
#include "select_lp.h"

#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <queue>
#include <utility>
#include <vector>

namespace lap {
namespace select {

struct SelectProblem {
    std::vector<double> c;
    std::vector<double> lower;
    std::vector<double> upper;
    std::vector<uint8_t> integer;
    // A continuous column whose value at an integer point is what the oracle
    // reports, the objective being that column alone; -1 when there is none.
    int64_t theta = -1;
    // Every integer point has an integer objective value.
    bool integral_objective = false;
    // Shift the simplex's costs off their ties (see DualSimplex::perturb).
    bool perturb = false;
    ExactRows rows;

    explicit SelectProblem(int64_t n) : rows(n) {}
    int64_t n() const { return rows.n(); }
};

struct Verdict {
    bool feasible = false;
    bool has_value = false;   // otherwise the value is c'x
    Expansion value;
    std::vector<RowSpec> rows;
};

using Oracle = std::function<Verdict(const std::vector<double>& x)>;

// Shown a fractional relaxed optimum, returns rows it violates; an empty
// answer means none worth adding. Rows must be valid for every integer point.
using Separator = std::function<std::vector<RowSpec>(const std::vector<double>& x, int64_t depth)>;

struct Progress {
    int64_t n_nodes = 0;
    double elapsed = 0.0;
    double incumbent = kInf;
    double bound = -kInf;
};

struct Limits {
    int64_t node_limit = std::numeric_limits<int64_t>::max();
    double time_limit = kInf;
    int64_t lp_iteration_limit = 1000000;
    // Asked before every node; true stops the search as an interrupt would.
    std::function<bool(const Progress&)> stop;
};

enum class Stop { optimality, node_limit, time_limit, interrupt };

struct SearchResult {
    Stop stopped = Stop::optimality;
    bool has_incumbent = false;
    std::vector<double> x;
    Expansion value;
    double bound = -kInf;            // a lower bound on every integer point
    bool bound_certified = true;     // no node was dropped without a proof
    int64_t n_nodes = 0;
    int64_t lp_iterations = 0;
    int64_t rows_added = 0;
};

namespace detail {

struct Fix {
    int32_t var;
    uint8_t value;
};

struct Node {
    int64_t id = 0;
    int64_t depth = 0;
    double bound = -kInf;
    std::vector<Fix> fixes;
    std::shared_ptr<const Basis> basis;
    // The branch that made this node: the column, the direction, how far the
    // parent's value sat from the side this child took, and the parent's
    // relaxed objective, from which the node learns its column's pseudocost.
    int64_t branch_var = -1;
    uint8_t branch_up = 0;
    double branch_frac = 0.0;
    double parent_objective = 0.0;
};

struct NodeOrder {
    bool operator()(const std::shared_ptr<Node>& a, const std::shared_ptr<Node>& b) const {
        if (a->bound != b->bound) return a->bound > b->bound;
        if (a->depth != b->depth) return a->depth < b->depth;
        return a->id > b->id;
    }
};

inline bool is_integral(double v) { return std::fabs(v - std::round(v)) <= 1e-9; }

}  // namespace detail

class BranchAndBound {
public:
    BranchAndBound(SelectProblem& prob, Oracle oracle, Limits limits, Separator separate = nullptr)
        : prob_(prob), oracle_(std::move(oracle)), separate_(std::move(separate)), limits_(std::move(limits)),
          root_lower_(prob.lower), root_upper_(prob.upper),
          lower_(prob.lower), upper_(prob.upper),
          lp_(prob.c, prob.rows.dense(), prob.lower, prob.upper) {
        if (prob.perturb) lp_.perturb(1e-7);
        for (int d = 0; d < 2; ++d) {
            pc_sum_[d].assign(static_cast<std::size_t>(prob.n()), 0.0);
            pc_count_[d].assign(static_cast<std::size_t>(prob.n()), 0);
        }
    }

    void set_incumbent(const std::vector<double>& x, const Expansion& value) {
        result_.has_incumbent = true;
        result_.x = x;
        result_.value = value;
    }

    SearchResult run() {
        started_ = std::chrono::steady_clock::now();
        auto root = std::make_shared<detail::Node>();
        root->id = next_id_++;
        open_.push(root);
        result_.stopped = Stop::optimality;

        while (!open_.empty()) {
            if (stop_now()) break;
            if (prunes(open_.top()->bound)) {
                // No open node can reach below the incumbent.
                while (!open_.empty()) open_.pop();
                break;
            }
            std::shared_ptr<detail::Node> node = open_.top();
            open_.pop();
            // Plunge: the preferred child is opened next, the other waits.
            while (node) {
                active_ = node;
                std::shared_ptr<detail::Node> next = process(*node);
                active_.reset();
                node = next;
                if (node && stop_now()) {
                    open_.push(node);
                    break;
                }
            }
            if (stopping_) break;
        }

        double bound = kInf;
        if (result_.has_incumbent) bound = exact::round_down(result_.value);
        while (!open_.empty()) {
            bound = std::min(bound, open_.top()->bound);
            open_.pop();
        }
        result_.bound = std::min(bound, unresolved_bound_);
        result_.lp_iterations = lp_.iterations();
        return result_;
    }

private:
    SelectProblem& prob_;
    Oracle oracle_;
    Separator separate_;
    Limits limits_;
    std::vector<double> root_lower_;
    std::vector<double> root_upper_;
    std::vector<double> lower_;
    std::vector<double> upper_;
    DualSimplex lp_;
    std::priority_queue<std::shared_ptr<detail::Node>, std::vector<std::shared_ptr<detail::Node>>,
                        detail::NodeOrder> open_;
    std::shared_ptr<detail::Node> active_;
    SearchResult result_;
    int64_t next_id_ = 0;
    bool stopping_ = false;
    bool improved_ = false;
    double unresolved_bound_ = kInf;
    std::chrono::steady_clock::time_point started_;

    double elapsed() const {
        return std::chrono::duration<double>(std::chrono::steady_clock::now() - started_).count();
    }

    bool stop_now() {
        if (stopping_) return true;
        if (result_.n_nodes >= limits_.node_limit) {
            result_.stopped = Stop::node_limit;
            stopping_ = true;
        } else if (elapsed() > limits_.time_limit) {
            result_.stopped = Stop::time_limit;
            stopping_ = true;
        } else if (limits_.stop) {
            Progress p;
            p.n_nodes = result_.n_nodes;
            p.elapsed = elapsed();
            p.incumbent = result_.has_incumbent ? exact::approximate(result_.value).value : kInf;
            p.bound = open_.empty() ? p.incumbent : open_.top()->bound;
            if (limits_.stop(p)) {
                result_.stopped = Stop::interrupt;
                stopping_ = true;
            }
        }
        return stopping_;
    }

    // Whether a node whose bound is `lb` can hold nothing below the incumbent.
    bool prunes(double lb) const {
        if (!result_.has_incumbent) return false;
        if (lb == kInf) return true;
        if (prob_.integral_objective) {
            // value is an integer; anything above value - 1 rounds up to it.
            const double v = exact::round_down(result_.value);
            return lb > v - 1.0;
        }
        return exact::compare(result_.value, lb) <= 0;
    }

    // A positive lower bound for the zero objective contradicts feasibility.
    // Both signs of the row weights are checked against the exact constraints
    // and the current column bounds.
    bool proves_infeasible(std::vector<double> y) const {
        if (static_cast<int64_t>(y.size()) != prob_.rows.m()) return false;
        for (double yr : y) if (!std::isfinite(yr)) return false;
        const std::vector<double> zero(static_cast<std::size_t>(prob_.n()), 0.0);
        const Box box{&lower_, &upper_};
        for (int direction = 0; direction < 2; ++direction) {
            bool finite = true;
            const Expansion bound = exact_bound(zero, prob_.rows, box, y, finite);
            if (finite && exact::sign(bound) > 0) return true;
            for (double& yr : y) yr = -yr;
        }
        return false;
    }

    // Reduced-cost fixing. A column whose move to its other end raises the
    // node's bound past the incumbent holds that end in no better point of
    // the subtree, so it is fixed where its term was read. At the root, with
    // no branching decision behind the bound, the fix holds for the whole
    // search; elsewhere it holds for the node and its subtree.
    int64_t fix_by_reduced_cost(detail::Node& node, const RigorousBound& rb) {
        const bool global = is_root(node);
        if (global) root_rb_ = rb;
        if (!result_.has_incumbent || rb.rise.empty()) return 0;
        int64_t fixed = 0;
        for (int64_t j = 0; j < prob_.n(); ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            if (!prob_.integer[sj] || lower_[sj] == upper_[sj] || rb.end[sj] < 0) continue;
            if (!prunes(rb.value + rb.rise[sj])) continue;
            const double keep = rb.end[sj] == 0 ? lower_[sj] : upper_[sj];
            lower_[sj] = keep;
            upper_[sj] = keep;
            lp_.set_bounds(j, keep, keep);
            ++fixed;
            if (global) {
                root_lower_[sj] = keep;
                root_upper_[sj] = keep;
            } else {
                node.fixes.push_back({static_cast<int32_t>(j), static_cast<uint8_t>(keep)});
            }
        }
        return fixed;
    }

    static bool is_root(const detail::Node& node) { return node.fixes.empty() && node.depth == 0; }

    // The root's bound, kept so that every later incumbent fixes the columns
    // it now proves, for the whole search: the bound was read over the root's
    // box, and rows added since only add to what it bounds.
    RigorousBound root_rb_;

    void fix_from_root() {
        if (!result_.has_incumbent || root_rb_.rise.empty()) return;
        for (int64_t j = 0; j < prob_.n(); ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            if (!prob_.integer[sj] || root_lower_[sj] == root_upper_[sj] || root_rb_.end[sj] < 0) continue;
            if (!prunes(root_rb_.value + root_rb_.rise[sj])) continue;
            const double keep = root_rb_.end[sj] == 0 ? root_lower_[sj] : root_upper_[sj];
            root_lower_[sj] = keep;
            root_upper_[sj] = keep;
        }
    }

    // Whether a node's own fixes agree with the bounds fixed for the whole
    // search since it was made; one that does not holds nothing.
    bool consistent(const detail::Node& node) const {
        for (const detail::Fix& f : node.fixes) {
            const std::size_t sj = static_cast<std::size_t>(f.var);
            if (f.value < root_lower_[sj] || f.value > root_upper_[sj]) return false;
        }
        return true;
    }

    void apply_bounds(const detail::Node& node) {
        for (int64_t j = 0; j < prob_.n(); ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            if (lower_[sj] != root_lower_[sj] || upper_[sj] != root_upper_[sj]) {
                lower_[sj] = root_lower_[sj];
                upper_[sj] = root_upper_[sj];
                lp_.set_bounds(j, lower_[sj], upper_[sj]);
            }
        }
        for (const detail::Fix& f : node.fixes) {
            const std::size_t sj = static_cast<std::size_t>(f.var);
            lower_[sj] = f.value;
            upper_[sj] = f.value;
            lp_.set_bounds(f.var, lower_[sj], upper_[sj]);
        }
    }

    // The candidate point: integer columns rounded, continuous ones as solved.
    std::vector<double> candidate() const {
        std::vector<double> x(lp_.x().begin(), lp_.x().begin() + prob_.n());
        for (int64_t j = 0; j < prob_.n(); ++j) {
            if (prob_.integer[static_cast<std::size_t>(j)]) {
                x[static_cast<std::size_t>(j)] = std::round(x[static_cast<std::size_t>(j)]);
            }
        }
        return x;
    }

    std::vector<int64_t> support(const std::vector<double>& x, bool integers_only) const {
        std::vector<int64_t> out;
        for (int64_t j = 0; j < prob_.n(); ++j) {
            if (integers_only && !prob_.integer[static_cast<std::size_t>(j)]) continue;
            if (x[static_cast<std::size_t>(j)] != 0.0) out.push_back(j);
        }
        return out;
    }

    // Index of the first hard row the point violates, exactly, or -1.
    int64_t violated_hard_row(const std::vector<double>& x) const {
        const std::vector<int64_t> sup = support(x, true);
        for (int64_t r = 0; r < prob_.rows.m(); ++r) {
            if (!prob_.rows.hard(r)) continue;
            const Expansion act = prob_.rows.activity(r, sup, x);
            if (std::isfinite(prob_.rows.lo(r)) && exact::compare(act, prob_.rows.lo(r)) < 0) return r;
            if (std::isfinite(prob_.rows.hi(r)) && exact::compare(act, prob_.rows.hi(r)) > 0) return r;
        }
        return -1;
    }

    Expansion objective_at(const std::vector<double>& x) const {
        Expansion acc;
        Expansion next;
        for (int64_t j = 0; j < prob_.n(); ++j) {
            const double xj = x[static_cast<std::size_t>(j)];
            const double cj = prob_.c[static_cast<std::size_t>(j)];
            if (xj == 0.0 || cj == 0.0) continue;
            for (double comp : exact::product(cj, xj)) {
                exact::grow_expansion(acc, comp, next);
                acc.swap(next);
            }
            if (acc.size() > 16) exact::compress(acc);
        }
        exact::compress(acc);
        return acc;
    }

    // A dual vector that satisfies complementary slackness exactly at an
    // integer point x whose proxy objective theta takes `value`: every row not
    // tight at x is dropped. One tight row that bounds theta keeps the exact
    // reciprocal of its power-of-two theta coefficient, so theta's reduced
    // cost is exactly zero.
    std::vector<double> polished_duals(const std::vector<double>& x, const Expansion& value) const {
        std::vector<double> y = lp_.y();
        const int64_t theta = prob_.theta;
        const std::vector<int64_t> sup = support(x, true);
        int64_t keep = -1;
        double keep_dual = 0.0;
        double keep_weight = 0.0;
        for (int64_t r = 0; r < prob_.rows.m(); ++r) {
            double& yr = y[static_cast<std::size_t>(r)];
            const bool on_theta = theta >= 0 && prob_.rows.approx(r, theta) != 0.0;
            Expansion act = prob_.rows.activity(r, sup, x);
            double theta_coef = 0.0;
            if (on_theta) {
                const Expansion coef = prob_.rows.entry(r, theta);
                int exponent = 0;
                if (coef.size() != 1 || coef[0] <= 0.0 ||
                    std::frexp(coef[0], &exponent) != 0.5 ||
                    !std::isfinite(1.0 / coef[0])) {
                    yr = 0.0;
                    continue;
                }
                theta_coef = coef[0];
                act = exact::expansion_sum(act, exact::scale_expansion(value, theta_coef));
            }
            const bool tight_lo = std::isfinite(prob_.rows.lo(r)) && exact::compare(act, prob_.rows.lo(r)) == 0;
            const bool tight_hi = std::isfinite(prob_.rows.hi(r)) && exact::compare(act, prob_.rows.hi(r)) == 0;
            if (!(tight_lo && yr > 0.0) && !(tight_hi && yr < 0.0)) yr = 0.0;
            if (on_theta) {
                const double influence = yr * theta_coef;
                if (influence > keep_dual) {
                    keep_dual = influence;
                    keep_weight = 1.0 / theta_coef;
                    keep = r;
                }
                yr = 0.0;
            }
        }
        if (keep >= 0) y[static_cast<std::size_t>(keep)] = keep_weight;
        return y;
    }

    // Whether the node's bound, read exactly at polished duals, reaches the
    // incumbent. When it does not, `against` names the integer columns whose
    // exact reduced cost points away from x: degenerate basic columns whose
    // computed duals miss zero by a rounding, which a branch fixing them at
    // x's value removes from the bound.
    bool closes_exactly(const std::vector<double>& x, const Expansion& value,
                        std::vector<int64_t>& against) const {
        const std::vector<double> y = polished_duals(x, value);
        bool finite = true;
        const Box box{&lower_, &upper_};
        against.clear();
        const Expansion lb = exact_bound(prob_.c, prob_.rows, box, y, finite, &x, &against);
        return finite && result_.has_incumbent && exact::compare(lb, result_.value) >= 0;
    }

    void consider(const std::vector<double>& x, const Expansion& value) {
        if (!result_.has_incumbent || exact::compare(value, result_.value) < 0) {
            result_.has_incumbent = true;
            result_.x = x;
            result_.value = value;
            improved_ = true;
            fix_from_root();
        }
    }

    // The swap search runs at the root, from every new incumbent, and every
    // kHeuristicEvery nodes from the node's relaxed point. It moves integer
    // columns only and measures the objective by their costs, so a proxy
    // objective, whose value no move can see, leaves it nothing to improve.
    static constexpr int64_t kHeuristicEvery = 64;

    bool heuristic_due() const {
        return result_.n_nodes == 1 || improved_ || result_.n_nodes % kHeuristicEvery == 0;
    }

    void search_from(const std::vector<double>& relaxed) {
        std::vector<double> start;
        if (improved_ && result_.has_incumbent) {
            start = result_.x;
        } else {
            start.assign(relaxed.begin(), relaxed.begin() + prob_.n());
            for (int64_t j = 0; j < prob_.n(); ++j) {
                const std::size_t sj = static_cast<std::size_t>(j);
                if (prob_.integer[sj]) start[sj] = start[sj] >= 0.5 ? 1.0 : 0.0;
            }
        }
        improved_ = false;
        // A proxy objective is invisible to the columns it depends on, so the
        // search lowers the relaxation's reduced costs instead: each column is
        // priced by the binding cuts at their dual weights, the linear picture
        // of the objective the relaxation itself optimizes.
        std::vector<double> price = prob_.c;
        if (prob_.theta >= 0) {
            price.assign(lp_.reduced_cost().begin(), lp_.reduced_cost().begin() + prob_.n());
        }
        SwapSearch ls(price, prob_.rows, prob_.integer, lower_, upper_);
        if (!ls.equations_hold(start) && !ls.rounded_start(relaxed, start)) {
            // No rounding of the relaxed point keeps every count; the
            // incumbent does, and the prices then pull it toward the
            // relaxation.
            if (!result_.has_incumbent) return;
            start = result_.x;
            for (int64_t j = 0; j < prob_.n(); ++j) {
                const std::size_t sj = static_cast<std::size_t>(j);
                if (prob_.integer[sj] && lower_[sj] == upper_[sj] && start[sj] != lower_[sj]) return;
            }
        }
        if (!ls.run(start, kSwapMoves)) return;
        offer(start);
    }

    static constexpr int64_t kSwapMoves = 4000;

    // A point found off the relaxation: checked exactly against the hard rows
    // and shown to the oracle like any integral relaxed optimum. Rows the
    // oracle returns are kept, since they are valid everywhere, and the point
    // is then not a candidate.
    void offer(const std::vector<double>& x) {
        if (violated_hard_row(x) >= 0) return;
        Verdict verdict;
        if (oracle_) verdict = oracle_(x);
        else verdict.feasible = true;
        if (!verdict.rows.empty()) {
            for (const RowSpec& row : verdict.rows) prob_.rows.add(row);
            result_.rows_added += static_cast<int64_t>(verdict.rows.size());
            lp_.sync_rows();
        }
        if (verdict.feasible) {
            const Expansion value = verdict.has_value ? verdict.value : objective_at(x);
            consider(x, value);
        }
    }

    std::shared_ptr<detail::Node> child(const detail::Node& parent, int64_t var, uint8_t value,
                                        double bound, const std::shared_ptr<const Basis>& basis) {
        auto kid = std::make_shared<detail::Node>();
        kid->id = next_id_++;
        kid->depth = parent.depth + 1;
        kid->bound = bound;
        kid->fixes = parent.fixes;
        kid->fixes.push_back({static_cast<int32_t>(var), value});
        kid->basis = basis;
        const double v = lp_.x()[static_cast<std::size_t>(var)];
        kid->branch_var = var;
        kid->branch_up = value;
        kid->branch_frac = value ? std::ceil(v) - v : v - std::floor(v);
        kid->parent_objective = lp_.objective();
        return kid;
    }

    // Pseudocosts: per column and direction, the mean gain in the relaxed
    // objective per unit of fractionality its branches have shown.
    std::vector<double> pc_sum_[2];
    std::vector<int64_t> pc_count_[2];

    void learn(const detail::Node& node) {
        if (node.branch_var < 0 || node.branch_frac <= 1e-9) return;
        const std::size_t d = node.branch_up;
        const double gain = std::max(0.0, lp_.objective() - node.parent_objective);
        pc_sum_[d][static_cast<std::size_t>(node.branch_var)] += gain / node.branch_frac;
        pc_count_[d][static_cast<std::size_t>(node.branch_var)] += 1;
    }

    // Reliability branching. A column whose pseudocost has been seen fewer
    // than kReliable times in either direction is strong-branched: both of
    // its children are solved for a few dual simplex iterations from this
    // node's basis, and what the bound did there is what its pseudocost
    // learns. A child whose bound, read rigorously from its own duals, already
    // reaches the incumbent holds nothing better, so the column is fixed to
    // the other side in this node itself, and a node both of whose children
    // are empty is empty.
    static constexpr int64_t kReliable = 4;
    static constexpr int64_t kStrongCandidates = 8;
    static constexpr int64_t kStrongIterations = 64;
    static constexpr int64_t kNodeEmpty = -2;
    static constexpr int64_t kNodeTightened = -3;

    int64_t strong_branch(detail::Node& node, int64_t fallback) {
        struct Cand {
            double score;
            int64_t j;
        };
        std::vector<Cand> cands;
        const std::vector<double> x0(lp_.x().begin(), lp_.x().begin() + prob_.n());
        for (int64_t j = 0; j < prob_.n(); ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            if (!prob_.integer[sj] || detail::is_integral(x0[sj])) continue;
            const double f_down = x0[sj] - std::floor(x0[sj]);
            const double f_up = std::ceil(x0[sj]) - x0[sj];
            cands.push_back({std::max(1e-6, pseudocost(j, 0) * f_down) *
                                 std::max(1e-6, pseudocost(j, 1) * f_up), j});
        }
        std::sort(cands.begin(), cands.end(), [](const Cand& a, const Cand& b) {
            if (a.score != b.score) return a.score > b.score;
            return a.j < b.j;
        });
        if (cands.size() > static_cast<std::size_t>(kStrongCandidates)) {
            cands.resize(static_cast<std::size_t>(kStrongCandidates));
        }
        bool any = false;
        for (const Cand& cd : cands) {
            const std::size_t sj = static_cast<std::size_t>(cd.j);
            if (pc_count_[0][sj] < kReliable || pc_count_[1][sj] < kReliable) any = true;
        }
        if (!any) return fallback;

        const Basis saved = lp_.basis();
        const double obj0 = lp_.objective();
        int64_t best_j = fallback;
        double best_score = -1.0;
        for (const Cand& cd : cands) {
            const int64_t j = cd.j;
            const std::size_t sj = static_cast<std::size_t>(j);
            if (pc_count_[0][sj] >= kReliable && pc_count_[1][sj] >= kReliable) {
                if (cd.score > best_score) {
                    best_score = cd.score;
                    best_j = j;
                }
                continue;
            }
            bool empty[2] = {false, false};
            double gain[2] = {0.0, 0.0};
            const double lo = lower_[sj];
            const double hi = upper_[sj];
            for (int d = 0; d < 2; ++d) {
                const double val = d ? std::ceil(x0[sj]) : std::floor(x0[sj]);
                lower_[sj] = val;
                upper_[sj] = val;
                lp_.set_bounds(j, val, val);
                const LpStatus st = lp_.solve(kStrongIterations);
                if (st == LpStatus::infeasible) {
                    empty[d] = proves_infeasible(lp_.infeasibility_duals());
                } else {
                    gain[d] = std::max(0.0, lp_.objective() - obj0);
                    if (st == LpStatus::optimal) {
                        const Box box{&lower_, &upper_};
                        if (prunes(rigorous_bound(prob_.c, prob_.rows, box, lp_.y()).value)) empty[d] = true;
                    }
                    const double f = d ? std::ceil(x0[sj]) - x0[sj] : x0[sj] - std::floor(x0[sj]);
                    if (f > 1e-9) {
                        pc_sum_[d][sj] += gain[d] / f;
                        pc_count_[d][sj] += 1;
                    }
                }
                lower_[sj] = lo;
                upper_[sj] = hi;
                lp_.set_bounds(j, lo, hi);
                lp_.load(saved);
            }
            if (empty[0] && empty[1]) return kNodeEmpty;
            if (empty[0] || empty[1]) {
                const uint8_t keep = empty[0] ? 1 : 0;
                node.fixes.push_back({static_cast<int32_t>(j), keep});
                lower_[sj] = keep;
                upper_[sj] = keep;
                lp_.set_bounds(j, keep, keep);
                return kNodeTightened;
            }
            const double score = std::max(1e-6, gain[0]) * std::max(1e-6, gain[1]);
            if (score > best_score) {
                best_score = score;
                best_j = j;
            }
        }
        return best_j;
    }

    double pseudocost(int64_t j, int d) const {
        const int64_t k = pc_count_[d][static_cast<std::size_t>(j)];
        if (k > 0) return pc_sum_[d][static_cast<std::size_t>(j)] / static_cast<double>(k);
        double sum = 0.0;
        int64_t cnt = 0;
        for (std::size_t i = 0; i < pc_count_[d].size(); ++i) {
            if (pc_count_[d][i] > 0) {
                sum += pc_sum_[d][i] / static_cast<double>(pc_count_[d][i]);
                ++cnt;
            }
        }
        return cnt ? sum / static_cast<double>(cnt) : 1.0;
    }

    std::shared_ptr<detail::Node> branch(const detail::Node& node, int64_t var, double lb,
                                         bool up_first) {
        const auto basis = std::make_shared<const Basis>(lp_.basis());
        auto down = child(node, var, 0, lb, basis);
        auto up = child(node, var, 1, lb, basis);
        if (up_first) {
            open_.push(down);
            return up;
        }
        open_.push(up);
        return down;
    }

    // An integer column the node leaves free, preferring the one whose
    // reduced cost is closest to zero: at an integral optimum it is the column
    // whose other value the relaxation is least sure about.
    int64_t free_integer(bool by_reduced_cost) const {
        int64_t best = -1;
        double best_score = kInf;
        for (int64_t j = 0; j < prob_.n(); ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            if (!prob_.integer[sj] || lower_[sj] == upper_[sj]) continue;
            const double score = by_reduced_cost ? std::fabs(lp_.reduced_cost()[sj]) : 0.0;
            if (score < best_score) {
                best_score = score;
                best = j;
            }
        }
        return best;
    }

    // With every selection variable fixed and theta the only free continuous
    // column, the oracle solves the entire leaf. A stalled floating-point LP
    // need not leave that leaf uncertified. Check the completed point against
    // every row exactly before accepting it, including the non-hard cuts.
    bool resolve_proxy_leaf() {
        if (prob_.theta < 0 || !oracle_) return false;
        for (int64_t j = 0; j < prob_.n(); ++j) {
            if (j != prob_.theta && lower_[j] != upper_[j]) return false;
        }
        std::vector<double> x = lower_;
        if (violated_hard_row(x) >= 0) return true;
        const Verdict verdict = oracle_(x);
        if (!verdict.feasible) return true;
        if (!verdict.has_value) return false;
        x[prob_.theta] = exact::round_up(verdict.value);
        if (x[prob_.theta] < lower_[prob_.theta] ||
            x[prob_.theta] > upper_[prob_.theta]) return false;
        const std::vector<int64_t> sup = support(x, false);
        for (int64_t r = 0; r < prob_.rows.m(); ++r) {
            const Expansion act = prob_.rows.activity(r, sup, x);
            if (std::isfinite(prob_.rows.lo(r)) &&
                exact::compare(act, prob_.rows.lo(r)) < 0) return false;
            if (std::isfinite(prob_.rows.hi(r)) &&
                exact::compare(act, prob_.rows.hi(r)) > 0) return false;
        }
        consider(x, verdict.value);
        return true;
    }

    std::shared_ptr<detail::Node> process(detail::Node& node) {
        ++result_.n_nodes;
        if (!consistent(node)) return nullptr;
        apply_bounds(node);
        if (node.basis) lp_.load(*node.basis);

        for (int64_t round = 0;; ++round) {
            const LpStatus status = lp_.solve(limits_.lp_iteration_limit);
            if (status == LpStatus::infeasible &&
                proves_infeasible(lp_.infeasibility_duals())) return nullptr;

            double lb = node.bound;
            if (status == LpStatus::optimal && round == 0) learn(node);
            if (status == LpStatus::optimal) {
                const Box box{&lower_, &upper_};
                const RigorousBound rb = rigorous_bound(prob_.c, prob_.rows, box, lp_.y());
                lb = std::max(lb, rb.value);
                // At the root a fix is a restart: the relaxation over the
                // smaller problem has new duals, which may prove more.
                if (!prunes(lb) && fix_by_reduced_cost(node, rb) > 0 && is_root(node) &&
                    round < kMaxRounds) {
                    continue;
                }
            }
            if (prunes(lb)) return nullptr;

            if (status != LpStatus::optimal) {
                // The relaxation did not finish: the node keeps its inherited
                // bound and is split on a free column.
                const int64_t j = free_integer(false);
                if (j < 0) {
                    if (resolve_proxy_leaf()) return nullptr;
                    result_.bound_certified = false;
                    unresolved_bound_ = std::min(unresolved_bound_, lb);
                    return nullptr;
                }
                return branch(node, j, lb, true);
            }

            if (status == LpStatus::optimal && heuristic_due()) {
                const bool had = result_.has_incumbent;
                const Expansion before = result_.value;
                search_from(lp_.x());
                if (prunes(lb)) return nullptr;
                const bool better = result_.has_incumbent &&
                                    (!had || exact::compare(result_.value, before) < 0);
                if (better && is_root(node) && round < kMaxRounds) {
                    // The new incumbent fixed columns for the whole search.
                    apply_bounds(node);
                    continue;
                }
            }

            // The fractional column whose two children promise the largest
            // product of bound gains, by pseudocost; ties to the lower index.
            int64_t frac = -1;
            double best = -1.0;
            for (int64_t j = 0; j < prob_.n(); ++j) {
                const std::size_t sj = static_cast<std::size_t>(j);
                if (!prob_.integer[sj]) continue;
                const double v = lp_.x()[sj];
                if (detail::is_integral(v)) continue;
                const double f_down = v - std::floor(v);
                const double f_up = std::ceil(v) - v;
                const double score = std::max(1e-6, pseudocost(j, 0) * f_down) *
                                     std::max(1e-6, pseudocost(j, 1) * f_up);
                if (score > best) {
                    best = score;
                    frac = j;
                }
            }
            if (frac >= 0 && separate_ && round < kMaxRounds) {
                const std::vector<RowSpec> cuts = separate_(lp_.x(), node.depth);
                if (!cuts.empty()) {
                    for (const RowSpec& row : cuts) prob_.rows.add(row);
                    result_.rows_added += static_cast<int64_t>(cuts.size());
                    lp_.sync_rows();
                    continue;
                }
            }
            if (frac >= 0) {
                const int64_t pick = strong_branch(node, frac);
                if (pick == kNodeEmpty) return nullptr;
                if (pick == kNodeTightened) continue;
                frac = pick;
            }
            if (frac >= 0) {
                const double v = lp_.x()[static_cast<std::size_t>(frac)];
                return branch(node, frac, lb, v - std::floor(v) >= 0.5);
            }

            // An integral relaxed optimum.
            const std::vector<double> x = candidate();
            const int64_t violated = violated_hard_row(x);
            if (violated >= 0) {
                std::vector<double> y(static_cast<std::size_t>(prob_.rows.m()), 0.0);
                y[static_cast<std::size_t>(violated)] = 1.0;
                if (proves_infeasible(std::move(y))) return nullptr;
                // The rounding of an almost feasible point left a hard row
                // violated exactly; the point is not a candidate, and the node
                // is split on a free column instead.
                const int64_t j = free_integer(true);
                if (j < 0) return nullptr;
                return branch(node, j, lb, lp_.x()[static_cast<std::size_t>(j)] >= 0.5);
            }

            Verdict verdict;
            if (oracle_) {
                verdict = oracle_(x);
            } else {
                verdict.feasible = true;
            }
            if (verdict.feasible) {
                const Expansion value = verdict.has_value ? verdict.value : objective_at(x);
                std::vector<double> stored = x;
                if (prob_.theta >= 0) {
                    stored[static_cast<std::size_t>(prob_.theta)] = exact::round_up(value);
                }
                consider(stored, value);
            }
            if (!verdict.rows.empty() && round < kMaxRounds) {
                for (const RowSpec& row : verdict.rows) prob_.rows.add(row);
                result_.rows_added += static_cast<int64_t>(verdict.rows.size());
                lp_.sync_rows();
                continue;
            }
            if (prunes(lb)) return nullptr;
            std::vector<int64_t> against;
            if (verdict.feasible && prob_.theta >= 0 && verdict.has_value &&
                closes_exactly(x, verdict.value, against)) {
                return nullptr;
            }
            // Nothing closes the node: its integer point is accounted for, and
            // the rest of its subtree is reached by fixing one more column,
            // one the exact bound named when it named any.
            int64_t j = -1;
            for (int64_t k : against) {
                if (prob_.integer[static_cast<std::size_t>(k)]) {
                    j = k;
                    break;
                }
            }
            if (j < 0) j = free_integer(true);
            if (j < 0) return nullptr;
            return branch(node, j, lb, x[static_cast<std::size_t>(j)] >= 0.5);
        }
    }

    // Rounds of rows one node may add before it is split instead: an oracle
    // whose rows the relaxation keeps satisfying to within its tolerance would
    // otherwise be asked about the same point forever.
    static constexpr int64_t kMaxRounds = 200;
};

}  // namespace select
}  // namespace lap
