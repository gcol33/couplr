// src/select/select_local.h
// Swap search for integer points of a unit-selection programme.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The rows of a selection problem come in two kinds. Equations hold counts
// equal - matched units per side, per exactly balanced cell - and their
// coefficients are the same for every column of one class: a left unit of one
// cell, a right unit of another. Inequalities bound sums with continuous
// coefficients, moment rows above all, and an integer point of the relaxation
// is rarely found by rounding, because the relaxed optimum sits exactly on
// several of them.
//
// What the equations allow is exchange. Replacing a selected column by an
// unselected one of the same class changes no equation, and adding two columns
// whose equation coefficients cancel - a left and a right unit of one cell -
// changes none either. The search below works with those two moves only, so
// every point it visits satisfies every equation, and it measures the
// inequalities by their total violation, each row scaled by its largest
// coefficient so that rows of different units weigh alike:
//
//   repair  exchanges within a class while that lowers the violation, and
//           drops a cancelling pair when no exchange does;
//   grow    adds the cancelling pair that improves the objective and keeps
//           the violation lowest, then repairs, and keeps the result only if
//           the repair reaches zero.
//
// Candidates are pre-ranked against the rows a move has to relieve, and only
// the best few of each class are paired, which keeps a move at the cost of a
// few thousand row evaluations rather than the square of the class size.
//
// The point returned is checked by its caller exactly; here rows count as
// satisfied with a margin of a few roundings to spare, so that the check does
// not fail on a row the doubles put at its bound.
#pragma once

#include "select_bound.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <map>
#include <numeric>
#include <utility>
#include <vector>

namespace lap {
namespace select {

class SwapSearch {
public:
    // `c` is the objective the search lowers: the programme's own costs, or
    // for a proxy objective that no column carries, the relaxation's reduced
    // costs, which price each column by the cuts the relaxation found binding.
    SwapSearch(const std::vector<double>& c, const ExactRows& rows,
               const std::vector<uint8_t>& integer, const std::vector<double>& lower,
               const std::vector<double>& upper)
        : c_(c), rows_(rows), lower_(lower), upper_(upper) {
        const int64_t n = rows.n();
        // Equations and hard inequalities.
        for (int64_t r = 0; r < rows.m(); ++r) {
            if (!rows.hard(r)) continue;
            if (rows.lo(r) == rows.hi(r)) eq_.push_back(r);
            else ineq_.push_back(r);
        }
        // Columns the search may move: integer, still free.
        std::map<std::vector<double>, int64_t> sig_class;
        cls_.assign(static_cast<std::size_t>(n), -1);
        for (int64_t j = 0; j < n; ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            if (!integer[sj] || lower[sj] == upper[sj] || lower[sj] != 0.0 || upper[sj] != 1.0) continue;
            std::vector<double> sig;
            sig.reserve(eq_.size());
            for (int64_t r : eq_) sig.push_back(rows.approx(r, j));
            auto it = sig_class.find(sig);
            int64_t k;
            if (it == sig_class.end()) {
                k = static_cast<int64_t>(members_.size());
                sig_class.emplace(sig, k);
                members_.emplace_back();
                signature_.push_back(sig);
            } else {
                k = it->second;
            }
            cls_[sj] = k;
            members_[static_cast<std::size_t>(k)].push_back(j);
        }
        // Classes whose signatures cancel pair up for growth.
        for (std::size_t a = 0; a < signature_.size(); ++a) {
            for (std::size_t b = a; b < signature_.size(); ++b) {
                bool cancel = true;
                for (std::size_t q = 0; q < eq_.size() && cancel; ++q) {
                    cancel = signature_[a][q] + signature_[b][q] == 0.0;
                }
                if (cancel) partners_.emplace_back(static_cast<int64_t>(a), static_cast<int64_t>(b));
            }
        }
        scale_.assign(ineq_.size(), 1.0);
        margin_.assign(ineq_.size(), 0.0);
        for (std::size_t q = 0; q < ineq_.size(); ++q) {
            const double* row = rows.dense().row(ineq_[q]);
            double mx = 0.0;
            for (int64_t j = 0; j < n; ++j) mx = std::max(mx, std::fabs(row[j]));
            scale_[q] = mx > 0.0 ? mx : 1.0;
            margin_[q] = 64.0 * static_cast<double>(n) * DBL_EPSILON * mx +
                         rows.rel_err(ineq_[q]) * mx * static_cast<double>(n) + rows.abs_err(ineq_[q]) * static_cast<double>(n);
        }
    }

    // A start near a relaxed point that keeps every count equal: each pair of
    // cancelling classes takes the same number of columns, the whole part of
    // the smaller of their two relaxed sums, and each class takes its columns
    // of largest relaxed value. Columns the search may not move keep their
    // rounded values. Returns whether the start satisfies every equation,
    // which a fixed column can prevent.
    bool rounded_start(const std::vector<double>& relaxed, std::vector<double>& start) const {
        start.assign(relaxed.begin(), relaxed.begin() + rows_.n());
        for (int64_t j = 0; j < rows_.n(); ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            start[sj] = cls_[sj] >= 0 ? 0.0 : std::round(start[sj]);
        }
        const auto take = [&](int64_t k, int64_t count) {
            std::vector<std::pair<double, int64_t>> order;
            for (int64_t j : members_[static_cast<std::size_t>(k)]) {
                order.emplace_back(-relaxed[static_cast<std::size_t>(j)], j);
            }
            std::sort(order.begin(), order.end());
            for (int64_t q = 0; q < count && q < static_cast<int64_t>(order.size()); ++q) {
                start[static_cast<std::size_t>(order[static_cast<std::size_t>(q)].second)] = 1.0;
            }
        };
        const auto mass = [&](int64_t k) {
            double s = 0.0;
            for (int64_t j : members_[static_cast<std::size_t>(k)]) s += relaxed[static_cast<std::size_t>(j)];
            return s;
        };
        for (const auto& pr : partners_) {
            if (pr.first == pr.second) continue;
            const int64_t count = static_cast<int64_t>(std::floor(std::min(mass(pr.first), mass(pr.second)) + 1e-9));
            take(pr.first, count);
            take(pr.second, count);
        }
        return equations_hold(start);
    }

    // Whether x satisfies every equation. The moves keep equations as they
    // are, so a start that misses one is not a start the search can repair.
    bool equations_hold(const std::vector<double>& x) const {
        for (int64_t r : eq_) {
            const double* row = rows_.dense().row(r);
            double acc = 0.0;
            for (int64_t j = 0; j < rows_.n(); ++j) {
                const double xj = x[static_cast<std::size_t>(j)];
                if (xj != 0.0) acc += row[j] * xj;
            }
            if (std::fabs(acc - rows_.lo(r)) > 1e-9 * (1.0 + std::fabs(acc))) return false;
        }
        return true;
    }

    // Improves `x` (0/1 on the movable columns) in place: repairs it to
    // feasibility, then grows it while the objective improves. Returns whether
    // the point left behind satisfies every inequality.
    bool run(std::vector<double>& x, int64_t max_moves) {
        x_ = &x;
        recompute();
        if (!repair(max_moves)) return false;
        grow(max_moves);
        improve(max_moves);
        return violation() <= 0.0;
    }

private:
    static constexpr std::size_t kShortlist = 48;
    static constexpr std::size_t kGrowTries = 8;

    const std::vector<double>& c_;
    const ExactRows& rows_;
    const std::vector<double>& lower_;
    const std::vector<double>& upper_;
    std::vector<int64_t> eq_;
    std::vector<int64_t> ineq_;
    std::vector<int64_t> cls_;
    std::vector<std::vector<int64_t>> members_;
    std::vector<std::vector<double>> signature_;
    std::vector<std::pair<int64_t, int64_t>> partners_;
    std::vector<double> scale_;
    std::vector<double> margin_;
    std::vector<double>* x_ = nullptr;
    std::vector<double> g_;   // activity of every inequality

    double a(std::size_t q, int64_t j) const { return rows_.dense().row(ineq_[q])[j]; }

    void recompute() {
        const std::vector<double>& x = *x_;
        g_.assign(ineq_.size(), 0.0);
        for (std::size_t q = 0; q < ineq_.size(); ++q) {
            const double* row = rows_.dense().row(ineq_[q]);
            double acc = 0.0;
            for (int64_t j = 0; j < rows_.n(); ++j) {
                const double xj = x[static_cast<std::size_t>(j)];
                if (xj != 0.0) acc += row[j] * xj;
            }
            g_[q] = acc;
        }
    }

    // Scaled violation of row q at activity v.
    double row_violation(std::size_t q, double v) const {
        const double lo = rows_.lo(ineq_[q]);
        const double hi = rows_.hi(ineq_[q]);
        double out = 0.0;
        if (std::isfinite(hi) && v > hi - margin_[q]) out = v - (hi - margin_[q]);
        if (std::isfinite(lo) && v < lo + margin_[q]) out = (lo + margin_[q]) - v;
        return out / scale_[q];
    }

    double violation() const {
        double v = 0.0;
        for (std::size_t q = 0; q < ineq_.size(); ++q) v += row_violation(q, g_[q]);
        return v;
    }

    // Violation after adding `add` and removing `drop` (either may be -1).
    double violation_after(int64_t add, int64_t drop, int64_t add2 = -1) const {
        double v = 0.0;
        for (std::size_t q = 0; q < ineq_.size(); ++q) {
            double g = g_[q];
            if (add >= 0) g += a(q, add);
            if (add2 >= 0) g += a(q, add2);
            if (drop >= 0) g -= a(q, drop);
            v += row_violation(q, g);
        }
        return v;
    }

    void flip(int64_t j, double to) {
        std::vector<double>& x = *x_;
        const double from = x[static_cast<std::size_t>(j)];
        if (from == to) return;
        for (std::size_t q = 0; q < ineq_.size(); ++q) g_[q] += (to - from) * a(q, j);
        x[static_cast<std::size_t>(j)] = to;
    }

    // Direction the inequalities ask to move in: +1 per unit of a row's
    // activity that is too high, -1 too low, weighted by the scaled violation;
    // rows within reach of a bound count a little, so a move that pushes them
    // over ranks below one that does not.
    std::vector<double> pressure() const {
        std::vector<double> w(ineq_.size(), 0.0);
        for (std::size_t q = 0; q < ineq_.size(); ++q) {
            const double lo = rows_.lo(ineq_[q]);
            const double hi = rows_.hi(ineq_[q]);
            const double viol = row_violation(q, g_[q]);
            if (viol > 0.0) {
                const bool high = std::isfinite(hi) && g_[q] > hi - margin_[q];
                w[q] = (high ? 1.0 : -1.0) * (1.0 + viol) / scale_[q];
            } else {
                const double room_hi = std::isfinite(hi) ? (hi - g_[q]) / scale_[q] : 1e300;
                const double room_lo = std::isfinite(lo) ? (g_[q] - lo) / scale_[q] : 1e300;
                if (room_hi < 1.0) w[q] = 1e-3 / scale_[q];
                else if (room_lo < 1.0) w[q] = -1e-3 / scale_[q];
            }
        }
        return w;
    }

    double score(const std::vector<double>& w, int64_t j) const {
        double s = 0.0;
        for (std::size_t q = 0; q < ineq_.size(); ++q) {
            if (w[q] != 0.0) s += w[q] * a(q, j);
        }
        return s;
    }

    // The best few columns of a class by score, in or out of the selection.
    std::vector<int64_t> shortlist(int64_t k, bool selected, const std::vector<double>& w,
                                   bool ascending) const {
        std::vector<std::pair<double, int64_t>> sc;
        for (int64_t j : members_[static_cast<std::size_t>(k)]) {
            const bool in = (*x_)[static_cast<std::size_t>(j)] != 0.0;
            if (in != selected) continue;
            sc.emplace_back(score(w, j), j);
        }
        const std::size_t keep = std::min(kShortlist, sc.size());
        if (ascending) {
            std::partial_sort(sc.begin(), sc.begin() + static_cast<std::ptrdiff_t>(keep), sc.end());
        } else {
            std::partial_sort(sc.begin(), sc.begin() + static_cast<std::ptrdiff_t>(keep), sc.end(),
                              [](const std::pair<double, int64_t>& p, const std::pair<double, int64_t>& q) {
                                  if (p.first != q.first) return p.first > q.first;
                                  return p.second < q.second;
                              });
        }
        std::vector<int64_t> out;
        for (std::size_t i = 0; i < keep; ++i) out.push_back(sc[i].second);
        return out;
    }

    bool repair(int64_t max_moves) {
        for (int64_t move = 0; move < max_moves; ++move) {
            const double v0 = violation();
            if (v0 <= 0.0) return true;
            const std::vector<double> w = pressure();
            double best = v0;
            int64_t best_in = -1;
            int64_t best_out = -1;
            for (std::size_t k = 0; k < members_.size(); ++k) {
                const std::vector<int64_t> outs = shortlist(static_cast<int64_t>(k), true, w, false);
                const std::vector<int64_t> ins = shortlist(static_cast<int64_t>(k), false, w, true);
                for (int64_t o : outs) {
                    for (int64_t i : ins) {
                        const double v = violation_after(i, o);
                        if (v < best - 1e-15) {
                            best = v;
                            best_in = i;
                            best_out = o;
                        }
                    }
                }
            }
            if (best_in >= 0) {
                flip(best_out, 0.0);
                flip(best_in, 1.0);
                continue;
            }
            // No exchange helps: drop the cancelling pair that helps most.
            double best_drop = kInf;
            int64_t d1 = -1;
            int64_t d2 = -1;
            for (const auto& pr : partners_) {
                const std::vector<int64_t> o1 = shortlist(pr.first, true, w, false);
                const std::vector<int64_t> o2 = shortlist(pr.second, true, w, false);
                for (int64_t p : o1) {
                    for (int64_t q : o2) {
                        if (p == q) continue;
                        double v = 0.0;
                        for (std::size_t r = 0; r < ineq_.size(); ++r) {
                            v += row_violation(r, g_[r] - a(r, p) - a(r, q));
                        }
                        if (v < best_drop) {
                            best_drop = v;
                            d1 = p;
                            d2 = q;
                        }
                    }
                }
            }
            if (d1 < 0) return false;
            flip(d1, 0.0);
            flip(d2, 0.0);
        }
        return violation() <= 0.0;
    }

    // Exchanges within a class that lower the objective and keep every
    // inequality satisfied, best first.
    void improve(int64_t max_moves) {
        for (int64_t move = 0; move < max_moves; ++move) {
            double best = 0.0;
            int64_t best_in = -1;
            int64_t best_out = -1;
            for (std::size_t k = 0; k < members_.size(); ++k) {
                std::vector<std::pair<double, int64_t>> outs;
                std::vector<std::pair<double, int64_t>> ins;
                for (int64_t j : members_[k]) {
                    const double cj = c_[static_cast<std::size_t>(j)];
                    if ((*x_)[static_cast<std::size_t>(j)] != 0.0) outs.emplace_back(-cj, j);
                    else ins.emplace_back(cj, j);
                }
                const std::size_t ko = std::min(kShortlist, outs.size());
                const std::size_t ki = std::min(kShortlist, ins.size());
                std::partial_sort(outs.begin(), outs.begin() + static_cast<std::ptrdiff_t>(ko), outs.end());
                std::partial_sort(ins.begin(), ins.begin() + static_cast<std::ptrdiff_t>(ki), ins.end());
                for (std::size_t a = 0; a < ko; ++a) {
                    for (std::size_t b = 0; b < ki; ++b) {
                        const int64_t o = outs[a].second;
                        const int64_t i = ins[b].second;
                        const double delta = c_[static_cast<std::size_t>(i)] - c_[static_cast<std::size_t>(o)];
                        if (delta >= best) continue;
                        if (violation_after(i, o) > 0.0) continue;
                        best = delta;
                        best_in = i;
                        best_out = o;
                    }
                }
            }
            if (best_in < 0) return;
            flip(best_out, 0.0);
            flip(best_in, 1.0);
        }
    }

    double objective() const {
        double z = 0.0;
        for (std::size_t j = 0; j < x_->size(); ++j) z += c_[j] * (*x_)[j];
        return z;
    }

    double objective_gain(int64_t p, int64_t q) const {
        return c_[static_cast<std::size_t>(p)] + c_[static_cast<std::size_t>(q)];
    }

    void grow(int64_t max_moves) {
        std::vector<double> saved;
        std::vector<std::pair<int64_t, int64_t>> refused;
        for (int64_t move = 0; move < max_moves; ++move) {
            const std::vector<double> w = pressure();
            double best = kInf;
            int64_t p1 = -1;
            int64_t p2 = -1;
            for (const auto& pr : partners_) {
                const std::vector<int64_t> i1 = shortlist(pr.first, false, w, true);
                const std::vector<int64_t> i2 = shortlist(pr.second, false, w, true);
                for (int64_t p : i1) {
                    for (int64_t q : i2) {
                        if (p == q || objective_gain(p, q) >= 0.0) continue;
                        if (std::find(refused.begin(), refused.end(), std::make_pair(p, q)) != refused.end()) continue;
                        const double v = violation_after(p, -1, q);
                        if (v < best) {
                            best = v;
                            p1 = p;
                            p2 = q;
                        }
                    }
                }
            }
            if (p1 < 0) return;
            saved = *x_;
            const double before = objective();
            flip(p1, 1.0);
            flip(p2, 1.0);
            if (!repair(max_moves) || violation() > 0.0 || objective() >= before) {
                *x_ = saved;
                recompute();
                refused.emplace_back(p1, p2);
                if (refused.size() > kGrowTries) return;
            }
        }
    }
};

}  // namespace select
}  // namespace lap
