// src/select/select_lp.h
// A bounded dual simplex over a few dense rows.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The problems the unit-selection search solves have one column per unit and a
// handful of rows: one balance equation per exactly balanced cell, two rows per
// moment bound, and whatever cuts the search adds. Every column is boxed, so
// the linear programme
//
//     min c'x   subject to   lo <= A x <= hi,   l <= x <= u
//
// is solved over the m x m basis of a few rows, and the n columns are only ever
// read through products with a row vector. The basis inverse is held densely
// and updated by one elimination step per pivot, which at this size costs less
// than a sparse factorization's bookkeeping, and it is recomputed from the
// basis columns at a fixed cadence so that rounding never accumulates across
// more than that many updates.
//
// Rows are written with a slack each, A x - r = 0 with lo <= r <= hi, so the
// slack is one more boxed variable and the all-slack basis is the identity up
// to sign. Every structural column is placed at the bound its cost points to,
// which makes that basis dual feasible from the start whenever the bound it
// points to is finite, and the dual simplex is the only algorithm needed: a
// branching decision moves a bound and an added row brings a basic slack, and
// both leave the basis dual feasible, so a node or a cut is re-solved from
// where its parent stopped.
//
// The ratio test is the long-step one. A boxed column whose breakpoint the dual
// step passes is moved to its other bound instead of entering the basis, for
// as long as the leaving row stays infeasible, which is what lets a single
// pivot take a selection from all-in to all-out across thousands of 0/1
// columns. Among the breakpoints at the step finally taken the largest pivot
// is preferred, Harris's rule, which keeps the basis away from small pivots.
//
// Nothing here is certified. The optimum and the duals are what a double
// computation gives, and what the search proves from them is read separately,
// exactly, by select_bound.h: any dual vector bounds the problem from below by
// weak duality, whether or not it is optimal.
#pragma once

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

namespace lap {
namespace select {

constexpr double kInf = std::numeric_limits<double>::infinity();

// Dense rows, row-major, n columns each.
struct DenseRows {
    int64_t n = 0;
    std::vector<double> a;
    std::vector<double> lo;
    std::vector<double> hi;

    int64_t m() const { return static_cast<int64_t>(lo.size()); }
    const double* row(int64_t r) const { return a.data() + r * n; }
    void add(const double* coef, double row_lo, double row_hi) {
        a.insert(a.end(), coef, coef + n);
        lo.push_back(row_lo);
        hi.push_back(row_hi);
    }
};

// Which variables are basic and where every other one sits. Variables are the
// n columns followed by one slack per row, and a basis records the rows it
// was taken over, so that rows added since come back with their slacks basic.
struct Basis {
    int64_t n_rows = 0;
    std::vector<int32_t> basic;      // per row position, the variable basic there
    std::vector<uint8_t> at_upper;   // per variable, nonbasic at its upper bound
};

enum class LpStatus { optimal, infeasible, iteration_limit };

class DualSimplex {
public:
    DualSimplex(const std::vector<double>& cost, const DenseRows& rows,
                const std::vector<double>& lower, const std::vector<double>& upper)
        : rows_(rows), n_(rows.n), c_(cost), lb_(lower), ub_(upper) {
        slack_basis();
    }

    // Shifts every structural cost away from zero by a small amount drawn from
    // the column index, in the direction its bound already sits, which keeps
    // the basis dual feasible. Columns of equal cost then no longer tie in the
    // ratio test, the source of long runs of degenerate pivots. The duals are
    // those of the shifted problem; what the search proves from them is read
    // against the unshifted costs, so the shift costs strength, never
    // validity.
    void perturb(double scale) {
        for (int64_t j = 0; j < n_; ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            uint64_t h = static_cast<uint64_t>(j) * 0x9E3779B97F4A7C15ULL;
            h ^= h >> 29;
            h *= 0xBF58476D1CE4E5B9ULL;
            h ^= h >> 32;
            const double u = static_cast<double>(h >> 11) * (1.0 / 9007199254740992.0);
            const double xi = scale * (1.0 + std::fabs(c_[sj])) * (0.5 + u);
            c_[sj] += at_upper_[sj] ? -xi : xi;
        }
        for (int64_t j = 0; j < n_; ++j) {
            if (pos_[static_cast<std::size_t>(j)] < 0) d_[static_cast<std::size_t>(j)] = c_[static_cast<std::size_t>(j)];
        }
        refresh();
    }

    int64_t n() const { return n_; }
    int64_t m() const { return m_; }
    const std::vector<double>& x() const { return x_; }
    const std::vector<double>& y() const { return y_; }
    const std::vector<double>& reduced_cost() const { return d_; }
    const std::vector<double>& infeasibility_duals() const { return infeasibility_duals_; }
    double lower(int64_t j) const { return lb_[static_cast<std::size_t>(j)]; }
    double upper(int64_t j) const { return ub_[static_cast<std::size_t>(j)]; }
    int64_t iterations() const { return iterations_; }

    double objective() const {
        double z = 0.0;
        for (int64_t j = 0; j < n_; ++j) z += c_[static_cast<std::size_t>(j)] * x_[static_cast<std::size_t>(j)];
        return z;
    }

    // A row appended to `rows` since the last call enters with its slack basic,
    // which keeps every reduced cost where it was.
    void sync_rows() {
        while (m_ < rows_.m()) {
            const int64_t r = m_;
            append_slack(r);
            // B grows by one row and one column, [B 0; c -1], whose inverse is
            // [B^-1 0; c B^-1 -1]: c holds the new row's entries on the basic
            // columns, which are zero on every basic slack.
            const int64_t mo = m_;
            ++m_;
            std::vector<double> grown(static_cast<std::size_t>(m_ * m_), 0.0);
            for (int64_t i = 0; i < mo; ++i) {
                for (int64_t k = 0; k < mo; ++k) grown[idx(i, k)] = binv_[static_cast<std::size_t>(i * mo + k)];
            }
            const double* row = rows_.row(r);
            for (int64_t k = 0; k < mo; ++k) {
                double s = 0.0;
                for (int64_t p = 0; p < mo; ++p) {
                    const int32_t v = basic_[static_cast<std::size_t>(p)];
                    if (v < n_) s += row[v] * binv_[static_cast<std::size_t>(p * mo + k)];
                }
                grown[idx(mo, k)] = s;
            }
            grown[idx(mo, mo)] = -1.0;
            binv_.swap(grown);
            basic_.push_back(static_cast<int32_t>(n_ + r));
            pos_[static_cast<std::size_t>(n_ + r)] = static_cast<int32_t>(mo);
        }
        refresh();
    }

    // Moves the bounds of column j. A nonbasic column goes to the bound its
    // reduced cost points to, so the basis stays dual feasible.
    void set_bounds(int64_t j, double lower, double upper) {
        lb_[static_cast<std::size_t>(j)] = lower;
        ub_[static_cast<std::size_t>(j)] = upper;
        if (pos_[static_cast<std::size_t>(j)] < 0) place_nonbasic(j);
    }

    Basis basis() const {
        Basis b;
        b.n_rows = m_;
        b.basic = basic_;
        b.at_upper = at_upper_;
        return b;
    }

    // Restores a basis taken earlier, over the rows it knew; later rows come in
    // with their slacks basic. A nonbasic column is re-placed under the current
    // bounds, which may have moved since.
    void load(const Basis& b) {
        basic_ = b.basic;
        for (int64_t r = b.n_rows; r < m_; ++r) basic_.push_back(static_cast<int32_t>(n_ + r));
        std::fill(pos_.begin(), pos_.end(), -1);
        for (std::size_t p = 0; p < basic_.size(); ++p) pos_[static_cast<std::size_t>(basic_[p])] = static_cast<int32_t>(p);
        for (int64_t v = 0; v < n_ + m_; ++v) {
            const std::size_t sv = static_cast<std::size_t>(v);
            at_upper_[sv] = (v < n_ + b.n_rows) ? b.at_upper[sv] : 0;
        }
        refactor();
        // Reduced costs depend on the basis alone, so they are read before the
        // nonbasic columns are placed by them.
        refresh();
        for (int64_t v = 0; v < n_ + m_; ++v) {
            if (pos_[static_cast<std::size_t>(v)] < 0) place_nonbasic(v);
        }
        refresh();
    }

    LpStatus solve(int64_t max_iterations) {
        infeasibility_duals_.clear();
        refresh();
        int64_t since_refactor = 0;
        for (int64_t it = 0; it < max_iterations; ++it) {
            const int64_t r = pick_leaving();
            if (r < 0) return LpStatus::optimal;
            const Step step = ratio_test(r);
            if (!step.feasible) {
                infeasibility_duals_.assign(binv_.begin() + r * m_,
                                             binv_.begin() + (r + 1) * m_);
                return LpStatus::infeasible;
            }
            apply(r, step);
            ++iterations_;
            if (++since_refactor >= kRefactorEvery) {
                refactor();
                since_refactor = 0;
            }
            refresh();
        }
        return LpStatus::iteration_limit;
    }

private:
    static constexpr int64_t kRefactorEvery = 64;
    static constexpr double kPrimalTol = 1e-9;
    static constexpr double kDualTol = 1e-9;
    static constexpr double kPivotTol = 1e-9;

    const DenseRows& rows_;
    int64_t n_;
    int64_t m_ = 0;
    std::vector<double> c_;
    std::vector<double> lb_;
    std::vector<double> ub_;
    std::vector<double> x_;
    std::vector<double> y_;
    std::vector<double> d_;
    std::vector<int32_t> basic_;
    std::vector<int32_t> pos_;
    std::vector<uint8_t> at_upper_;
    std::vector<double> binv_;   // m x m, row-major
    std::vector<double> alpha_;  // tableau row of the last ratio test
    std::vector<double> alpha_magnitude_;
    std::vector<double> infeasibility_duals_;
    int64_t iterations_ = 0;

    std::size_t idx(int64_t i, int64_t k) const { return static_cast<std::size_t>(i * m_ + k); }

    double cost(int64_t v) const { return v < n_ ? c_[static_cast<std::size_t>(v)] : 0.0; }
    double lo_of(int64_t v) const {
        return v < n_ ? lb_[static_cast<std::size_t>(v)] : rows_.lo[static_cast<std::size_t>(v - n_)];
    }
    double hi_of(int64_t v) const {
        return v < n_ ? ub_[static_cast<std::size_t>(v)] : rows_.hi[static_cast<std::size_t>(v - n_)];
    }

    void append_slack(int64_t) {
        x_.push_back(0.0);
        d_.push_back(0.0);
        pos_.push_back(-1);
        at_upper_.push_back(0);
    }

    void slack_basis() {
        x_.assign(static_cast<std::size_t>(n_ + rows_.m()), 0.0);
        d_.assign(x_.size(), 0.0);
        pos_.assign(x_.size(), -1);
        at_upper_.assign(x_.size(), 0);
        m_ = rows_.m();
        basic_.resize(static_cast<std::size_t>(m_));
        binv_.assign(static_cast<std::size_t>(m_ * m_), 0.0);
        for (int64_t r = 0; r < m_; ++r) {
            basic_[static_cast<std::size_t>(r)] = static_cast<int32_t>(n_ + r);
            pos_[static_cast<std::size_t>(n_ + r)] = static_cast<int32_t>(r);
            binv_[idx(r, r)] = -1.0;
        }
        // With the slacks basic every structural reduced cost is its cost.
        for (int64_t j = 0; j < n_; ++j) {
            d_[static_cast<std::size_t>(j)] = c_[static_cast<std::size_t>(j)];
            place_nonbasic(j);
        }
        refresh();
    }

    // A nonbasic variable sits at the bound its reduced cost points to, the
    // finite one when only one is, and at zero when it is free.
    void place_nonbasic(int64_t v) {
        const std::size_t sv = static_cast<std::size_t>(v);
        const double lo = lo_of(v);
        const double hi = hi_of(v);
        bool upper;
        if (std::isfinite(lo) && std::isfinite(hi)) {
            upper = d_[sv] < 0.0 || (d_[sv] == 0.0 && at_upper_[sv]);
        } else {
            upper = !std::isfinite(lo) && std::isfinite(hi);
        }
        at_upper_[sv] = upper ? 1 : 0;
        x_[sv] = upper ? hi : (std::isfinite(lo) ? lo : 0.0);
    }

    // Column entries: a structural column is its row coefficients, a slack is
    // minus a unit vector.
    void column(int64_t v, std::vector<double>& out) const {
        out.assign(static_cast<std::size_t>(m_), 0.0);
        if (v < n_) {
            for (int64_t r = 0; r < m_; ++r) out[static_cast<std::size_t>(r)] = rows_.row(r)[v];
        } else {
            out[static_cast<std::size_t>(v - n_)] = -1.0;
        }
    }

    // Duals, reduced costs and basic values from the basis inverse.
    void refresh() {
        y_.assign(static_cast<std::size_t>(m_), 0.0);
        for (int64_t p = 0; p < m_; ++p) {
            const double cb = cost(basic_[static_cast<std::size_t>(p)]);
            if (cb == 0.0) continue;
            for (int64_t k = 0; k < m_; ++k) y_[static_cast<std::size_t>(k)] += cb * binv_[idx(p, k)];
        }
        for (int64_t j = 0; j < n_; ++j) d_[static_cast<std::size_t>(j)] = c_[static_cast<std::size_t>(j)];
        for (int64_t r = 0; r < m_; ++r) {
            const double yr = y_[static_cast<std::size_t>(r)];
            if (yr == 0.0) continue;
            const double* row = rows_.row(r);
            for (int64_t j = 0; j < n_; ++j) d_[static_cast<std::size_t>(j)] -= yr * row[j];
        }
        for (int64_t r = 0; r < m_; ++r) d_[static_cast<std::size_t>(n_ + r)] = y_[static_cast<std::size_t>(r)];
        for (int64_t p = 0; p < m_; ++p) d_[static_cast<std::size_t>(basic_[static_cast<std::size_t>(p)])] = 0.0;

        // B x_B = -N x_N.
        std::vector<double> s(static_cast<std::size_t>(m_), 0.0);
        for (int64_t r = 0; r < m_; ++r) {
            const double* row = rows_.row(r);
            double acc = 0.0;
            for (int64_t j = 0; j < n_; ++j) {
                if (pos_[static_cast<std::size_t>(j)] < 0) acc += row[j] * x_[static_cast<std::size_t>(j)];
            }
            if (pos_[static_cast<std::size_t>(n_ + r)] < 0) acc -= x_[static_cast<std::size_t>(n_ + r)];
            s[static_cast<std::size_t>(r)] = acc;
        }
        for (int64_t p = 0; p < m_; ++p) {
            double acc = 0.0;
            for (int64_t k = 0; k < m_; ++k) acc += binv_[idx(p, k)] * s[static_cast<std::size_t>(k)];
            x_[static_cast<std::size_t>(basic_[static_cast<std::size_t>(p)])] = -acc;
        }
    }

    // The basis inverse from the basis columns, by Gauss-Jordan elimination with
    // partial pivoting. A column the elimination finds dependent is replaced by
    // the slack of a row no other column covers, which keeps a basis and moves
    // the replaced variable to a bound.
    void refactor() {
        std::vector<double> bmat(static_cast<std::size_t>(m_ * m_), 0.0);
        std::vector<double> column_scale(static_cast<std::size_t>(m_), 0.0);
        std::vector<double> col;
        for (int64_t p = 0; p < m_; ++p) {
            column(basic_[static_cast<std::size_t>(p)], col);
            for (int64_t r = 0; r < m_; ++r) {
                bmat[idx(r, p)] = col[static_cast<std::size_t>(r)];
                column_scale[static_cast<std::size_t>(p)] =
                    std::max(column_scale[static_cast<std::size_t>(p)],
                             std::fabs(col[static_cast<std::size_t>(r)]));
            }
        }
        std::vector<double> inv(static_cast<std::size_t>(m_ * m_), 0.0);
        for (int64_t i = 0; i < m_; ++i) inv[idx(i, i)] = 1.0;
        std::vector<int64_t> row_of(static_cast<std::size_t>(m_));
        std::vector<uint8_t> used(static_cast<std::size_t>(m_), 0);
        for (int64_t p = 0; p < m_; ++p) {
            int64_t best = -1;
            double best_abs = 0.0;
            for (int64_t r = 0; r < m_; ++r) {
                if (used[static_cast<std::size_t>(r)]) continue;
                const double a = std::fabs(bmat[idx(r, p)]);
                if (a > best_abs) {
                    best_abs = a;
                    best = r;
                }
            }
            if (best < 0 || best_abs <= kPivotTol * column_scale[static_cast<std::size_t>(p)]) {
                // A dependent basis. The all-slack basis is always a basis and,
                // with every column placed by its cost, always dual feasible,
                // so the solve continues from there.
                reset_to_slacks();
                return;
            }
            used[static_cast<std::size_t>(best)] = 1;
            row_of[static_cast<std::size_t>(p)] = best;
            const double piv = bmat[idx(best, p)];
            for (int64_t k = 0; k < m_; ++k) {
                bmat[idx(best, k)] /= piv;
                inv[idx(best, k)] /= piv;
            }
            for (int64_t r = 0; r < m_; ++r) {
                if (r == best) continue;
                const double f = bmat[idx(r, p)];
                if (f == 0.0) continue;
                for (int64_t k = 0; k < m_; ++k) {
                    bmat[idx(r, k)] -= f * bmat[idx(best, k)];
                    inv[idx(r, k)] -= f * inv[idx(best, k)];
                }
            }
        }
        // inv now maps B to a row permutation of I: row row_of[p] of inv is
        // row p of B^-1.
        for (int64_t p = 0; p < m_; ++p) {
            const int64_t src = row_of[static_cast<std::size_t>(p)];
            for (int64_t k = 0; k < m_; ++k) binv_[idx(p, k)] = inv[idx(src, k)];
        }
    }

    void reset_to_slacks() {
        for (int64_t v = 0; v < n_ + m_; ++v) pos_[static_cast<std::size_t>(v)] = -1;
        for (int64_t r = 0; r < m_; ++r) {
            basic_[static_cast<std::size_t>(r)] = static_cast<int32_t>(n_ + r);
            pos_[static_cast<std::size_t>(n_ + r)] = static_cast<int32_t>(r);
        }
        std::fill(binv_.begin(), binv_.end(), 0.0);
        for (int64_t r = 0; r < m_; ++r) binv_[idx(r, r)] = -1.0;
        for (int64_t j = 0; j < n_; ++j) {
            d_[static_cast<std::size_t>(j)] = c_[static_cast<std::size_t>(j)];
            place_nonbasic(j);
        }
        refresh();
    }

    // The basic variable furthest outside its bounds, each violation weighted
    // by the norm of its row of the basis inverse, which approximates dual
    // steepest edge at the cost of one pass over the inverse.
    int64_t pick_leaving() const {
        int64_t best = -1;
        double best_score = 0.0;
        for (int64_t p = 0; p < m_; ++p) {
            const int32_t v = basic_[static_cast<std::size_t>(p)];
            const double xv = x_[static_cast<std::size_t>(v)];
            const double lo = lo_of(v);
            const double hi = hi_of(v);
            double viol = 0.0;
            if (xv < lo - kPrimalTol * (1.0 + std::fabs(lo))) viol = lo - xv;
            else if (xv > hi + kPrimalTol * (1.0 + std::fabs(hi))) viol = xv - hi;
            if (viol <= 0.0) continue;
            double norm = 0.0;
            for (int64_t k = 0; k < m_; ++k) norm += binv_[idx(p, k)] * binv_[idx(p, k)];
            const double score = viol * viol / std::max(norm, 1e-300);
            if (score > best_score) {
                best_score = score;
                best = p;
            }
        }
        return best;
    }

    struct Step {
        bool feasible = true;
        int64_t entering = -1;
        std::vector<int64_t> flips;
        double target = 0.0;   // bound the leaving variable goes to
        bool to_upper = false;
    };

    Step ratio_test(int64_t r) {
        Step step;
        const int32_t leaving = basic_[static_cast<std::size_t>(r)];
        const double xl = x_[static_cast<std::size_t>(leaving)];
        const double lo = lo_of(leaving);
        const double hi = hi_of(leaving);
        const bool below = xl < lo;
        step.to_upper = !below;
        step.target = below ? lo : hi;
        const double dir = below ? 1.0 : -1.0;
        double slope = std::fabs(xl - step.target);

        // The tableau row: alpha_v = (row r of B^-1) . column v.
        const double* rho = binv_.data() + r * m_;
        alpha_.assign(static_cast<std::size_t>(n_ + m_), 0.0);
        alpha_magnitude_.assign(static_cast<std::size_t>(n_ + m_), 0.0);
        for (int64_t k = 0; k < m_; ++k) {
            const double rk = rho[k];
            if (rk == 0.0) continue;
            const double* row = rows_.row(k);
            for (int64_t j = 0; j < n_; ++j) {
                const double term = rk * row[j];
                alpha_[static_cast<std::size_t>(j)] += term;
                alpha_magnitude_[static_cast<std::size_t>(j)] += std::fabs(term);
            }
        }
        for (int64_t k = 0; k < m_; ++k) {
            alpha_[static_cast<std::size_t>(n_ + k)] = -rho[k];
            alpha_magnitude_[static_cast<std::size_t>(n_ + k)] = std::fabs(rho[k]);
        }

        struct Cand {
            int64_t v;
            double ratio;
            double abs_alpha;
        };
        std::vector<Cand> cands;
        for (int64_t v = 0; v < n_ + m_; ++v) {
            const std::size_t sv = static_cast<std::size_t>(v);
            if (pos_[sv] >= 0) continue;
            const double vlo = lo_of(v);
            const double vhi = hi_of(v);
            if (vlo == vhi) continue;
            const double a = alpha_[sv];
            if (std::fabs(a) <= kPivotTol * alpha_magnitude_[sv]) continue;
            // Moving v off its bound, in direction sigma, changes the leaving
            // variable by -a * sigma; it must move toward the violated bound. A
            // free nonbasic column may move either way.
            double sigma = at_upper_[sv] ? -1.0 : 1.0;
            if (!std::isfinite(vlo) && !std::isfinite(vhi)) sigma = (-a * dir > 0.0) ? 1.0 : -1.0;
            if (-a * sigma * dir <= 0.0) continue;
            const double ratio = std::max(0.0, sigma * d_[sv]) / std::fabs(a);
            cands.push_back({v, ratio, std::fabs(a)});
        }
        if (cands.empty()) {
            step.feasible = false;
            return step;
        }
        std::sort(cands.begin(), cands.end(), [](const Cand& a, const Cand& b) {
            if (a.ratio != b.ratio) return a.ratio < b.ratio;
            return a.v < b.v;
        });

        // Long step: pass breakpoints while the leaving row stays infeasible.
        std::size_t k = 0;
        while (k < cands.size()) {
            const int64_t v = cands[k].v;
            const double width = hi_of(v) - lo_of(v);
            if (!std::isfinite(width)) break;
            const double after = slope - cands[k].abs_alpha * width;
            if (after <= 0.0) break;
            slope = after;
            ++k;
        }
        if (k == cands.size()) {
            // Every candidate flips and the row is still infeasible: nothing
            // brings it inside its bounds.
            step.feasible = false;
            return step;
        }
        // Harris: among the breakpoints within the dual tolerance of the one the
        // step stops at, the largest pivot enters.
        const double bound = cands[k].ratio + kDualTol / cands[k].abs_alpha;
        std::size_t pick = k;
        for (std::size_t q = k; q < cands.size() && cands[q].ratio <= bound; ++q) {
            if (cands[q].abs_alpha > cands[pick].abs_alpha) pick = q;
        }
        for (std::size_t q = 0; q < k; ++q) step.flips.push_back(cands[q].v);
        step.entering = cands[pick].v;
        return step;
    }

    void apply(int64_t r, const Step& step) {
        for (int64_t v : step.flips) {
            const std::size_t sv = static_cast<std::size_t>(v);
            at_upper_[sv] = at_upper_[sv] ? 0 : 1;
            x_[sv] = at_upper_[sv] ? hi_of(v) : lo_of(v);
        }
        const int64_t q = step.entering;
        const int32_t leaving = basic_[static_cast<std::size_t>(r)];

        // w = B^-1 a_q; the pivot is w_r.
        std::vector<double> col;
        column(q, col);
        std::vector<double> w(static_cast<std::size_t>(m_), 0.0);
        double pivot_magnitude = 0.0;
        for (int64_t i = 0; i < m_; ++i) {
            double acc = 0.0;
            for (int64_t k = 0; k < m_; ++k) {
                const double term = binv_[idx(i, k)] * col[static_cast<std::size_t>(k)];
                acc += term;
                if (i == r) pivot_magnitude += std::fabs(term);
            }
            w[static_cast<std::size_t>(i)] = acc;
        }
        const double piv = w[static_cast<std::size_t>(r)];
        if (std::fabs(piv) <= kPivotTol * pivot_magnitude) {
            refactor();
            return;
        }
        for (int64_t k = 0; k < m_; ++k) binv_[idx(r, k)] /= piv;
        for (int64_t i = 0; i < m_; ++i) {
            if (i == r) continue;
            const double f = w[static_cast<std::size_t>(i)];
            if (f == 0.0) continue;
            for (int64_t k = 0; k < m_; ++k) binv_[idx(i, k)] -= f * binv_[idx(r, k)];
        }
        basic_[static_cast<std::size_t>(r)] = static_cast<int32_t>(q);
        pos_[static_cast<std::size_t>(q)] = static_cast<int32_t>(r);
        pos_[static_cast<std::size_t>(leaving)] = -1;
        at_upper_[static_cast<std::size_t>(leaving)] = step.to_upper ? 1 : 0;
        x_[static_cast<std::size_t>(leaving)] = step.target;
    }
};

}  // namespace select
}  // namespace lap
