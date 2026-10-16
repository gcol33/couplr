// src/select/select_bound.h
// Rows held exactly, and the lower bounds their duals prove.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// A row's coefficients are generally not doubles. A moment row's left entry is
// u_i - b, a difference of two doubles; a distance cut's entries are the
// potentials of a network solve, which are sums along shortest paths. The
// simplex is handed each entry rounded, and the rounding is what it optimizes,
// but every number the search proves is read from the entries as they are:
// a row is stored with its rounding for the simplex and its expansions for the
// proofs.
//
// Weak duality is what turns a dual vector into a proof. For any y,
//
//     c'x = y'(A x) + (c - A'y)'x
//         >= sum_r min(y_r lo_r, y_r hi_r) + sum_j min_{l_j <= x_j <= u_j} d_j x_j
//
// over every x in the box with lo <= A x <= hi, d = c - A'y. The right-hand
// side needs nothing from the simplex beyond y itself: not optimality, not
// feasibility, not accuracy. A row whose dual points at an infinite bound is
// dropped from y first, since the inequality holds for every y and that one
// would make the bound -infinity.
//
// It is evaluated two ways. rigorous_bound() runs in doubles and carries a
// bound on its own rounding, which it subtracts, so the double it returns is
// at most the exact value; that is cheap enough for every node. exact_bound()
// returns the value itself as an expansion, for the comparisons a rounding
// bound cannot settle: an incumbent and a bound that meet exactly.
#pragma once

#include "../core/lap_exact.h"
#include "select_lp.h"

#include <cfloat>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

namespace lap {
namespace select {

using exact::Expansion;

// A row as the caller states it: one expansion per column, its two bounds, and
// whether it is a constraint on the integer columns alone, which an integral
// candidate is checked against exactly, or a row that also involves a
// continuous column, whose value the candidate's evaluation decides.
struct RowSpec {
    std::vector<Expansion> coef;
    double lo = -kInf;
    double hi = kInf;
    bool hard = true;
};

// A positive power of two preserves every coefficient and finite endpoint
// exactly. Its exponent brings the largest coefficient near one, subject to
// retaining all components without overflow or underflow.
inline int row_scale_exponent(const RowSpec& spec) {
    double largest = 0.0;
    for (const Expansion& e : spec.coef) {
        for (double v : e) largest = std::max(largest, std::fabs(v));
    }
    if (largest == 0.0 || !std::isfinite(largest)) return 0;
    int exponent = -std::ilogb(largest);
    const auto preserves = [](double v, int power) {
        if (!std::isfinite(v) || v == 0.0) return true;
        const double scaled = std::scalbn(v, power);
        return std::isfinite(scaled) && std::scalbn(scaled, -power) == v;
    };
    while (exponent != 0) {
        bool valid = preserves(spec.lo, exponent) && preserves(spec.hi, exponent);
        for (const Expansion& e : spec.coef) {
            for (double v : e) {
                if (!preserves(v, exponent)) valid = false;
            }
        }
        if (valid) break;
        exponent += exponent > 0 ? -1 : 1;
    }
    return exponent;
}

class ExactRows {
public:
    explicit ExactRows(int64_t n) { dense_.n = n; }

    int64_t n() const { return dense_.n; }
    int64_t m() const { return dense_.m(); }
    const DenseRows& dense() const { return dense_; }
    bool hard(int64_t r) const { return hard_[static_cast<std::size_t>(r)] != 0; }
    double lo(int64_t r) const { return dense_.lo[static_cast<std::size_t>(r)]; }
    double hi(int64_t r) const { return dense_.hi[static_cast<std::size_t>(r)]; }
    double approx(int64_t r, int64_t j) const { return dense_.row(r)[j]; }

    // |exact - approx| <= rel_err * |approx| + abs_err, for every entry of r.
    double rel_err(int64_t r) const { return rel_err_[static_cast<std::size_t>(r)]; }
    double abs_err(int64_t r) const { return abs_err_[static_cast<std::size_t>(r)]; }

    void add(const RowSpec& spec) {
        const int64_t n = dense_.n;
        const int scale = row_scale_exponent(spec);
        std::vector<double> approx(static_cast<std::size_t>(n), 0.0);
        Stored st;
        bool exact_doubles = true;
        double rel = 0.0;
        double abs_e = 0.0;
        st.off.assign(static_cast<std::size_t>(n + 1), 0);
        for (int64_t j = 0; j < n; ++j) {
            Expansion e = spec.coef[static_cast<std::size_t>(j)];
            for (double& component : e) component = std::scalbn(component, scale);
            const exact::Approximation a = exact::approximate(e);
            approx[static_cast<std::size_t>(j)] = a.value;
            if (e.size() > 1) exact_doubles = false;
            if (a.error > 0.0) {
                if (a.value != 0.0) rel = std::max(rel, a.error / std::fabs(a.value));
                else abs_e = std::max(abs_e, a.error);
            }
            st.comp.insert(st.comp.end(), e.begin(), e.end());
            st.off[static_cast<std::size_t>(j + 1)] = static_cast<int64_t>(st.comp.size());
        }
        if (exact_doubles) {
            st.comp.clear();
            st.off.clear();
        }
        dense_.add(approx.data(), std::scalbn(spec.lo, scale),
                   std::scalbn(spec.hi, scale));
        stored_.push_back(std::move(st));
        hard_.push_back(spec.hard ? 1 : 0);
        rel_err_.push_back(rel * 1.0000001);
        abs_err_.push_back(abs_e * 1.0000001);
    }

    // Entry (r, j) exactly.
    Expansion entry(int64_t r, int64_t j) const {
        const Stored& st = stored_[static_cast<std::size_t>(r)];
        if (st.off.empty()) {
            const double a = approx(r, j);
            return a == 0.0 ? Expansion() : Expansion{a};
        }
        const auto b = st.comp.begin() + st.off[static_cast<std::size_t>(j)];
        const auto e = st.comp.begin() + st.off[static_cast<std::size_t>(j + 1)];
        return Expansion(b, e);
    }

    // a_r . x exactly, for x whose nonzero entries are listed in `support`
    // with their values.
    Expansion activity(int64_t r, const std::vector<int64_t>& support,
                       const std::vector<double>& x) const {
        Expansion acc;
        Expansion next;
        for (int64_t j : support) {
            const double xj = x[static_cast<std::size_t>(j)];
            const Expansion term = exact::scale_expansion(entry(r, j), xj);
            for (double comp : term) {
                exact::grow_expansion(acc, comp, next);
                acc.swap(next);
            }
            if (acc.size() > 16) exact::compress(acc);
        }
        exact::compress(acc);
        return acc;
    }

private:
    struct Stored {
        std::vector<double> comp;
        std::vector<int64_t> off;
    };
    DenseRows dense_;
    std::vector<Stored> stored_;
    std::vector<uint8_t> hard_;
    std::vector<double> rel_err_;
    std::vector<double> abs_err_;
};

// The box the columns live in at one node.
struct Box {
    const std::vector<double>* lower;
    const std::vector<double>* upper;
};

// y with every component that points at an infinite row bound set to zero.
inline std::vector<double> usable_duals(const ExactRows& rows, std::vector<double> y) {
    for (int64_t r = 0; r < rows.m(); ++r) {
        double& yr = y[static_cast<std::size_t>(r)];
        if (yr > 0.0 && !std::isfinite(rows.lo(r))) yr = 0.0;
        if (yr < 0.0 && !std::isfinite(rows.hi(r))) yr = 0.0;
    }
    return y;
}

// min over x in [lo, hi] of d x for every d in [dl, dh], in doubles: the
// smallest corner product, where an infinite end of the box multiplies only a
// d interval that keeps the product from running to -infinity.
inline double box_min(double dl, double dh, double lo, double hi) {
    double best = kInf;
    const auto corner = [&](double d, double x) {
        if (std::isinf(x)) {
            if (d == 0.0) return 0.0;
            return (d > 0.0) == (x > 0.0) ? kInf : -kInf;
        }
        return d * x;
    };
    best = std::min(best, corner(dl, lo));
    best = std::min(best, corner(dh, lo));
    best = std::min(best, corner(dl, hi));
    best = std::min(best, corner(dh, hi));
    return best;
}

struct RigorousBound {
    double value = -kInf;           // at most the exact weak-duality bound
    std::vector<double> y;          // the duals it was read at
    // Per column, a lower bound on how far the bound rises when the column is
    // held at the other end of its box from the end its term was read at, and
    // that end (0 lower, 1 upper, -1 when the reduced cost's sign is not
    // settled, in which case the rise is 0). value + rise[j] is then a bound
    // on every point with the column at its other end: reduced-cost fixing.
    std::vector<double> rise;
    std::vector<int8_t> end;
};

// The weak-duality bound at y, in doubles, rounded so that it never exceeds
// the exact value. When a column with an infinite bound would make the bound
// -infinity, y is shrunk toward zero until it does not, which keeps the bound
// finite at the price of part of its strength.
inline RigorousBound rigorous_bound(const std::vector<double>& c, const ExactRows& rows,
                                    const Box& box, const std::vector<double>& y_in) {
    const int64_t n = rows.n();
    const int64_t m = rows.m();
    RigorousBound out;
    std::vector<double> y = usable_duals(rows, y_in);
    const double gamma = static_cast<double>(m + 3) * DBL_EPSILON * 1.01;

    std::vector<double> g(static_cast<std::size_t>(n), 0.0);
    std::vector<double> s(static_cast<std::size_t>(n), 0.0);
    std::vector<double> e(static_cast<std::size_t>(n), 0.0);
    const auto accumulate = [&]() {
        std::fill(g.begin(), g.end(), 0.0);
        std::fill(s.begin(), s.end(), 0.0);
        std::fill(e.begin(), e.end(), 0.0);
        for (int64_t r = 0; r < m; ++r) {
            const double yr = y[static_cast<std::size_t>(r)];
            if (yr == 0.0) continue;
            const double* row = rows.dense().row(r);
            const double rel = rows.rel_err(r);
            const double ab = rows.abs_err(r);
            const double ay = std::fabs(yr);
            for (int64_t j = 0; j < n; ++j) {
                const double a = row[j];
                g[static_cast<std::size_t>(j)] += yr * a;
                s[static_cast<std::size_t>(j)] += ay * std::fabs(a);
                if (rel > 0.0 || ab > 0.0) {
                    e[static_cast<std::size_t>(j)] += ay * (rel * std::fabs(a) + ab);
                }
            }
        }
    };
    accumulate();

    // Shrink y until no column with an infinite bound pulls the bound to
    // -infinity. Scaling y by t moves d_j to c_j - t g_j and its error bound
    // to about t times its own, so t = c_j / (|g_j| + err_j), taken a few
    // roundings short, clears the column; the check is repeated on the
    // shrunken y, since every error bound moves with it.
    for (int attempt = 0; attempt < 8; ++attempt) {
        double shrink = 1.0;
        for (int64_t j = 0; j < n; ++j) {
            const std::size_t sj = static_cast<std::size_t>(j);
            const double lo = (*box.lower)[sj];
            const double hi = (*box.upper)[sj];
            const double err = gamma * (std::fabs(c[sj]) + s[sj]) * 1.01 + e[sj];
            const double d = c[sj] - g[sj];
            if (std::isinf(hi) && d - err < 0.0) {
                if (c[sj] <= 0.0) return out;
                shrink = std::min(shrink, c[sj] / (std::fabs(g[sj]) + err + DBL_MIN) *
                                              (1.0 - 16.0 * DBL_EPSILON));
            }
            if (std::isinf(lo) && d + err > 0.0) {
                if (c[sj] >= 0.0) return out;
                shrink = std::min(shrink, (-c[sj]) / (std::fabs(g[sj]) + err + DBL_MIN) *
                                              (1.0 - 16.0 * DBL_EPSILON));
            }
        }
        if (shrink >= 1.0) break;
        for (double& yr : y) yr *= shrink;
        accumulate();
        if (attempt == 7) return out;
    }

    double total = 0.0;
    double magnitude = 0.0;
    double slack_err = 0.0;
    out.rise.assign(static_cast<std::size_t>(n), 0.0);
    out.end.assign(static_cast<std::size_t>(n), -1);
    for (int64_t j = 0; j < n; ++j) {
        const std::size_t sj = static_cast<std::size_t>(j);
        const double err = gamma * (std::fabs(c[sj]) + s[sj]) * 1.01 + e[sj];
        const double d = c[sj] - g[sj];
        const double lo = (*box.lower)[sj];
        const double hi = (*box.upper)[sj];
        const double t = box_min(d - err, d + err, lo, hi);
        if (t == -kInf) return out;
        if (std::isfinite(lo) && std::isfinite(hi) && lo < hi) {
            // A settled sign reads the term at one end; the other end's term,
            // over the same interval of d, less this one, is the rise, taken
            // a few roundings short.
            int8_t at = -1;
            if (d - err > 0.0) at = 0;
            if (d + err < 0.0) at = 1;
            if (at >= 0) {
                const double other = at == 0 ? hi : lo;
                const double t_other = box_min(d - err, d + err, other, other);
                const double r = t_other - t;
                const double margin = 4.0 * DBL_EPSILON * (std::fabs(t_other) + std::fabs(t));
                if (r - margin > 0.0) {
                    out.rise[sj] = r - margin;
                    out.end[sj] = at;
                }
            }
        }
        total += t;
        magnitude += std::fabs(t);
        // The products d * x in box_min round once each.
        slack_err += DBL_EPSILON * std::fabs(t);
    }
    for (int64_t r = 0; r < m; ++r) {
        const double yr = y[static_cast<std::size_t>(r)];
        if (yr == 0.0) continue;
        const double t = yr > 0.0 ? yr * rows.lo(r) : yr * rows.hi(r);
        total += t;
        magnitude += std::fabs(t);
        slack_err += DBL_EPSILON * std::fabs(t);
    }
    const double sum_err = static_cast<double>(n + m + 2) * DBL_EPSILON * magnitude;
    double value = total - 1.01 * (sum_err + slack_err);
    value = std::nextafter(value, -kInf);
    out.value = value;
    out.y = std::move(y);
    return out;
}

// The weak-duality bound at y exactly; `finite` is false when a column with an
// infinite bound has a reduced cost of the wrong sign at y. When `point` is
// given, `against` receives every column whose exact reduced cost would move
// it off the value `point` holds: the columns that keep the bound below the
// value at that point.
inline Expansion exact_bound(const std::vector<double>& c, const ExactRows& rows,
                             const Box& box, const std::vector<double>& y_in, bool& finite,
                             const std::vector<double>* point = nullptr,
                             std::vector<int64_t>* against = nullptr) {
    const int64_t n = rows.n();
    const int64_t m = rows.m();
    const std::vector<double> y = usable_duals(rows, y_in);
    std::vector<int64_t> active;
    for (int64_t r = 0; r < m; ++r) {
        if (y[static_cast<std::size_t>(r)] != 0.0) active.push_back(r);
    }
    finite = true;
    Expansion total;
    Expansion next;
    const auto add = [&](const Expansion& term) {
        for (double comp : term) {
            exact::grow_expansion(total, comp, next);
            total.swap(next);
        }
        if (total.size() > 32) exact::compress(total);
    };

    for (int64_t j = 0; j < n; ++j) {
        const std::size_t sj = static_cast<std::size_t>(j);
        Expansion d;
        if (c[sj] != 0.0) d.push_back(c[sj]);
        for (int64_t r : active) {
            const Expansion a = rows.entry(r, j);
            if (a.empty()) continue;
            d = exact::expansion_sum(d, exact::negated(
                exact::scale_expansion(a, y[static_cast<std::size_t>(r)])));
        }
        const int sgn = exact::sign(d);
        if (sgn == 0) continue;
        const double bound = sgn > 0 ? (*box.lower)[sj] : (*box.upper)[sj];
        if (point && against && (*point)[sj] != bound && (*box.lower)[sj] != (*box.upper)[sj]) {
            against->push_back(j);
        }
        if (std::isinf(bound)) {
            finite = false;
            return Expansion();
        }
        if (bound == 0.0) continue;
        add(exact::scale_expansion(d, bound));
    }
    for (int64_t r : active) {
        const double yr = y[static_cast<std::size_t>(r)];
        const double rb = yr > 0.0 ? rows.lo(r) : rows.hi(r);
        add(exact::product(yr, rb));
    }
    exact::compress(total);
    return total;
}

}  // namespace select
}  // namespace lap
