// src/flow/flow_balltree.h
// A ball tree over the control units, and the bounds that let a pricer skip a
// subtree instead of reading it.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// price_block() reads every omitted pair, and the seed reads every pair again.
// Both questions are "which columns of row i are cheap", and a structure over
// the columns answers them by bounding a whole subtree at once: if the
// cheapest pair the subtree could hold still prices at or above zero, none of
// its columns is a violator and none of them is read.
//
// The bound needs two structures per node, not one, because the two things
// that make a pair inadmissible live in different coordinates:
//
//   - a whitened centre and radius, for the distance. Mahalanobis is Euclidean
//     after whitening by the Cholesky factor of the inverse covariance, so one
//     ball serves both metrics and the factor is computed once here. The
//     source holds inv_cov itself rather than a factor.
//   - an axis-aligned box in the *original* covariates, for the caliper.
//     Whitening is a rotation and a scaling, so it destroys the axis alignment
//     a per-variable caliper is stated in, and a box in whitened coordinates
//     would bound the wrong differences.
//
// The tree holds only what the source cannot answer for itself: the leaf
// ordering, the two structures, the whitening factor, and the per-node largest
// column dual. Calipers, max_distance and the maximize flag stay on the source
// and are read from it at query time.
//
// Manhattan and Chebyshev get no tree. A whitened ball bounds neither, and
// build_ball_tree() returns an empty tree for them rather than a wrong one,
// which routes those metrics to price_block().
#pragma once

#include "../core/lap_error.h"
#include "../core/lap_lazy_types.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <string>
#include <vector>

namespace lap {

// Whether a ball in whitened coordinates bounds the metric at all.
inline bool metric_has_ball_bound(DistanceMetric metric) {
    switch (metric) {
        case DistanceMetric::Euclidean:
        case DistanceMetric::SquaredEuclidean:
        case DistanceMetric::Mahalanobis:
            return true;
        case DistanceMetric::Manhattan:
        case DistanceMetric::Chebyshev:
            return false;
    }
    return false;
}

// The distance the source reports for a pair whose whitened separation is `d`,
// before the maximize flag is applied. Euclidean and Mahalanobis are that
// separation; SquaredEuclidean is its square. Non-decreasing in d either way,
// which is what makes a bound on d a bound on the cost.
inline double metric_cost_of(DistanceMetric metric, double d) {
    return metric == DistanceMetric::SquaredEuclidean ? d * d : d;
}

// Lower-triangular L with A = L L', A the symmetric part of `a`. False when A
// is not positive definite, which is the source saying its Mahalanobis
// distance has no whitening and no ball can bound it.
inline bool cholesky_lower(const std::vector<double>& a, int64_t n,
                           std::vector<double>& l) {
    l.assign(static_cast<std::size_t>(n * n), 0.0);
    for (int64_t i = 0; i < n; ++i) {
        for (int64_t j = 0; j <= i; ++j) {
            // raw_distance() reads diff' A diff, which is diff' ((A + A')/2)
            // diff for any A at all, so the symmetric part is the matrix being
            // factored and an inv_cov off by a rounding error in its symmetry
            // is not a different problem.
            double s = 0.5 * (a[static_cast<std::size_t>(i * n + j)] +
                              a[static_cast<std::size_t>(j * n + i)]);
            for (int64_t k = 0; k < j; ++k) {
                s -= l[static_cast<std::size_t>(i * n + k)] *
                     l[static_cast<std::size_t>(j * n + k)];
            }
            if (i == j) {
                if (!(s > 0.0)) return false;
                l[static_cast<std::size_t>(i * n + i)] = std::sqrt(s);
            } else {
                l[static_cast<std::size_t>(i * n + j)] =
                    s / l[static_cast<std::size_t>(j * n + j)];
            }
        }
    }
    return true;
}

struct BallTree {
    int64_t n_vars = 0;
    int32_t n_units = 0;
    int32_t leaf_size = 16;

    // The control units in leaf order. Node `id` owns perm[lo[id] .. hi[id]).
    std::vector<int32_t> perm;

    std::vector<int32_t> lo, hi, left, right;

    std::vector<double> centre;          // n_nodes * n_vars, whitened
    std::vector<double> radius;          // n_nodes
    std::vector<double> box_lo, box_hi;  // n_nodes * n_vars, original
    std::vector<double> max_v;           // n_nodes, refreshed per pricing round

    // The control coordinates after whitening, n_units * n_vars, indexed by
    // unit rather than by leaf position so that a permutation of `perm` costs
    // nothing to follow.
    std::vector<double> whitened;

    // L' of the Cholesky factor, n_vars * n_vars row-major and upper
    // triangular. Empty when whitening is the identity.
    std::vector<double> factor;

    // A relative allowance on the bound's own arithmetic. Every bound the tree
    // reports is lowered by it before anyone reads it, so a prune fires only
    // where the bound clears its threshold by more than the bound can be
    // wrong. Descending where it does not clear costs a leaf evaluation, which
    // is the cost source's own answer, so an allowance set too high loses time
    // and an allowance set too low would lose an edge.
    //
    // Two things go into it. The centre distance and the radius are each a sum
    // of n_vars squares and a square root, whose relative error is bounded by
    // gamma_{n_vars + 3} in the standard sense, with gamma_k = k*eps/(1 - k*eps).
    // Under Mahalanobis the tree bounds ||L' d|| while the source evaluates
    // d' A d for the same A the factor came from; the two are the same real
    // number, and they differ in floating point by the residual of the
    // factorization, which is measured against A rather than assumed.
    double bound_rel = 0.0;

    bool empty() const { return lo.empty(); }
    int32_t n_nodes() const { return static_cast<int32_t>(lo.size()); }
    bool is_leaf(int32_t id) const {
        return left[static_cast<std::size_t>(id)] < 0;
    }

    const double* node_centre(int32_t id) const {
        return &centre[static_cast<std::size_t>(id) * static_cast<std::size_t>(n_vars)];
    }
    const double* node_box_lo(int32_t id) const {
        return &box_lo[static_cast<std::size_t>(id) * static_cast<std::size_t>(n_vars)];
    }
    const double* node_box_hi(int32_t id) const {
        return &box_hi[static_cast<std::size_t>(id) * static_cast<std::size_t>(n_vars)];
    }
    const double* unit_whitened(int32_t unit) const {
        return &whitened[static_cast<std::size_t>(unit) * static_cast<std::size_t>(n_vars)];
    }
};

namespace detail {

// Split perm[a, b) recursively, recording the node structure and nothing else.
// A node is appended before its children, so the geometry pass that follows
// can fill a parent from its children in one reverse sweep.
inline int32_t ball_split(BallTree& tree, int32_t a, int32_t b) {
    const int32_t id = static_cast<int32_t>(tree.lo.size());
    tree.lo.push_back(a);
    tree.hi.push_back(b);
    tree.left.push_back(-1);
    tree.right.push_back(-1);

    if (b - a <= tree.leaf_size) return id;

    const int64_t n_vars = tree.n_vars;
    const double* w = tree.whitened.data();
    const int32_t* p = tree.perm.data();

    int64_t split_dim = 0;
    double best = -1.0;
    for (int64_t k = 0; k < n_vars; ++k) {
        double mn = std::numeric_limits<double>::infinity();
        double mx = -std::numeric_limits<double>::infinity();
        for (int32_t t = a; t < b; ++t) {
            const double x = w[static_cast<std::size_t>(p[t]) *
                                   static_cast<std::size_t>(n_vars) +
                               static_cast<std::size_t>(k)];
            if (x < mn) mn = x;
            if (x > mx) mx = x;
        }
        if (mx - mn > best) {
            best = mx - mn;
            split_dim = k;
        }
    }
    // Every member sits at the same whitened point, so there is no dimension
    // to cut on and the node stays a leaf however many units it holds. Its
    // radius is zero, which makes it the tightest bound in the tree anyway.
    if (!(best > 0.0)) return id;

    const int32_t mid = a + (b - a) / 2;
    std::nth_element(tree.perm.begin() + a, tree.perm.begin() + mid,
                     tree.perm.begin() + b,
                     [w, n_vars, split_dim](int32_t lhs, int32_t rhs) {
                         return w[static_cast<std::size_t>(lhs) *
                                      static_cast<std::size_t>(n_vars) +
                                  static_cast<std::size_t>(split_dim)] <
                                w[static_cast<std::size_t>(rhs) *
                                      static_cast<std::size_t>(n_vars) +
                                  static_cast<std::size_t>(split_dim)];
                     });

    const int32_t l = ball_split(tree, a, mid);
    const int32_t r = ball_split(tree, mid, b);
    tree.left[static_cast<std::size_t>(id)] = l;
    tree.right[static_cast<std::size_t>(id)] = r;
    return id;
}

}  // namespace detail

// Write the whitened image of one original-coordinate point into `out`, which
// must hold n_vars doubles. This is the query side of the same map the tree's
// centres were built under.
inline void whiten_point(const BallTree& tree, const double* x, double* out) {
    const int64_t n = tree.n_vars;
    if (tree.factor.empty()) {
        std::copy(x, x + n, out);
        return;
    }
    for (int64_t k = 0; k < n; ++k) {
        const double* row = &tree.factor[static_cast<std::size_t>(k * n)];
        double s = 0.0;
        for (int64_t m = k; m < n; ++m) s += row[m] * x[m];
        out[k] = s;
    }
}

namespace detail {

// gamma_k in the standard floating-point sense, saturating rather than going
// negative when k*eps reaches one, which a covariate count could only do at a
// dimension no tree would be built at.
inline double gamma_of(int64_t k) {
    const double e = 0.5 * std::numeric_limits<double>::epsilon();
    const double d = static_cast<double>(k) * e;
    if (!(d < 1.0)) return std::numeric_limits<double>::infinity();
    return d / (1.0 - d);
}

// The relative allowance the tree lowers every bound by.
//
// The geometric part is gamma_{n_vars + 3}: a sum of n_vars squares, the
// square root over it, and the centre and radius that were built the same way.
//
// The Mahalanobis part is the factorization residual. The tree measures
// ||L' d||^2 where the source measures d' A d, and the difference is
// d' (L L' - A) d, so the squared distances differ by at most
// ||L L' - A||_F relative to ||A||_F. Taking that ratio on the squared
// quantity and applying it to the distance is conservative, since a relative
// error r on a square is a relative error below r on its root for r below one.
inline double bound_allowance(const LazyCostMatrix& src,
                              const std::vector<double>& factor,
                              int64_t n_vars) {
    double rel = gamma_of(n_vars + 3);
    if (src.metric() != DistanceMetric::Mahalanobis || factor.empty()) {
        return rel;
    }

    const std::vector<double>& a = src.inv_cov();
    double num = 0.0;
    double den = 0.0;
    for (int64_t i = 0; i < n_vars; ++i) {
        for (int64_t j = 0; j < n_vars; ++j) {
            // factor holds L' upper triangular, so (L L')_ij is the inner
            // product of columns i and j of L', over the rows both reach.
            double lij = 0.0;
            const int64_t stop = i < j ? i : j;
            for (int64_t k = 0; k <= stop; ++k) {
                lij += factor[static_cast<std::size_t>(k * n_vars + i)] *
                       factor[static_cast<std::size_t>(k * n_vars + j)];
            }
            const double aij = 0.5 * (a[static_cast<std::size_t>(i * n_vars + j)] +
                                      a[static_cast<std::size_t>(j * n_vars + i)]);
            const double d = lij - aij;
            num += d * d;
            den += aij * aij;
        }
    }
    if (!(den > 0.0)) return rel;
    const double resid = std::sqrt(num) / std::sqrt(den);
    if (!std::isfinite(resid)) return std::numeric_limits<double>::infinity();
    return rel + resid;
}

}  // namespace detail

// The tree over `src`'s columns, or an empty tree when the metric or the
// covariance leaves no bound. An empty tree is a routing answer rather than an
// error: it says this source is price_block()'s to scan.
inline BallTree build_ball_tree(const LazyCostMatrix& src, int32_t leaf_size = 16) {
    BallTree tree;
    if (!metric_has_ball_bound(src.metric())) return tree;
    if (src.ncol <= 0 || src.n_vars() <= 0) return tree;

    const int64_t n_vars = src.n_vars();
    if (src.metric() == DistanceMetric::Mahalanobis) {
        if (static_cast<int64_t>(src.inv_cov().size()) != n_vars * n_vars) return tree;
        std::vector<double> l;
        if (!cholesky_lower(src.inv_cov(), n_vars, l)) return tree;
        tree.factor.assign(static_cast<std::size_t>(n_vars * n_vars), 0.0);
        for (int64_t m = 0; m < n_vars; ++m) {
            for (int64_t k = 0; k <= m; ++k) {
                tree.factor[static_cast<std::size_t>(k * n_vars + m)] =
                    l[static_cast<std::size_t>(m * n_vars + k)];
            }
        }
    }

    tree.n_vars = n_vars;
    tree.n_units = static_cast<int32_t>(src.ncol);
    tree.leaf_size = leaf_size > 0 ? leaf_size : 1;
    tree.bound_rel = detail::bound_allowance(src, tree.factor, n_vars);

    tree.whitened.assign(static_cast<std::size_t>(src.ncol) *
                             static_cast<std::size_t>(n_vars), 0.0);
    for (int64_t j = 0; j < src.ncol; ++j) {
        whiten_point(tree, src.right_row(j),
                     &tree.whitened[static_cast<std::size_t>(j) *
                                    static_cast<std::size_t>(n_vars)]);
    }

    tree.perm.resize(static_cast<std::size_t>(src.ncol));
    for (int32_t j = 0; j < tree.n_units; ++j) {
        tree.perm[static_cast<std::size_t>(j)] = j;
    }
    detail::ball_split(tree, 0, tree.n_units);

    const std::size_t n_nodes = tree.lo.size();
    const std::size_t span = static_cast<std::size_t>(n_vars);
    tree.centre.assign(n_nodes * span, 0.0);
    tree.box_lo.assign(n_nodes * span, 0.0);
    tree.box_hi.assign(n_nodes * span, 0.0);
    tree.radius.assign(n_nodes, 0.0);
    tree.max_v.assign(n_nodes, 0.0);

    for (int32_t id = static_cast<int32_t>(n_nodes) - 1; id >= 0; --id) {
        const std::size_t base = static_cast<std::size_t>(id) * span;
        const int32_t a = tree.lo[static_cast<std::size_t>(id)];
        const int32_t b = tree.hi[static_cast<std::size_t>(id)];

        double* ctr = &tree.centre[base];
        for (int32_t t = a; t < b; ++t) {
            const double* pw = tree.unit_whitened(tree.perm[static_cast<std::size_t>(t)]);
            for (int64_t k = 0; k < n_vars; ++k) ctr[k] += pw[k];
        }
        const double inv = 1.0 / static_cast<double>(b - a);
        for (int64_t k = 0; k < n_vars; ++k) ctr[k] *= inv;

        double worst = 0.0;
        for (int32_t t = a; t < b; ++t) {
            const double* pw = tree.unit_whitened(tree.perm[static_cast<std::size_t>(t)]);
            double s = 0.0;
            for (int64_t k = 0; k < n_vars; ++k) {
                const double d = pw[k] - ctr[k];
                s += d * d;
            }
            if (s > worst) worst = s;
        }
        tree.radius[static_cast<std::size_t>(id)] = std::sqrt(worst);

        double* blo = &tree.box_lo[base];
        double* bhi = &tree.box_hi[base];
        if (tree.is_leaf(id)) {
            for (int64_t k = 0; k < n_vars; ++k) {
                blo[k] = std::numeric_limits<double>::infinity();
                bhi[k] = -std::numeric_limits<double>::infinity();
            }
            for (int32_t t = a; t < b; ++t) {
                const double* px = src.right_row(
                    static_cast<int64_t>(tree.perm[static_cast<std::size_t>(t)]));
                for (int64_t k = 0; k < n_vars; ++k) {
                    if (px[k] < blo[k]) blo[k] = px[k];
                    if (px[k] > bhi[k]) bhi[k] = px[k];
                }
            }
        } else {
            const double* llo = tree.node_box_lo(tree.left[static_cast<std::size_t>(id)]);
            const double* lhi = tree.node_box_hi(tree.left[static_cast<std::size_t>(id)]);
            const double* rlo = tree.node_box_lo(tree.right[static_cast<std::size_t>(id)]);
            const double* rhi = tree.node_box_hi(tree.right[static_cast<std::size_t>(id)]);
            for (int64_t k = 0; k < n_vars; ++k) {
                blo[k] = llo[k] < rlo[k] ? llo[k] : rlo[k];
                bhi[k] = lhi[k] > rhi[k] ? lhi[k] : rhi[k];
            }
        }
    }

    return tree;
}

// The whitened separation between a query point and the nearest and furthest
// members a node could hold. Both are bounds over the ball rather than
// distances any member attains.
struct BallBounds {
    double d_lo = 0.0;
    double d_hi = 0.0;
};

inline BallBounds node_ball_bounds(const BallTree& tree, const double* q_whitened,
                                   int32_t id) {
    const int64_t n = tree.n_vars;
    const double* ctr = tree.node_centre(id);
    double s = 0.0;
    for (int64_t k = 0; k < n; ++k) {
        const double d = q_whitened[k] - ctr[k];
        s += d * d;
    }
    const double dc = std::sqrt(s);
    const double r = tree.radius[static_cast<std::size_t>(id)];
    // The near side of the ball is what a prune rests on, so the centre
    // distance is read low and the radius high; the far side, which bounds a
    // maximised cost, is read the other way. Both are moved by the tree's own
    // allowance, so neither side can be tighter than the arithmetic that
    // produced it.
    const double rel = tree.bound_rel;
    const double dc_lo = dc - rel * dc;
    const double r_hi = r + rel * r;
    BallBounds out;
    out.d_lo = dc_lo > r_hi ? dc_lo - r_hi : 0.0;
    const double far = dc + r;
    out.d_hi = far + rel * far;
    return out;
}

// A lower bound on src.at(i, j) over every column the node holds. Under
// maximize the cost falls as the distance grows, so the bound comes off the far
// side of the ball.
inline double cost_lo_of(const LazyCostMatrix& src, const BallBounds& b) {
    // metric_cost_of is monotone in the distance, so a bound on the distance
    // carries to a bound on the cost, and its own rounding is one step.
    const double inf = std::numeric_limits<double>::infinity();
    const double c = src.is_negated() ? -metric_cost_of(src.metric(), b.d_hi)
                                      : metric_cost_of(src.metric(), b.d_lo);
    return std::nextafter(c, -inf);
}

// Whether max_distance forbids every column the node holds. The comparison is
// against the distance the source measures, before the maximize flag, which is
// the order raw_distance() is tested in.
inline bool distance_out_of(const LazyCostMatrix& src, const BallBounds& b) {
    const double limit = src.max_distance();
    if (!std::isfinite(limit)) return false;
    return metric_cost_of(src.metric(), b.d_lo) > limit;
}

// The same two questions from a node rather than from a ball already measured.
// A caller asking both about one node measures the ball once and reads them
// off it; these are for a caller asking one.
inline double node_cost_lo(const BallTree& tree, const LazyCostMatrix& src,
                           const double* q_whitened, int32_t id) {
    return cost_lo_of(src, node_ball_bounds(tree, q_whitened, id));
}

inline bool node_distance_out(const BallTree& tree, const LazyCostMatrix& src,
                              const double* q_whitened, int32_t id) {
    if (!std::isfinite(src.max_distance())) return false;
    return distance_out_of(src, node_ball_bounds(tree, q_whitened, id));
}

// Whether the calipers forbid every column the node holds. Read in the
// original covariates, where the per-variable window is stated.
inline bool node_caliper_out(const BallTree& tree, const LazyCostMatrix& src,
                             const double* q_original, int32_t id) {
    const std::vector<CaliperSpec>& cals = src.calipers();
    if (cals.empty()) return false;
    const double* blo = tree.node_box_lo(id);
    const double* bhi = tree.node_box_hi(id);
    const double inf = std::numeric_limits<double>::infinity();
    for (const CaliperSpec& cal : cals) {
        const std::size_t k = static_cast<std::size_t>(cal.var_index);
        const double x = q_original[k];
        // The window's two endpoints are each one rounded addition, and the
        // box holds coordinates copied rather than computed. Widening the
        // window by one representable step on each side puts the rounding on
        // the side that declines to prune.
        const double hi_edge = std::nextafter(x + cal.threshold, inf);
        const double lo_edge = std::nextafter(x - cal.threshold, -inf);
        if (blo[k] > hi_edge) return true;
        if (bhi[k] < lo_edge) return true;
    }
    return false;
}

// A lower bound on the cost of every column the node holds, infinite when the
// node's box or its ball puts them all out of reach. This is the one question
// a descent asks of a node, and both the caliper and the distance limit answer
// it by ruling the node out rather than by bounding it.
inline double node_cost_floor(const BallTree& tree, const LazyCostMatrix& src,
                              const double* q_whitened, const double* q_original,
                              int32_t id) {
    if (node_caliper_out(tree, src, q_original, id)) {
        return std::numeric_limits<double>::infinity();
    }
    const BallBounds b = node_ball_bounds(tree, q_whitened, id);
    if (distance_out_of(src, b)) return std::numeric_limits<double>::infinity();
    return cost_lo_of(src, b);
}

// One bottom-up pass setting each node's largest column dual, which is the
// half of the pricing bound that moves. Children are appended after their
// parent, so a reverse sweep over the nodes fills every one after both of its
// children and the pass is O(n_units + n_nodes).
inline void refresh_max_v(BallTree& tree, const std::vector<double>& v) {
    if (tree.empty()) return;
    if (static_cast<int32_t>(v.size()) != tree.n_units) {
        LAP_THROW_DIMENSION("refresh_max_v: " + std::to_string(v.size()) +
                            " column duals for " + std::to_string(tree.n_units) +
                            " columns");
    }
    for (int32_t id = tree.n_nodes() - 1; id >= 0; --id) {
        if (tree.is_leaf(id)) {
            double m = -std::numeric_limits<double>::infinity();
            const int32_t b = tree.hi[static_cast<std::size_t>(id)];
            for (int32_t t = tree.lo[static_cast<std::size_t>(id)]; t < b; ++t) {
                const double x = v[static_cast<std::size_t>(
                    tree.perm[static_cast<std::size_t>(t)])];
                if (x > m) m = x;
            }
            tree.max_v[static_cast<std::size_t>(id)] = m;
        } else {
            const double l = tree.max_v[static_cast<std::size_t>(
                tree.left[static_cast<std::size_t>(id)])];
            const double r = tree.max_v[static_cast<std::size_t>(
                tree.right[static_cast<std::size_t>(id)])];
            tree.max_v[static_cast<std::size_t>(id)] = l > r ? l : r;
        }
    }
}

}  // namespace lap
