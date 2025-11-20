// utils_gabow_tarjan.h
// Header for Gabow-Tarjan utilities

#ifndef UTILS_GABOW_TARJAN_H
#define UTILS_GABOW_TARJAN_H

#include <vector>

// ============================================================================
// Constants and Type Definitions
// ============================================================================

constexpr int NIL = -1;
constexpr long long BIG_INT = 1000000000000000LL; // 1e15

using CostMatrix = std::vector<std::vector<long long>>;
using MatchVec   = std::vector<int>;
using DualVec    = std::vector<long long>;

// ============================================================================
// Module A: Cost-length & 1-feasibility utilities
// ============================================================================

long long cost_length(long long c_ij, bool in_matching);

bool is_eligible(long long c_ij, bool in_matching, 
                 long long yu, long long yv);

bool check_one_feasible(const CostMatrix& cost,
                        const MatchVec& row_match,
                        const MatchVec& col_match,
                        const DualVec& y_u,
                        const DualVec& y_v);

// ============================================================================
// Module B: Equality graph construction
// ============================================================================

std::vector<std::vector<int>>
build_equality_graph(const CostMatrix& cost,
                     const MatchVec& row_match,
                     const DualVec& y_u,
                     const DualVec& y_v);

// ============================================================================
// Module C: Augment matching along a path
// ============================================================================

void augment_along_path(const std::vector<std::pair<int,int>>& edges,
                        MatchVec& row_match,
                        MatchVec& col_match);

// ============================================================================
// Module D: Maximal set of augmenting paths on equality graph
// ============================================================================

std::vector<std::pair<int,int>>
find_one_augmenting_path_eq(const std::vector<std::vector<int>>& eq_graph,
                            const MatchVec& row_match,
                            const MatchVec& col_match,
                            const std::vector<bool>& banned_row,
                            const std::vector<bool>& banned_col);

std::vector<std::vector<std::pair<int,int>>>
find_maximal_augmenting_paths(const std::vector<std::vector<int>>& eq_graph,
                              const MatchVec& row_match,
                              const MatchVec& col_match);

// ============================================================================
// Module E: Hungarian-style search on cost-length (Step 2 core)
// ============================================================================

CostMatrix build_cl_matrix(const CostMatrix& cost,
                           const MatchVec& row_match);

bool hungarian_search_cl(const CostMatrix& C_cl,
                        MatchVec& row_match,
                        MatchVec& col_match,
                        DualVec& y_u,
                        DualVec& y_v);

bool hungarian_step_one_feasible(const CostMatrix& cost,
                                 MatchVec& row_match,
                                 MatchVec& col_match,
                                 DualVec& y_u,
                                 DualVec& y_v);

// ============================================================================
// Module F: match_gt - Inner Gabow-Tarjan matching algorithm
// ============================================================================

bool is_perfect(const MatchVec& row_match);

bool apply_step1(const CostMatrix& cost,
                MatchVec& row_match,
                MatchVec& col_match,
                DualVec& y_u,
                DualVec& y_v);

void match_gt(const CostMatrix& cost,
             MatchVec& row_match,
             MatchVec& col_match,
             DualVec& y_u,
             DualVec& y_v,
             int max_iters = 1000,
             bool check_feasible = false);

// ============================================================================
// Module G: scale_match - Wrapper for bit-scaling outer loop
// ============================================================================

void scale_match(const CostMatrix& cost,
                MatchVec& row_match,
                MatchVec& col_match,
                DualVec& y_u,
                DualVec& y_v);

// ============================================================================
// Module H: Gabow-Tarjan bit-scaling outer loop
// ============================================================================

long long find_max_cost(const CostMatrix& cost);

void solve_gabow_tarjan_inner(const CostMatrix& cost,
                              MatchVec& row_match,
                              MatchVec& col_match,
                              DualVec& y_u,
                              DualVec& y_v);

#endif // UTILS_GABOW_TARJAN_H
