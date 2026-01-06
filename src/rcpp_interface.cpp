// src/rcpp_interface.cpp
// Exports for 4-mode pixel morphing + LAP solvers

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <limits>
#include <string>
#include <algorithm>
#include "../core/lap_internal.h"
#include "../core/lap_utils.h"
#include "../gabow_tarjan/utils_gabow_tarjan.h"

using namespace Rcpp;

// =======================
// Forward decls for greedy matching (implemented in solvers/greedy_matching.cpp)
extern Rcpp::List greedy_matching_sorted_impl(Rcpp::NumericMatrix cost_matrix, bool maximize);
extern Rcpp::List greedy_matching_row_best_impl(Rcpp::NumericMatrix cost_matrix, bool maximize);
extern Rcpp::List greedy_matching_pq_impl(Rcpp::NumericMatrix cost_matrix, bool maximize);
extern Rcpp::List greedy_matching_impl(Rcpp::NumericMatrix cost_matrix, bool maximize, std::string strategy);

// =======================
// Forward decls for LAP solvers (no [[Rcpp::export]] here)
Rcpp::List solve_cycle_cancel_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_gabow_tarjan_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_lapmod_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_bottleneck_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_csa_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_sinkhorn_impl(Rcpp::NumericMatrix cost, double lambda, double tol,
                               int max_iter, Rcpp::Nullable<Rcpp::NumericVector> r_weights,
                               Rcpp::Nullable<Rcpp::NumericVector> c_weights);
Rcpp::IntegerVector sinkhorn_round_impl(Rcpp::NumericMatrix P);
Rcpp::List solve_ramshaw_tarjan_impl(Rcpp::NumericMatrix cost, bool maximize);
// =======================
Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);
Rcpp::List solve_bruteforce_impl(NumericMatrix cost, bool maximize);
Rcpp::List solve_jv_impl(NumericMatrix cost, bool maximize);
Rcpp::List solve_murty_impl(Rcpp::NumericMatrix cost, int k, bool maximize, std::string single_method);
Rcpp::List solve_auction_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize, std::string schedule);
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize,
                                     double initial_epsilon_factor,
                                     double alpha,
                                     double final_epsilon);
Rcpp::List solve_auction_gauss_seidel_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
Rcpp::List solve_ssp_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_hungarian_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_csflow_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_kbest_lawler_impl(Rcpp::NumericMatrix cost, int k, std::string method_base, bool maximize);
Rcpp::List solve_hk01_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_line_metric_impl(const Rcpp::NumericVector& x,
                                  const Rcpp::NumericVector& y,
                                  const std::string& cost,
                                  bool maximize);
Rcpp::List solve_ssap_bucket_impl(Rcpp::NumericMatrix cost, bool maximize);

// =======================
// Pixel morphing core (implemented in morph_pixel_level.cpp)
// =======================
extern Rcpp::List analyze_color_overlap(const Rcpp::NumericVector& pixelsA,
                                        const Rcpp::NumericVector& pixelsB,
                                        int H, int W,
                                        int quantize_bits);

// REMOVED: extract_patches - No longer needed with square tiling implementation

// REMOVED: compute_color_match_assignment - R does the assignment!
// REMOVED: compute_color_walk_assignment - R does the assignment!

extern Rcpp::NumericMatrix compute_pixel_cost(const Rcpp::NumericVector& pixelsA,
                                              const Rcpp::NumericVector& pixelsB,
                                              int H, int W,
                                              double alpha, double beta);

extern Rcpp::NumericVector downscale_image(const Rcpp::NumericVector& pixels,
                                           int H, int W, int H_new, int W_new);

extern Rcpp::IntegerVector upscale_assignment(const Rcpp::IntegerVector& assignment,
                                              int H_orig, int W_orig,
                                              int H_scaled, int W_scaled);

extern Rcpp::List morph_pixel_level_impl(const Rcpp::NumericVector& pixelsA,
                                         const Rcpp::NumericVector& pixelsB,
                                         const Rcpp::IntegerVector& assignment,
                                         int H, int W,
                                         int n_frames);

extern Rcpp::List color_palette_info(const Rcpp::NumericVector& pixelsA,
                                     const Rcpp::NumericVector& pixelsB,
                                     int H, int W,
                                     int quantize_bits);

extern Rcpp::NumericMatrix spatial_cost_matrix(const Rcpp::IntegerVector& idxA,
                                               const Rcpp::IntegerVector& idxB,
                                               int H, int W);

// =======================
// LAP Solver Exports
// =======================

// [[Rcpp::export]]
Rcpp::List lap_prepare_cost_matrix(NumericMatrix cost, bool maximize) {
  return prepare_cost_matrix_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_bruteforce(NumericMatrix cost, bool maximize) {
  return solve_bruteforce_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_jv(NumericMatrix cost, bool maximize) {
  return solve_jv_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_kbest_murty(Rcpp::NumericMatrix cost, int k, bool maximize,
                           std::string single_method = "jv") {
  return solve_murty_impl(cost, k, maximize, single_method);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_auction(Rcpp::NumericMatrix cost, bool maximize,
                             Rcpp::Nullable<double> eps = R_NilValue) {
  double eps_in = std::numeric_limits<double>::quiet_NaN();
  if (eps.isNotNull()) eps_in = Rcpp::as<double>(eps.get());
  return solve_auction_impl(cost, maximize, eps_in);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_auction_scaled(Rcpp::NumericMatrix cost, bool maximize,
                                    std::string schedule = "alpha7") {
  std::transform(schedule.begin(), schedule.end(), schedule.begin(),
                 [](unsigned char c){ return static_cast<char>(std::tolower(c)); });
  if (schedule != "alpha7" && schedule != "pow2" && schedule != "halves") {
    Rcpp::stop("Invalid schedule: '%s'. Use: 'alpha7', 'pow2', 'halves'.", schedule.c_str());
  }
  return solve_auction_scaled_impl(cost, maximize, schedule);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_auction_scaled_params(Rcpp::NumericMatrix cost, bool maximize,
                                           double initial_epsilon_factor = 1.0,
                                           double alpha = 7.0,
                                           Rcpp::Nullable<double> final_epsilon = R_NilValue) {
  double fe = -1.0;
  if (final_epsilon.isNotNull()) fe = Rcpp::as<double>(final_epsilon.get());
  return solve_auction_scaled_impl(cost, maximize, initial_epsilon_factor, alpha, fe);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_auction_gs(Rcpp::NumericMatrix cost, bool maximize,
                                Rcpp::Nullable<double> eps = R_NilValue) {
  double eps_in = std::numeric_limits<double>::quiet_NaN();
  if (eps.isNotNull()) eps_in = Rcpp::as<double>(eps.get());
  return solve_auction_gauss_seidel_impl(cost, maximize, eps_in);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_ssp(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_ssp_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_hungarian(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_hungarian_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_csflow(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_csflow_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_kbest_lawler(Rcpp::NumericMatrix cost, int k,
                            std::string method_base = "jv", bool maximize = false) {
  return solve_kbest_lawler_impl(cost, k, method_base, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_hk01(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_hk01_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_line_metric_cpp(Rcpp::NumericVector x,
                                     Rcpp::NumericVector y,
                                     std::string cost = "L1",
                                     bool maximize = false) {
  return solve_line_metric_impl(x, y, cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_ssap_bucket(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_ssap_bucket_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_gabow_tarjan(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_gabow_tarjan_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_lapmod(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_lapmod_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_bottleneck(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_bottleneck_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_csa(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_csa_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_sinkhorn(Rcpp::NumericMatrix cost, double lambda = 10.0,
                              double tol = 1e-9, int max_iter = 1000,
                              Rcpp::Nullable<Rcpp::NumericVector> r_weights = R_NilValue,
                              Rcpp::Nullable<Rcpp::NumericVector> c_weights = R_NilValue) {
  return solve_sinkhorn_impl(cost, lambda, tol, max_iter, r_weights, c_weights);
}

// [[Rcpp::export]]
Rcpp::IntegerVector sinkhorn_round(Rcpp::NumericMatrix P) {
  return sinkhorn_round_impl(P);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_ramshaw_tarjan(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_ramshaw_tarjan_impl(cost, maximize);
}

// =======================
// Greedy matching exports (implemented in solvers/greedy_matching.cpp)
// =======================

// [[Rcpp::export]]
Rcpp::List greedy_matching_sorted(Rcpp::NumericMatrix cost_matrix, bool maximize = false) {
  return greedy_matching_sorted_impl(cost_matrix, maximize);
}

// [[Rcpp::export]]
Rcpp::List greedy_matching_row_best(Rcpp::NumericMatrix cost_matrix, bool maximize = false) {
  return greedy_matching_row_best_impl(cost_matrix, maximize);
}

// [[Rcpp::export]]
Rcpp::List greedy_matching_pq(Rcpp::NumericMatrix cost_matrix, bool maximize = false) {
  return greedy_matching_pq_impl(cost_matrix, maximize);
}

// [[Rcpp::export]]
Rcpp::List greedy_matching(Rcpp::NumericMatrix cost_matrix, bool maximize = false,
                          std::string strategy = "row_best") {
  return greedy_matching_impl(cost_matrix, maximize, strategy);
}

// =======================
// Pixel morphing exports
// =======================

// [[Rcpp::export]]
Rcpp::List analyze_color_overlap_cpp(Rcpp::NumericVector pixelsA,
                                     Rcpp::NumericVector pixelsB,
                                     int H, int W,
                                     int quantize_bits = 5) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixelsA.size() != expected || pixelsB.size() != expected)
    Rcpp::stop("pixelsA and pixelsB must be H*W*3.");
  return analyze_color_overlap(pixelsA, pixelsB, H, W, quantize_bits);
}

// REMOVED: extract_patches_cpp - No longer needed with square tiling implementation

// REMOVED: compute_color_match_assignment_cpp - R does the assignment!
// REMOVED: compute_color_walk_assignment_cpp - R does the assignment!

// [[Rcpp::export]]
Rcpp::NumericMatrix compute_pixel_cost_cpp(const Rcpp::NumericVector& pixelsA,
                                           const Rcpp::NumericVector& pixelsB,
                                           int H, int W,
                                           double alpha, double beta) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixelsA.size() != expected || pixelsB.size() != expected)
    Rcpp::stop("pixelsA and pixelsB must be H*W*3.");
  return compute_pixel_cost(pixelsA, pixelsB, H, W, alpha, beta);
}

// [[Rcpp::export]]
Rcpp::NumericVector downscale_image_cpp(Rcpp::NumericVector pixels,
                                        int H, int W, int H_new, int W_new) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixels.size() != expected)
    Rcpp::stop("pixels must be H*W*3.");
  return downscale_image(pixels, H, W, H_new, W_new);
}

// [[Rcpp::export]]
Rcpp::IntegerVector upscale_assignment_cpp(Rcpp::IntegerVector assignment,
                                           int H_orig, int W_orig,
                                           int H_scaled, int W_scaled) {
  const int N_scaled = H_scaled * W_scaled;
  if (assignment.size() != N_scaled)
    Rcpp::stop("assignment must have H_scaled*W_scaled elements.");
  return upscale_assignment(assignment, H_orig, W_orig, H_scaled, W_scaled);
}

// [[Rcpp::export]]
Rcpp::List morph_pixel_level_cpp(Rcpp::NumericVector pixelsA,
                                 Rcpp::NumericVector pixelsB,
                                 Rcpp::IntegerVector assignment,
                                 int H, int W,
                                 int n_frames) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixelsA.size() != expected || pixelsB.size() != expected)
    Rcpp::stop("pixelsA and pixelsB must be H*W*3.");
  if (assignment.size() != N)
    Rcpp::stop("assignment must have H*W elements.");
  for (int i = 0; i < N; ++i) {
    if (assignment[i] < 0 || assignment[i] >= N) assignment[i] = i;
  }
  return morph_pixel_level_impl(pixelsA, pixelsB, assignment, H, W, n_frames);
}

// [[Rcpp::export]]
Rcpp::List color_palette_info_cpp(Rcpp::NumericVector pixelsA,
                                  Rcpp::NumericVector pixelsB,
                                  int H, int W,
                                  int quantize_bits = 5) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixelsA.size() != expected || pixelsB.size() != expected)
    Rcpp::stop("pixelsA and pixelsB must be H*W*3.");
  return color_palette_info(pixelsA, pixelsB, H, W, quantize_bits);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix spatial_cost_matrix_cpp(Rcpp::IntegerVector idxA,
                                            Rcpp::IntegerVector idxB,
                                            int H, int W) {
  return spatial_cost_matrix(idxA, idxB, H, W);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_cycle_cancel(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_cycle_cancel_impl(cost, maximize);
}


// =======================
// Gabow-Tarjan Module A exports (for testing)
// =======================

// [[Rcpp::export]]
long long gt_cost_length(long long c_ij, bool in_matching) {
    return cost_length(c_ij, in_matching);
}

// [[Rcpp::export]]
bool gt_is_eligible(long long c_ij, bool in_matching, 
                    long long yu, long long yv) {
    return is_eligible(c_ij, in_matching, yu, yv);
}

// [[Rcpp::export]]
bool gt_check_one_feasible(Rcpp::NumericMatrix cost_r,
                           Rcpp::IntegerVector row_match_r,
                           Rcpp::IntegerVector col_match_r,
                           Rcpp::NumericVector y_u_r,
                           Rcpp::NumericVector y_v_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();
    
    // Convert R objects to C++ types
    CostMatrix cost(n, std::vector<long long>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cost[i][j] = static_cast<long long>(cost_r(i, j));
        }
    }
    
    // Convert to 0-based indexing (R is 1-based)
    MatchVec row_match(n);
    for (int i = 0; i < n; ++i) {
        row_match[i] = row_match_r[i] - 1; // Convert 1-based to 0-based
    }
    
    MatchVec col_match(m);
    for (int j = 0; j < m; ++j) {
        col_match[j] = col_match_r[j] - 1; // Convert 1-based to 0-based
    }
    
    DualVec y_u(n);
    for (int i = 0; i < n; ++i) {
        y_u[i] = static_cast<long long>(y_u_r[i]);
    }
    
    DualVec y_v(m);
    for (int j = 0; j < m; ++j) {
        y_v[j] = static_cast<long long>(y_v_r[j]);
    }
    
    return check_one_feasible(cost, row_match, col_match, y_u, y_v);
}

// [[Rcpp::export]]
Rcpp::List gt_build_equality_graph(Rcpp::NumericMatrix cost_r,
                                    Rcpp::IntegerVector row_match_r,
                                    Rcpp::NumericVector y_u_r,
                                    Rcpp::NumericVector y_v_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();
    
    // Convert R objects to C++ types
    CostMatrix cost(n, std::vector<long long>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cost[i][j] = static_cast<long long>(cost_r(i, j));
        }
    }
    
    // Convert to 0-based indexing (R is 1-based)
    MatchVec row_match(n);
    for (int i = 0; i < n; ++i) {
        row_match[i] = row_match_r[i] - 1;
    }
    
    DualVec y_u(n);
    for (int i = 0; i < n; ++i) {
        y_u[i] = static_cast<long long>(y_u_r[i]);
    }
    
    DualVec y_v(m);
    for (int j = 0; j < m; ++j) {
        y_v[j] = static_cast<long long>(y_v_r[j]);
    }
    
    // Build equality graph
    std::vector<std::vector<int>> eq_graph = build_equality_graph(cost, row_match, y_u, y_v);
    
    // Convert to R list (convert back to 1-based indexing)
    Rcpp::List result(n);
    for (int i = 0; i < n; ++i) {
        Rcpp::IntegerVector row_edges(eq_graph[i].size());
        for (size_t k = 0; k < eq_graph[i].size(); ++k) {
            row_edges[k] = eq_graph[i][k] + 1; // Convert to 1-based
        }
        result[i] = row_edges;
    }
    
    return result;
}

// [[Rcpp::export]]
Rcpp::List gt_augment_along_path(Rcpp::IntegerMatrix edges_r,
                                  Rcpp::IntegerVector row_match_r,
                                  Rcpp::IntegerVector col_match_r) {
    int n_edges = edges_r.nrow();
    int n = row_match_r.size();
    int m = col_match_r.size();
    
    // Convert edges to C++ format (convert to 0-based)
    std::vector<std::pair<int,int>> edges;
    edges.reserve(n_edges);
    for (int k = 0; k < n_edges; ++k) {
        int i = edges_r(k, 0) - 1;  // Row index (convert to 0-based)
        int j = edges_r(k, 1) - 1;  // Column index (convert to 0-based)
        edges.emplace_back(i, j);
    }
    
    // Convert matches to C++ format (convert to 0-based)
    MatchVec row_match(n);
    for (int i = 0; i < n; ++i) {
        row_match[i] = row_match_r[i] - 1;
    }
    
    MatchVec col_match(m);
    for (int j = 0; j < m; ++j) {
        col_match[j] = col_match_r[j] - 1;
    }
    
    // Apply augmentation
    augment_along_path(edges, row_match, col_match);
    
    // Convert back to R format (1-based)
    Rcpp::IntegerVector row_match_out(n);
    for (int i = 0; i < n; ++i) {
        row_match_out[i] = row_match[i] + 1;
    }
    
    Rcpp::IntegerVector col_match_out(m);
    for (int j = 0; j < m; ++j) {
        col_match_out[j] = col_match[j] + 1;
    }
    
    return Rcpp::List::create(
        Rcpp::Named("row_match") = row_match_out,
        Rcpp::Named("col_match") = col_match_out
    );
}

// [[Rcpp::export]]
Rcpp::List gt_find_maximal_augmenting_paths(Rcpp::List eq_graph_r,
                                             Rcpp::IntegerVector row_match_r,
                                             Rcpp::IntegerVector col_match_r) {
    int n = eq_graph_r.size();
    
    // Convert equality graph to C++ format
    std::vector<std::vector<int>> eq_graph(n);
    for (int i = 0; i < n; ++i) {
        Rcpp::IntegerVector row_edges = eq_graph_r[i];
        eq_graph[i].reserve(row_edges.size());
        for (int k = 0; k < row_edges.size(); ++k) {
            eq_graph[i].push_back(row_edges[k] - 1);  // Convert to 0-based
        }
    }
    
    // Convert matches to C++ format
    MatchVec row_match(n);
    for (int i = 0; i < n; ++i) {
        row_match[i] = row_match_r[i] - 1;
    }
    
    int m = col_match_r.size();
    MatchVec col_match(m);
    for (int j = 0; j < m; ++j) {
        col_match[j] = col_match_r[j] - 1;
    }
    
    // Find paths
    auto paths = find_maximal_augmenting_paths(eq_graph, row_match, col_match);
    
    // Convert paths back to R format (list of matrices)
    Rcpp::List result(paths.size());
    for (size_t p = 0; p < paths.size(); ++p) {
        int path_len = paths[p].size();
        Rcpp::IntegerMatrix path_matrix(path_len, 2);
        for (int k = 0; k < path_len; ++k) {
            path_matrix(k, 0) = paths[p][k].first + 1;   // Convert to 1-based
            path_matrix(k, 1) = paths[p][k].second + 1;  // Convert to 1-based
        }
        result[p] = path_matrix;
    }
    
    return result;
}

// =======================
// Gabow-Tarjan Module E exports (for testing)
// =======================

// [[Rcpp::export]]
Rcpp::NumericMatrix gt_build_cl_matrix(Rcpp::NumericMatrix cost_r,
                                       Rcpp::IntegerVector row_match_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();
    
    // Convert R objects to C++ types
    CostMatrix cost(n, std::vector<long long>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cost[i][j] = static_cast<long long>(cost_r(i, j));
        }
    }
    
    // Convert to 0-based indexing (R is 1-based)
    MatchVec row_match(n);
    for (int i = 0; i < n; ++i) {
        row_match[i] = row_match_r[i] - 1;
    }
    
    // Build cost-length matrix
    CostMatrix C_cl = build_cl_matrix(cost, row_match);
    
    // Convert back to R matrix
    Rcpp::NumericMatrix result(n, m);
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            result(i, j) = static_cast<double>(C_cl[i][j]);
        }
    }
    
    return result;
}

// [[Rcpp::export]]
Rcpp::List gt_hungarian_step_one_feasible(Rcpp::NumericMatrix cost_r,
                                          Rcpp::IntegerVector row_match_r,
                                          Rcpp::IntegerVector col_match_r,
                                          Rcpp::NumericVector y_u_r,
                                          Rcpp::NumericVector y_v_r) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();
    
    // Convert R objects to C++ types
    CostMatrix cost(n, std::vector<long long>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cost[i][j] = static_cast<long long>(cost_r(i, j));
        }
    }
    
    // Convert to 0-based indexing (R is 1-based)
    MatchVec row_match(n);
    for (int i = 0; i < n; ++i) {
        row_match[i] = row_match_r[i] - 1;
    }
    
    MatchVec col_match(m);
    for (int j = 0; j < m; ++j) {
        col_match[j] = col_match_r[j] - 1;
    }
    
    DualVec y_u(n);
    for (int i = 0; i < n; ++i) {
        y_u[i] = static_cast<long long>(y_u_r[i]);
    }
    
    DualVec y_v(m);
    for (int j = 0; j < m; ++j) {
        y_v[j] = static_cast<long long>(y_v_r[j]);
    }
    
    // Run Hungarian step
    bool found = hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v);
    
    // Convert back to R format (1-based)
    Rcpp::IntegerVector row_match_out(n);
    for (int i = 0; i < n; ++i) {
        row_match_out[i] = row_match[i] + 1;
    }
    
    Rcpp::IntegerVector col_match_out(m);
    for (int j = 0; j < m; ++j) {
        col_match_out[j] = col_match[j] + 1;
    }
    
    Rcpp::NumericVector y_u_out(n);
    for (int i = 0; i < n; ++i) {
        y_u_out[i] = static_cast<double>(y_u[i]);
    }
    
    Rcpp::NumericVector y_v_out(m);
    for (int j = 0; j < m; ++j) {
        y_v_out[j] = static_cast<double>(y_v[j]);
    }
    
    return Rcpp::List::create(
        Rcpp::Named("found") = found,
        Rcpp::Named("row_match") = row_match_out,
        Rcpp::Named("col_match") = col_match_out,
        Rcpp::Named("y_u") = y_u_out,
        Rcpp::Named("y_v") = y_v_out
    );
}

// =======================
// Gabow-Tarjan Module F exports (for testing)
// =======================

// [[Rcpp::export]]
Rcpp::List gt_match_gt(Rcpp::NumericMatrix cost_r,
                       Rcpp::Nullable<Rcpp::IntegerVector> row_match_r = R_NilValue,
                       Rcpp::Nullable<Rcpp::IntegerVector> col_match_r = R_NilValue,
                       Rcpp::Nullable<Rcpp::NumericVector> y_u_r = R_NilValue,
                       Rcpp::Nullable<Rcpp::NumericVector> y_v_r = R_NilValue,
                       int max_iters = 1000,
                       bool check_feasible = false) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();
    
    // Convert cost matrix
    CostMatrix cost(n, std::vector<long long>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cost[i][j] = static_cast<long long>(cost_r(i, j));
        }
    }
    
    // Initialize or convert matching
    MatchVec row_match(n, NIL);
    MatchVec col_match(m, NIL);
    
    if (row_match_r.isNotNull()) {
        Rcpp::IntegerVector rm = row_match_r.get();
        for (int i = 0; i < n; ++i) {
            row_match[i] = rm[i] - 1;  // Convert to 0-based
        }
    }
    
    if (col_match_r.isNotNull()) {
        Rcpp::IntegerVector cm = col_match_r.get();
        for (int j = 0; j < m; ++j) {
            col_match[j] = cm[j] - 1;  // Convert to 0-based
        }
    }
    
    // Initialize or convert duals
    DualVec y_u(n, 0);
    DualVec y_v(m, 0);
    
    if (y_u_r.isNotNull()) {
        Rcpp::NumericVector yu = y_u_r.get();
        for (int i = 0; i < n; ++i) {
            y_u[i] = static_cast<long long>(yu[i]);
        }
    }
    
    if (y_v_r.isNotNull()) {
        Rcpp::NumericVector yv = y_v_r.get();
        for (int j = 0; j < m; ++j) {
            y_v[j] = static_cast<long long>(yv[j]);
        }
    }
    
    // Run match_gt
    try {
        match_gt(cost, row_match, col_match, y_u, y_v, max_iters, check_feasible);
    } catch (const std::exception& e) {
        Rcpp::stop(std::string("match_gt error: ") + e.what());
    }
    
    // Convert back to R format (1-based)
    Rcpp::IntegerVector row_match_out(n);
    for (int i = 0; i < n; ++i) {
        row_match_out[i] = row_match[i] + 1;
    }
    
    Rcpp::IntegerVector col_match_out(m);
    for (int j = 0; j < m; ++j) {
        col_match_out[j] = col_match[j] + 1;
    }
    
    Rcpp::NumericVector y_u_out(n);
    for (int i = 0; i < n; ++i) {
        y_u_out[i] = static_cast<double>(y_u[i]);
    }
    
    Rcpp::NumericVector y_v_out(m);
    for (int j = 0; j < m; ++j) {
        y_v_out[j] = static_cast<double>(y_v[j]);
    }
    
    return Rcpp::List::create(
        Rcpp::Named("row_match") = row_match_out,
        Rcpp::Named("col_match") = col_match_out,
        Rcpp::Named("y_u") = y_u_out,
        Rcpp::Named("y_v") = y_v_out
    );
}

// =======================
// Gabow-Tarjan Module G exports (for testing)
// =======================

// [[Rcpp::export]]
Rcpp::List scale_match_cpp(Rcpp::NumericMatrix cost_r,
                           Rcpp::Nullable<Rcpp::IntegerVector> row_match_r = R_NilValue,
                           Rcpp::Nullable<Rcpp::IntegerVector> col_match_r = R_NilValue,
                           Rcpp::Nullable<Rcpp::NumericVector> y_u_r = R_NilValue,
                           Rcpp::Nullable<Rcpp::NumericVector> y_v_r = R_NilValue) {
    int n = cost_r.nrow();
    int m = cost_r.ncol();
    
    // Convert cost matrix
    CostMatrix cost(n, std::vector<long long>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            cost[i][j] = static_cast<long long>(cost_r(i, j));
        }
    }
    
    // Initialize or convert matching
    MatchVec row_match(n, NIL);
    MatchVec col_match(m, NIL);
    
    if (row_match_r.isNotNull()) {
        Rcpp::IntegerVector rm = row_match_r.get();
        for (int i = 0; i < n; ++i) {
            row_match[i] = rm[i] - 1;  // Convert to 0-based
        }
    }
    
    if (col_match_r.isNotNull()) {
        Rcpp::IntegerVector cm = col_match_r.get();
        for (int j = 0; j < m; ++j) {
            col_match[j] = cm[j] - 1;  // Convert to 0-based
        }
    }
    
    // Initialize or convert duals
    DualVec y_u(n, 0);
    DualVec y_v(m, 0);
    
    if (y_u_r.isNotNull()) {
        Rcpp::NumericVector yu = y_u_r.get();
        for (int i = 0; i < n; ++i) {
            y_u[i] = static_cast<long long>(yu[i]);
        }
    }
    
    if (y_v_r.isNotNull()) {
        Rcpp::NumericVector yv = y_v_r.get();
        for (int j = 0; j < m; ++j) {
            y_v[j] = static_cast<long long>(yv[j]);
        }
    }
    
    // Run scale_match
    try {
        scale_match(cost, row_match, col_match, y_u, y_v);
    } catch (const std::exception& e) {
        Rcpp::stop(std::string("scale_match error: ") + e.what());
    }
    
    // Convert back to R format (1-based)
    Rcpp::IntegerVector row_match_out(n);
    for (int i = 0; i < n; ++i) {
        row_match_out[i] = row_match[i] + 1;
    }
    
    Rcpp::IntegerVector col_match_out(m);
    for (int j = 0; j < m; ++j) {
        col_match_out[j] = col_match[j] + 1;
    }
    
    Rcpp::NumericVector y_u_out(n);
    for (int i = 0; i < n; ++i) {
        y_u_out[i] = static_cast<double>(y_u[i]);
    }
    
    Rcpp::NumericVector y_v_out(m);
    for (int j = 0; j < m; ++j) {
        y_v_out[j] = static_cast<double>(y_v[j]);
    }
    
    return Rcpp::List::create(
        Rcpp::Named("row_match") = row_match_out,
        Rcpp::Named("col_match") = col_match_out,
        Rcpp::Named("y_u") = y_u_out,
        Rcpp::Named("y_v") = y_v_out
    );
}


