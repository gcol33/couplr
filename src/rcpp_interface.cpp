// src/rcpp_interface.cpp
// Exports for 4-mode pixel morphing + LAP solvers

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <limits>
#include <string>
#include <algorithm>
#include <cmath>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"
#include "../gabow_tarjan/utils_gabow_tarjan.h"

using namespace Rcpp;

// =======================
// Forward decl for greedy matching (implemented in solvers/greedy_matching.cpp)
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
Rcpp::List solve_push_relabel_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_jv_duals_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_jv_duals_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                    std::string metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                    double max_distance, Rcpp::List calipers,
                                    Rcpp::CharacterVector var_names, bool maximize);
Rcpp::List solve_network_simplex_rcpp(const Rcpp::NumericMatrix& cost_matrix);
// =======================
Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);
Rcpp::List probe_cost_matrix_impl(SEXP cost);
Rcpp::List solve_bruteforce_impl(NumericMatrix cost, bool maximize);
Rcpp::List solve_jv_impl(NumericMatrix cost, bool maximize);
Rcpp::List solve_jv_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              std::string metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers,
                              Rcpp::CharacterVector var_names, bool maximize);
Rcpp::List solve_auction_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                   std::string metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                   double max_distance, Rcpp::List calipers,
                                   Rcpp::CharacterVector var_names, bool maximize,
                                   Rcpp::Nullable<double> eps);
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
Rcpp::List solve_munkres_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_csflow_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_kbest_lawler_impl(Rcpp::NumericMatrix cost, int k, std::string method_base, bool maximize);
Rcpp::List solve_hk01_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_line_metric_impl(const Rcpp::NumericVector& x,
                                  const Rcpp::NumericVector& y,
                                  const std::string& cost,
                                  bool maximize);
Rcpp::List solve_ssap_bucket_impl(Rcpp::NumericMatrix cost, bool maximize);

// =======================
// Forward decls for the flow model (implemented in flow/flow_rcpp.cpp)
Rcpp::List flow_solve_impl(int n_nodes, Rcpp::NumericVector supply,
                           Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                           Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                           Rcpp::NumericVector cost,
                           Rcpp::NumericVector warm_flow,
                           Rcpp::NumericVector warm_potential,
                           double time_limit, double tol, double relax_eps,
                           double max_augmentations, bool return_potentials);
Rcpp::List flow_certify_impl(int n_nodes, Rcpp::NumericVector supply,
                             Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                             Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                             Rcpp::NumericVector cost, Rcpp::NumericVector flow,
                             Rcpp::NumericVector potential, double tol);
Rcpp::List flow_compile_full_match_impl(Rcpp::NumericMatrix cost,
                                        double min_controls, double max_controls);
Rcpp::List flow_compile_couples_impl(std::string design, double n_rows,
                                     double n_cols, double ratio);
Rcpp::List flow_trace_assignment_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List flow_trace_push_relabel_impl(Rcpp::NumericMatrix cost, bool maximize);

// Forward decls for the edge-generation loop (implemented in
// flow/flow_implicit_rcpp.cpp)
Rcpp::List implicit_dense_impl(Rcpp::NumericMatrix cost, bool maximize,
                               double keep_per_row, double width, double tol,
                               double max_rounds, bool certify);
Rcpp::List implicit_lazy_impl(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                              std::string distance,
                              Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                              double max_distance, Rcpp::List calipers,
                              Rcpp::CharacterVector vars, bool maximize,
                              double keep_per_row, double width, double tol,
                              double max_rounds, bool certify);

// Forward decl for the design path (implemented in flow/flow_path_rcpp.cpp)
Rcpp::List match_path_lazy_impl(Rcpp::NumericMatrix left_mat,
                                Rcpp::NumericMatrix right_mat,
                                std::string distance,
                                Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                Rcpp::NumericVector values, Rcpp::List calipers,
                                Rcpp::CharacterVector vars, bool maximize,
                                double keep_per_row, double width, double tol,
                                double max_rounds, bool certify);

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

// Takes SEXP rather than NumericMatrix so an integer cost matrix is read in
// place; a NumericMatrix parameter would coerce it and allocate the copy this
// probe exists to avoid.
// [[Rcpp::export]]
Rcpp::List lap_probe_cost_matrix(SEXP cost) {
  return probe_cost_matrix_impl(cost);
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
Rcpp::List cpp_lap_solve_jv_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                 std::string metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                 double max_distance, Rcpp::List calipers,
                                 Rcpp::CharacterVector var_names, bool maximize) {
  return solve_jv_lazy_impl(left_mat, right_mat, metric, inv_cov, max_distance,
                            calipers, var_names, maximize);
}

// [[Rcpp::export]]
Rcpp::List cpp_lap_solve_auction_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                      std::string metric, Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                      double max_distance, Rcpp::List calipers,
                                      Rcpp::CharacterVector var_names, bool maximize,
                                      Rcpp::Nullable<double> eps = R_NilValue) {
  return solve_auction_lazy_impl(left_mat, right_mat, metric, inv_cov, max_distance,
                                 calipers, var_names, maximize, eps);
}

// [[Rcpp::export]]
Rcpp::List lap_certify_dense(Rcpp::NumericMatrix cost, Rcpp::IntegerVector match,
                             Rcpp::NumericVector u, Rcpp::NumericVector v,
                             bool maximize, double tol,
                             std::string arithmetic) {
  return certify_dense_impl(cost, match, u, v, maximize, tol, arithmetic);
}

// [[Rcpp::export]]
Rcpp::List lap_certify_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                            std::string distance,
                            Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                            double max_distance, Rcpp::List calipers,
                            Rcpp::CharacterVector vars,
                            Rcpp::IntegerVector match, Rcpp::NumericVector u,
                            Rcpp::NumericVector v, bool maximize, double tol,
                            std::string arithmetic) {
  return certify_lazy_impl(left_mat, right_mat, distance, inv_cov, max_distance,
                           calipers, vars, match, u, v, maximize, tol, arithmetic);
}

// [[Rcpp::export]]
Rcpp::List lap_scan_reduced_costs(Rcpp::NumericMatrix cost, Rcpp::NumericVector u,
                                  Rcpp::NumericVector v, double tol) {
  return scan_reduced_costs_impl(cost, u, v, tol);
}

// [[Rcpp::export]]
Rcpp::List lap_hall_witness_dense(Rcpp::NumericMatrix cost) {
  return hall_witness_dense_impl(cost);
}

// [[Rcpp::export]]
Rcpp::List lap_hall_witness_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                                 std::string distance,
                                 Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                 double max_distance, Rcpp::List calipers,
                                 Rcpp::CharacterVector vars) {
  return hall_witness_lazy_impl(left_mat, right_mat, distance, inv_cov,
                                max_distance, calipers, vars);
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
    LAP_ERROR("Invalid schedule: '%s'. Use: 'alpha7', 'pow2', 'halves'.", schedule.c_str());
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
Rcpp::List lap_solve_munkres(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_munkres_impl(cost, maximize);
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

// [[Rcpp::export]]
Rcpp::List lap_solve_push_relabel(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_push_relabel_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_jv_duals(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_jv_duals_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List cpp_lap_solve_jv_duals_lazy(Rcpp::NumericMatrix left_mat,
                                       Rcpp::NumericMatrix right_mat,
                                       std::string metric,
                                       Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                                       double max_distance, Rcpp::List calipers,
                                       Rcpp::CharacterVector var_names, bool maximize) {
  return solve_jv_duals_lazy_impl(left_mat, right_mat, metric, inv_cov, max_distance,
                                  calipers, var_names, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_solve_network_simplex(Rcpp::NumericMatrix cost) {
  return solve_network_simplex_rcpp(cost);
}

// =======================
// Flow model exports
// =======================

// [[Rcpp::export]]
Rcpp::List lap_flow_solve(int n_nodes, Rcpp::NumericVector supply,
                          Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                          Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                          Rcpp::NumericVector cost,
                          Rcpp::NumericVector warm_flow,
                          Rcpp::NumericVector warm_potential,
                          double time_limit, double tol = 1e-12,
                          double relax_eps = 1e-18, double max_augmentations = 0.0,
                          bool return_potentials = true) {
  return flow_solve_impl(n_nodes, supply, tail, head, lower, upper, cost,
                         warm_flow, warm_potential, time_limit, tol, relax_eps,
                         max_augmentations, return_potentials);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_certify(int n_nodes, Rcpp::NumericVector supply,
                            Rcpp::IntegerVector tail, Rcpp::IntegerVector head,
                            Rcpp::NumericVector lower, Rcpp::NumericVector upper,
                            Rcpp::NumericVector cost, Rcpp::NumericVector flow,
                            Rcpp::NumericVector potential, double tol) {
  return flow_certify_impl(n_nodes, supply, tail, head, lower, upper, cost, flow,
                           potential, tol);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_compile_full_match(Rcpp::NumericMatrix cost,
                                       double min_controls, double max_controls) {
  return flow_compile_full_match_impl(cost, min_controls, max_controls);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_compile_couples(std::string design, double n_rows,
                                    double n_cols, double ratio = 1.0) {
  return flow_compile_couples_impl(design, n_rows, n_cols, ratio);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_trace_assignment(Rcpp::NumericMatrix cost,
                                     bool maximize = false) {
  return flow_trace_assignment_impl(cost, maximize);
}

// [[Rcpp::export]]
Rcpp::List lap_flow_trace_push_relabel(Rcpp::NumericMatrix cost,
                                       bool maximize = false) {
  return flow_trace_push_relabel_impl(cost, maximize);
}

// =======================
// Edge generation exports
// =======================
// The assignment over a complete implicit graph, solved by generating the pairs
// it turns out to need. The caller states the cost source and the search knobs;
// the loop keeps the problem, the candidate set and every round on the C++ side
// and crosses once.

// [[Rcpp::export]]
Rcpp::List lap_implicit_dense(Rcpp::NumericMatrix cost, bool maximize = false,
                              double keep_per_row = 5.0, double width = 5.0,
                              double tol = 1e-9, double max_rounds = 60.0,
                              bool certify = true) {
  return implicit_dense_impl(cost, maximize, keep_per_row, width, tol, max_rounds,
                             certify);
}

// [[Rcpp::export]]
Rcpp::List lap_implicit_lazy(Rcpp::NumericMatrix left_mat, Rcpp::NumericMatrix right_mat,
                             std::string distance,
                             Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                             double max_distance, Rcpp::List calipers,
                             Rcpp::CharacterVector vars, bool maximize = false,
                             double keep_per_row = 5.0, double width = 5.0,
                             double tol = 1e-9, double max_rounds = 60.0,
                             bool certify = true) {
  return implicit_lazy_impl(left_mat, right_mat, distance, inv_cov, max_distance,
                            calipers, vars, maximize, keep_per_row, width, tol,
                            max_rounds, certify);
}

// [[Rcpp::export]]
Rcpp::List lap_match_path_lazy(Rcpp::NumericMatrix left_mat,
                               Rcpp::NumericMatrix right_mat,
                               std::string distance,
                               Rcpp::Nullable<Rcpp::NumericMatrix> inv_cov,
                               Rcpp::NumericVector values, Rcpp::List calipers,
                               Rcpp::CharacterVector vars, bool maximize = false,
                               double keep_per_row = 5.0, double width = 5.0,
                               double tol = 1e-9, double max_rounds = 60.0,
                               bool certify = true) {
  return match_path_lazy_impl(left_mat, right_mat, distance, inv_cov, values,
                              calipers, vars, maximize, keep_per_row, width, tol,
                              max_rounds, certify);
}

// =======================
// Greedy matching exports (implemented in solvers/greedy_matching.cpp)
// =======================

// One front door: strategy selects the greedy variant. The per-strategy
// implementations live in solvers/greedy_matching.cpp (lap::greedy_matching_*)
// and are reached through the lap::greedy_matching dispatcher.
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
    LAP_ERROR("pixelsA and pixelsB must be H*W*3.");
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
    LAP_ERROR("pixelsA and pixelsB must be H*W*3.");
  return compute_pixel_cost(pixelsA, pixelsB, H, W, alpha, beta);
}

// [[Rcpp::export]]
Rcpp::NumericVector downscale_image_cpp(Rcpp::NumericVector pixels,
                                        int H, int W, int H_new, int W_new) {
  const int N = H * W;
  const int expected = N * 3;
  if (pixels.size() != expected)
    LAP_ERROR("pixels must be H*W*3.");
  return downscale_image(pixels, H, W, H_new, W_new);
}

// [[Rcpp::export]]
Rcpp::IntegerVector upscale_assignment_cpp(Rcpp::IntegerVector assignment,
                                           int H_orig, int W_orig,
                                           int H_scaled, int W_scaled) {
  const int N_scaled = H_scaled * W_scaled;
  if (assignment.size() != N_scaled)
    LAP_ERROR("assignment must have H_scaled*W_scaled elements.");
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
    LAP_ERROR("pixelsA and pixelsB must be H*W*3.");
  if (assignment.size() != N)
    LAP_ERROR("assignment must have H*W elements.");
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
    LAP_ERROR("pixelsA and pixelsB must be H*W*3.");
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
// Dense-scan successive shortest path export
// =======================

// Forward declaration for production solver
extern Rcpp::List sap_dense_solve_impl(Rcpp::NumericMatrix cost_r, bool maximize);

// [[Rcpp::export]]
Rcpp::List sap_dense_solve(Rcpp::NumericMatrix cost_r) {
    return sap_dense_solve_impl(cost_r, false);
}

// =======================
// Gabow-Tarjan test helpers
// =======================

namespace {

CostMatrix gt_cost_from_r(const Rcpp::NumericMatrix& cost) {
  const int n = cost.nrow();
  const int m = cost.ncol();
  CostMatrix out(n, std::vector<long long>(m, BIG_INT));
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      const double val = cost(i, j);
      out[i][j] = R_finite(val) ? static_cast<long long>(std::llround(val)) : BIG_INT;
    }
  }
  return out;
}

MatchVec gt_match_from_r(const Rcpp::IntegerVector& match) {
  MatchVec out(match.size(), NIL);
  for (int i = 0; i < match.size(); ++i) {
    if (match[i] != NA_INTEGER && match[i] > 0) {
      out[i] = match[i] - 1;
    }
  }
  return out;
}

DualVec gt_duals_from_r(const Rcpp::NumericVector& duals) {
  DualVec out(duals.size(), 0);
  for (int i = 0; i < duals.size(); ++i) {
    out[i] = static_cast<long long>(std::llround(duals[i]));
  }
  return out;
}

Rcpp::IntegerVector gt_match_to_r(const MatchVec& match) {
  Rcpp::IntegerVector out(match.size());
  for (int i = 0; i < static_cast<int>(match.size()); ++i) {
    out[i] = match[i] == NIL ? 0 : match[i] + 1;
  }
  return out;
}

Rcpp::NumericVector gt_duals_to_r(const DualVec& duals) {
  Rcpp::NumericVector out(duals.size());
  for (int i = 0; i < static_cast<int>(duals.size()); ++i) {
    out[i] = static_cast<double>(duals[i]);
  }
  return out;
}

// Column view of a row-side matching, which is what the solver reads.
MatchVec gt_col_match_of(const MatchVec& row_match, int m) {
  MatchVec out(m, NIL);
  for (int i = 0; i < static_cast<int>(row_match.size()); ++i) {
    const int j = row_match[i];
    if (j >= 0 && j < m) out[j] = i;
  }
  return out;
}

Rcpp::List gt_paths_to_r(const std::vector<std::vector<std::pair<int, int>>>& paths) {
  Rcpp::List out(paths.size());
  for (int p = 0; p < static_cast<int>(paths.size()); ++p) {
    Rcpp::IntegerMatrix mat(paths[p].size(), 2);
    for (int e = 0; e < static_cast<int>(paths[p].size()); ++e) {
      mat(e, 0) = paths[p][e].first + 1;
      mat(e, 1) = paths[p][e].second + 1;
    }
    out[p] = mat;
  }
  return out;
}

} // namespace

// [[Rcpp::export]]
long long gt_cost_length(long long c_ij, bool in_matching) {
  return cost_length(c_ij, in_matching);
}

// [[Rcpp::export]]
bool gt_is_eligible(long long c_ij, bool in_matching, long long y_u, long long y_v) {
  return is_eligible(c_ij, in_matching, y_u, y_v);
}

// [[Rcpp::export]]
bool gt_check_one_feasible(Rcpp::NumericMatrix cost,
                           Rcpp::IntegerVector row_match,
                           Rcpp::IntegerVector col_match,
                           Rcpp::NumericVector y_u,
                           Rcpp::NumericVector y_v) {
  (void)row_match;  // the matching is read from the column side
  return check_one_feasible(gt_cost_from_r(cost),
                            gt_match_from_r(col_match),
                            gt_duals_from_r(y_u),
                            gt_duals_from_r(y_v));
}

// [[Rcpp::export]]
Rcpp::List gt_build_equality_graph(Rcpp::NumericMatrix cost,
                                   Rcpp::IntegerVector row_match,
                                   Rcpp::NumericVector y_u,
                                   Rcpp::NumericVector y_v) {
  auto graph = build_equality_graph(gt_cost_from_r(cost),
                                    gt_col_match_of(gt_match_from_r(row_match),
                                                    cost.ncol()),
                                    gt_duals_from_r(y_u),
                                    gt_duals_from_r(y_v));
  Rcpp::List out(graph.size());
  for (int i = 0; i < static_cast<int>(graph.size()); ++i) {
    Rcpp::IntegerVector row(graph[i].size());
    for (int k = 0; k < static_cast<int>(graph[i].size()); ++k) {
      row[k] = graph[i][k] + 1;
    }
    out[i] = row;
  }
  return out;
}

// [[Rcpp::export]]
Rcpp::List gt_augment_along_path(Rcpp::IntegerMatrix edges,
                                 Rcpp::IntegerVector row_match,
                                 Rcpp::IntegerVector col_match) {
  std::vector<std::pair<int, int>> path;
  path.reserve(edges.nrow());
  for (int r = 0; r < edges.nrow(); ++r) {
    path.emplace_back(edges(r, 0) - 1, edges(r, 1) - 1);
  }
  MatchVec rows = gt_match_from_r(row_match);
  MatchVec cols = gt_match_from_r(col_match);
  augment_along_path(path, rows, cols);
  return Rcpp::List::create(
    Rcpp::Named("row_match") = gt_match_to_r(rows),
    Rcpp::Named("col_match") = gt_match_to_r(cols)
  );
}

// [[Rcpp::export]]
Rcpp::List gt_find_maximal_augmenting_paths(Rcpp::List eq_graph,
                                            Rcpp::IntegerVector row_match,
                                            Rcpp::IntegerVector col_match) {
  std::vector<std::vector<int>> graph(eq_graph.size());
  for (int i = 0; i < eq_graph.size(); ++i) {
    Rcpp::IntegerVector row = eq_graph[i];
    graph[i].reserve(row.size());
    for (int j : row) {
      graph[i].push_back(j - 1);
    }
  }
  (void)row_match;  // the search reads the matching from the column side
  auto paths = find_maximal_augmenting_paths(graph, gt_match_from_r(col_match));
  return gt_paths_to_r(paths);
}

// [[Rcpp::export]]
Rcpp::List gt_hungarian_step_one_feasible(Rcpp::NumericMatrix cost,
                                          Rcpp::IntegerVector row_match,
                                          Rcpp::IntegerVector col_match,
                                          Rcpp::NumericVector y_u,
                                          Rcpp::NumericVector y_v) {
  MatchVec rows = gt_match_from_r(row_match);
  MatchVec cols = gt_match_from_r(col_match);
  DualVec yu = gt_duals_from_r(y_u);
  DualVec yv = gt_duals_from_r(y_v);
  bool found = hungarian_step_one_feasible(gt_cost_from_r(cost), rows, cols, yu, yv);
  return Rcpp::List::create(
    Rcpp::Named("found") = found,
    Rcpp::Named("row_match") = gt_match_to_r(rows),
    Rcpp::Named("col_match") = gt_match_to_r(cols),
    Rcpp::Named("y_u") = gt_duals_to_r(yu),
    Rcpp::Named("y_v") = gt_duals_to_r(yv)
  );
}

// [[Rcpp::export]]
Rcpp::List gt_match_gt(Rcpp::NumericMatrix cost,
                       Rcpp::Nullable<Rcpp::IntegerVector> row_match = R_NilValue,
                       Rcpp::Nullable<Rcpp::IntegerVector> col_match = R_NilValue,
                       Rcpp::Nullable<Rcpp::NumericVector> y_u = R_NilValue,
                       Rcpp::Nullable<Rcpp::NumericVector> y_v = R_NilValue,
                       int max_iters = 1000,
                       bool check_feasible = false) {
  const int n = cost.nrow();
  const int m = cost.ncol();
  MatchVec rows = row_match.isNotNull()
    ? gt_match_from_r(Rcpp::IntegerVector(row_match))
    : MatchVec(n, NIL);
  MatchVec cols = col_match.isNotNull()
    ? gt_match_from_r(Rcpp::IntegerVector(col_match))
    : MatchVec(m, NIL);
  DualVec yu = y_u.isNotNull()
    ? gt_duals_from_r(Rcpp::NumericVector(y_u))
    : DualVec(n, 0);
  DualVec yv = y_v.isNotNull()
    ? gt_duals_from_r(Rcpp::NumericVector(y_v))
    : DualVec(m, 0);
  match_gt(gt_cost_from_r(cost), rows, cols, yu, yv, max_iters, check_feasible);
  return Rcpp::List::create(
    Rcpp::Named("row_match") = gt_match_to_r(rows),
    Rcpp::Named("col_match") = gt_match_to_r(cols),
    Rcpp::Named("y_u") = gt_duals_to_r(yu),
    Rcpp::Named("y_v") = gt_duals_to_r(yv)
  );
}

// [[Rcpp::export]]
Rcpp::List scale_match_cpp(Rcpp::NumericMatrix cost,
                           Rcpp::Nullable<Rcpp::IntegerVector> row_match = R_NilValue,
                           Rcpp::Nullable<Rcpp::IntegerVector> col_match = R_NilValue,
                           Rcpp::Nullable<Rcpp::NumericVector> y_u = R_NilValue,
                           Rcpp::Nullable<Rcpp::NumericVector> y_v = R_NilValue) {
  const int n = cost.nrow();
  const int m = cost.ncol();
  MatchVec rows = row_match.isNotNull()
    ? gt_match_from_r(Rcpp::IntegerVector(row_match))
    : MatchVec(n, NIL);
  MatchVec cols = col_match.isNotNull()
    ? gt_match_from_r(Rcpp::IntegerVector(col_match))
    : MatchVec(m, NIL);
  DualVec yu = y_u.isNotNull()
    ? gt_duals_from_r(Rcpp::NumericVector(y_u))
    : DualVec(n, 0);
  DualVec yv = y_v.isNotNull()
    ? gt_duals_from_r(Rcpp::NumericVector(y_v))
    : DualVec(m, 0);
  scale_match(gt_cost_from_r(cost), rows, cols, yu, yv);
  return Rcpp::List::create(
    Rcpp::Named("row_match") = gt_match_to_r(rows),
    Rcpp::Named("col_match") = gt_match_to_r(cols),
    Rcpp::Named("y_u") = gt_duals_to_r(yu),
    Rcpp::Named("y_v") = gt_duals_to_r(yv)
  );
}

