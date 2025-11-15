// src/rcpp_interface.cpp
// Exports for 4-mode pixel morphing + LAP solvers

// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <limits>
#include <string>
#include <algorithm>
#include "lap_internal.h"
#include "lap_utils.h"

using namespace Rcpp;

// =======================
// Forward decls for LAP solvers (no [[Rcpp::export]] here)
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
