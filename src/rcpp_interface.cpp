// [[Rcpp::plugins(cpp17)]]
#include <Rcpp.h>
#include <limits>
#include "lap_internal.h"
#include "lap_utils.h"
using namespace Rcpp;

Rcpp::List prepare_cost_matrix_impl(NumericMatrix cost, bool maximize);
Rcpp::List solve_bruteforce_impl(NumericMatrix cost, bool maximize);

// already present:
// [[Rcpp::export]]
Rcpp::List lap_prepare_cost_matrix(NumericMatrix cost, bool maximize) {
  return prepare_cost_matrix_impl(cost, maximize);
}

// new export for the brute-force solver
// [[Rcpp::export]]
Rcpp::List lap_solve_bruteforce(NumericMatrix cost, bool maximize) {
  return solve_bruteforce_impl(cost, maximize);
}
// forward declarations already present above
Rcpp::List solve_jv_impl(NumericMatrix cost, bool maximize);

// [[Rcpp::export]]
Rcpp::List lap_solve_jv(NumericMatrix cost, bool maximize) {
  return solve_jv_impl(cost, maximize);
}
// forward decl
Rcpp::List solve_murty_impl(Rcpp::NumericMatrix cost, int k, bool maximize, std::string single_method);

// [[Rcpp::export]]
Rcpp::List lap_kbest_murty(Rcpp::NumericMatrix cost, int k, bool maximize, std::string single_method = "jv") {
  return solve_murty_impl(cost, k, maximize, single_method);
}

// --- Auction (fixed epsilon) ---
Rcpp::List solve_auction_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
// legacy two-arg kept in solve_auction.cpp

// [[Rcpp::export]]
Rcpp::List lap_solve_auction(Rcpp::NumericMatrix cost, bool maximize,
                             Rcpp::Nullable<double> eps = R_NilValue) {
  double eps_in = std::numeric_limits<double>::quiet_NaN();
  if (eps.isNotNull()) eps_in = Rcpp::as<double>(eps.get());
  return solve_auction_impl(cost, maximize, eps_in);
}

// --- Auction (scaled epsilon) ---
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize, std::string schedule);

// [[Rcpp::export]]
Rcpp::List lap_solve_auction_scaled(Rcpp::NumericMatrix cost, bool maximize,
                                    std::string schedule = "pow2") {
  return solve_auction_scaled_impl(cost, maximize, schedule);
}

// --- Auction (Gauss-Seidel) ---
Rcpp::List solve_auction_gauss_seidel_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);

// [[Rcpp::export]]
Rcpp::List lap_solve_auction_gs(Rcpp::NumericMatrix cost, bool maximize,
                                Rcpp::Nullable<double> eps = R_NilValue) {
  double eps_in = std::numeric_limits<double>::quiet_NaN();
  if (eps.isNotNull()) eps_in = Rcpp::as<double>(eps.get());
  return solve_auction_gauss_seidel_impl(cost, maximize, eps_in);
}

Rcpp::List solve_ssp_impl(Rcpp::NumericMatrix cost, bool maximize);

// [[Rcpp::export]]
Rcpp::List lap_solve_ssp(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_ssp_impl(cost, maximize);
}
Rcpp::List solve_hungarian_impl(Rcpp::NumericMatrix cost, bool maximize);

// [[Rcpp::export]]
Rcpp::List lap_solve_hungarian(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_hungarian_impl(cost, maximize);
}
// forward declaration (defined in solve_csflow.cpp)
Rcpp::List solve_csflow_impl(Rcpp::NumericMatrix cost, bool maximize);

// [[Rcpp::export]]
Rcpp::List lap_solve_csflow(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_csflow_impl(cost, maximize);
}

// forward declaration
Rcpp::List solve_kbest_lawler_impl(Rcpp::NumericMatrix cost, int k,
                                   std::string method_base, bool maximize);

// [[Rcpp::export]]
Rcpp::List lap_kbest_lawler(Rcpp::NumericMatrix cost, int k,
                            std::string method_base = "jv", bool maximize = false) {
  return solve_kbest_lawler_impl(cost, k, method_base, maximize);
}

// forward declaration (defined in solve_hk01.cpp)
Rcpp::List solve_hk01_impl(Rcpp::NumericMatrix cost, bool maximize);

// [[Rcpp::export]]
Rcpp::List lap_solve_hk01(Rcpp::NumericMatrix cost, bool maximize) {
  return solve_hk01_impl(cost, maximize);
}