#pragma once
// src/lap_internal.h
#pragma once
#include <Rcpp.h>
#include "lap_utils.h"
#include <string>

// preparation
Rcpp::List prepare_cost_matrix_impl(Rcpp::NumericMatrix cost, bool maximize);

// single-solution solvers (internal)
Rcpp::List solve_bruteforce_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_jv_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_hungarian_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_ssp_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_csflow_impl(Rcpp::NumericMatrix cost, bool maximize);
Rcpp::List solve_auction_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
Rcpp::List solve_auction_scaled_impl(Rcpp::NumericMatrix cost, bool maximize, std::string schedule);
Rcpp::List solve_auction_gauss_seidel_impl(Rcpp::NumericMatrix cost, bool maximize, double eps_in);
Rcpp::List solve_hk01_impl(Rcpp::NumericMatrix cost, bool maximize);

// k-best solvers (internal)
Rcpp::List solve_murty_impl(Rcpp::NumericMatrix cost, int k, bool maximize, std::string single_method);
Rcpp::List solve_kbest_lawler_impl(Rcpp::NumericMatrix cost, int k, std::string method_base, bool maximize);