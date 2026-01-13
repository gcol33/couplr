#include <Rcpp.h>
#include "../core/lap_internal.h"
#include "../core/lap_utils_rcpp.h"
using namespace Rcpp;

// Internal implementation only (no Rcpp export)
Rcpp::List prepare_cost_matrix_impl(Rcpp::NumericMatrix cost, bool maximize) {
  const int n = cost.nrow();
  const int m = cost.ncol();

  std::vector<double> rowmaj(n * m);
  std::vector<int>    mask(n * m, 0);

  double cmax = R_NegInf;

  // Convert column-major (R) to row-major buffer; NA -> +Inf and mask=1
  for (int j = 0; j < m; ++j) {
    for (int i = 0; i < n; ++i) {
      const int idx = i * m + j;  // row-major index
      double x = cost(i, j);
      if (Rcpp::NumericVector::is_na(x)) {
        rowmaj[idx] = R_PosInf;
        mask[idx]   = 1;
      } else {
        rowmaj[idx] = x;
        if (x > cmax) cmax = x;
      }
    }
  }

  // If maximize, flip finite costs: c' = cmax - c
  if (maximize && R_finite(cmax)) {
    for (int k = 0; k < n * m; ++k) {
      if (!R_finite(rowmaj[k])) continue; // keep +Inf for forbidden
      rowmaj[k] = cmax - rowmaj[k];
    }
  }

  return Rcpp::List::create(
    Rcpp::_["cost"] = Rcpp::NumericVector(rowmaj.begin(), rowmaj.end()),
    Rcpp::_["mask"] = Rcpp::IntegerVector(mask.begin(), mask.end()),
    Rcpp::_["n"]    = n,
    Rcpp::_["m"]    = m,
    Rcpp::_["cmax"] = cmax
  );
}
