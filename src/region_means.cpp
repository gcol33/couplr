// src/region_means.cpp
#include <Rcpp.h>
#ifdef _OPENMP
  #include <omp.h>
#endif
using namespace Rcpp;

// Implementation only. 1-based labels; labels<=0 are ignored.
// pixels: numeric (H*W*3), RGB in [0..255]
// labels: HxW integers in [1..K]
// Returns: list(col=Kx3 double, xy=Kx2 double)
List region_means_impl(const NumericVector& pixels,
                       const IntegerMatrix& labels,
                       const int H,
                       const int W,
                       const int K) {
  if (pixels.size() != H * W * 3)
    stop("pixels length mismatch (expected H*W*3).");
  if (labels.nrow() != H || labels.ncol() != W)
    stop("labels dims mismatch.");

  // accumulators
  std::vector<double> sumR(K, 0.0), sumG(K, 0.0), sumB(K, 0.0);
  std::vector<double> sumX(K, 0.0), sumY(K, 0.0);
  std::vector<double> cnt (K, 0.0);

  // stride helpers
  const int HW = H * W;

  // Parallel over columns or rows; each thread has private buffers, then reduce.
  #ifdef _OPENMP
  const int nthreads = omp_get_max_threads();
  #else
  const int nthreads = 1;
  #endif

  std::vector< std::vector<double> > tR(nthreads, std::vector<double>(K, 0.0));
  std::vector< std::vector<double> > tG(nthreads, std::vector<double>(K, 0.0));
  std::vector< std::vector<double> > tB(nthreads, std::vector<double>(K, 0.0));
  std::vector< std::vector<double> > tX(nthreads, std::vector<double>(K, 0.0));
  std::vector< std::vector<double> > tY(nthreads, std::vector<double>(K, 0.0));
  std::vector< std::vector<double> > tC(nthreads, std::vector<double>(K, 0.0));

  #pragma omp parallel for if(HW > 20000) schedule(static)
  for (int x = 0; x < W; ++x) {
    #ifdef _OPENMP
    const int tid = omp_get_thread_num();
    #else
    const int tid = 0;
    #endif
    for (int y = 0; y < H; ++y) {
      const int k = labels(y, x) - 1; // 1-based -> 0-based
      if (k < 0 || k >= K) continue;

      const int idx = y + x * H;     // column-major
      const double r = pixels[idx + 0 * HW];
      const double g = pixels[idx + 1 * HW];
      const double b = pixels[idx + 2 * HW];

      tR[tid][k] += r;
      tG[tid][k] += g;
      tB[tid][k] += b;
      tX[tid][k] += (x + 1);         // 1-based x
      tY[tid][k] += (y + 1);         // 1-based y
      tC[tid][k] += 1.0;
    }
  }

  // reduce
  for (int t = 0; t < nthreads; ++t) {
    for (int k = 0; k < K; ++k) {
      sumR[k] += tR[t][k];
      sumG[k] += tG[t][k];
      sumB[k] += tB[t][k];
      sumX[k] += tX[t][k];
      sumY[k] += tY[t][k];
      cnt [k] += tC[t][k];
    }
  }

  // build outputs
  NumericMatrix col(K, 3);
  NumericMatrix xy (K, 2);
  for (int k = 0; k < K; ++k) {
    const double c = cnt[k] > 0 ? cnt[k] : 1.0;
    col(k, 0) = sumR[k] / c;
    col(k, 1) = sumG[k] / c;
    col(k, 2) = sumB[k] / c;

    xy (k, 0) = sumX[k] / c;
    xy (k, 1) = sumY[k] / c;
  }

  return List::create(
    _["col"] = col,  // Kx3 RGB means (0..255)
    _["xy"]  = xy    // Kx2 centers (1-based)
  );
}
