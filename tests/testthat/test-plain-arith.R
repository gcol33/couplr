# The arithmetic behind the built-in distances: means, covariances, the
# precision matrix, scaling statistics and the per-pair kernels are plain
# double operations in a fixed order (src/core/lap_plain_arith.h), so their
# results are the same bits on every platform.
#
# The reference implementations below repeat that arithmetic one scalar
# operation at a time. R evaluates each scalar operation on its own, in
# doubles, so no operation is fused with another and no accumulator is wider
# than a double; agreement to the last bit therefore shows that the compiled
# code did neither.

# Covariates that are exact binary fractions, so they are the same doubles on
# every platform without going through a random number generator.
plain_units <- function(n, p, shift) {
  x <- matrix(0, n, p)
  for (i in seq_len(n)) for (k in seq_len(p)) {
    x[i, k] <- ((i * 7919 + k * 104729 + shift * 31 + i * i * k) %% 1009) / 16 -
      20 * k
  }
  x[, p] <- x[, p] * 512
  x
}

ref_mean <- function(blocks, k) {
  s <- 0
  n <- 0
  for (b in blocks) {
    for (i in seq_len(nrow(b))) s <- s + b[i, k]
    n <- n + nrow(b)
  }
  m <- s / n
  t <- 0
  for (b in blocks) for (i in seq_len(nrow(b))) t <- t + (b[i, k] - m)
  m + t / n
}

ref_cross <- function(blocks, a, b, ma, mb) {
  s <- 0
  for (blk in blocks) for (i in seq_len(nrow(blk))) {
    s <- s + (blk[i, a] - ma) * (blk[i, b] - mb)
  }
  s
}

ref_covariance <- function(groups, p) {
  S <- matrix(0, p, p)
  n <- 0
  for (g in groups) {
    n <- n + sum(vapply(g, nrow, 1L))
    m <- vapply(seq_len(p), function(k) ref_mean(g, k), 0)
    for (a in seq_len(p)) for (b in a:p) {
      S[a, b] <- S[a, b] + ref_cross(g, a, b, m[a], m[b])
    }
  }
  dof <- n - length(groups)
  for (a in seq_len(p)) for (b in a:p) {
    v <- S[a, b] / dof
    S[a, b] <- v
    S[b, a] <- v
  }
  S
}

ref_cholesky <- function(A) {
  n <- nrow(A)
  L <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(i)) {
    s <- 0.5 * (A[i, j] + A[j, i])
    for (k in seq_len(j - 1L)) s <- s - L[i, k] * L[j, k]
    L[i, j] <- if (i == j) sqrt(s) else s / L[j, j]
  }
  L
}

ref_precision <- function(A) {
  n <- nrow(A)
  L <- ref_cholesky(A)
  X <- matrix(0, n, n)
  for (j in seq_len(n)) {
    rhs <- numeric(n)
    rhs[j] <- 1
    for (i in seq_len(n)) {
      s <- rhs[i]
      for (k in seq_len(i - 1L)) s <- s - L[i, k] * X[k, j]
      X[i, j] <- s / L[i, i]
    }
  }
  P <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in i:n) {
    s <- 0
    for (k in j:n) s <- s + X[k, i] * X[k, j]
    P[i, j] <- s
    P[j, i] <- s
  }
  P
}

ref_mahalanobis <- function(left, right, P) {
  p <- ncol(left)
  D <- matrix(0, nrow(left), nrow(right))
  for (i in seq_len(nrow(left))) for (j in seq_len(nrow(right))) {
    s <- 0
    for (a in seq_len(p)) {
      da <- left[i, a] - right[j, a]
      if (da == 0) next
      row <- 0
      for (b in seq_len(p)) row <- row + P[a, b] * (left[i, b] - right[j, b])
      s <- s + da * row
    }
    D[i, j] <- sqrt(max(s, 0))
  }
  D
}

ref_euclidean <- function(left, right) {
  D <- matrix(0, nrow(left), nrow(right))
  for (i in seq_len(nrow(left))) for (j in seq_len(nrow(right))) {
    s <- 0
    for (k in seq_len(ncol(left))) {
      d <- left[i, k] - right[j, k]
      s <- s + d * d
    }
    D[i, j] <- sqrt(s)
  }
  D
}

test_that("the pooled covariance of a small case is the hand-computed one", {
  left <- matrix(c(1, 3, 2, 6), 2, 2)
  right <- matrix(c(0, 2, 4, 1, 2, 6), 3, 2)
  # Cross-products about the group means: left (2, 4; 4, 8), right
  # (8, 10; 10, 14); their sum over 5 - 2 degrees of freedom.
  expect_identical(couplr:::cpp_group_covariance(left, right),
                   matrix(c(10, 14, 14, 22) / 3, 2, 2))
})

test_that("the pooled covariance agrees with stats::cov and with the scalar reference", {
  left <- plain_units(23, 5, 1)
  right <- plain_units(41, 5, 2)
  S <- couplr:::cpp_group_covariance(left, right)
  pooled <- (22 * stats::cov(left) + 40 * stats::cov(right)) / 62
  expect_equal(S, pooled, tolerance = 1e-12)
  expect_identical(S, t(S))
  expect_identical(S, ref_covariance(list(list(left), list(right)), 5))
})

test_that("a group of one unit gives the covariance of the stacked sample", {
  left <- plain_units(1, 3, 1)
  right <- plain_units(12, 3, 2)
  S <- couplr:::cpp_group_covariance(left, right)
  expect_equal(S, unname(stats::cov(rbind(left, right))), tolerance = 1e-12)
  expect_identical(S, ref_covariance(list(list(left, right)), 3))
  expect_identical(couplr:::cpp_group_covariance(right, left),
                   ref_covariance(list(list(right, left)), 3))
})

test_that("the precision matrix of a small case is the hand-computed one", {
  A <- matrix(c(4, 2, 2, 3), 2, 2)
  # det 8, adjugate (3, -2; -2, 4)
  expect_equal(couplr:::mahalanobis_precision(A),
               matrix(c(3, -2, -2, 4) / 8, 2, 2), tolerance = 1e-15)
})

test_that("the precision matrix agrees with chol2inv and with the scalar reference", {
  S <- couplr:::cpp_group_covariance(plain_units(23, 5, 1), plain_units(41, 5, 2))
  P <- couplr:::mahalanobis_precision(S)
  expect_equal(P, chol2inv(chol(S)), tolerance = 1e-12)
  expect_identical(P, t(P))
  expect_identical(P, ref_precision(S))
})

test_that("the precision matrix keeps dimnames and reads the symmetric part", {
  A <- matrix(c(4, 2, 2, 3), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  expect_identical(dimnames(couplr:::mahalanobis_precision(A)), dimnames(A))
  B <- A
  B[1, 2] <- 2.5
  B[2, 1] <- 1.5
  expect_identical(couplr:::mahalanobis_precision(B), couplr:::mahalanobis_precision(A))
})

test_that("a matrix that is not positive definite is reported as singular", {
  expect_null(couplr:::cpp_precision_matrix(matrix(1, 2, 2)))
  expect_null(couplr:::cpp_precision_matrix(matrix(c(1, 2, 2, 1), 2, 2)))
  expect_null(couplr:::cpp_precision_matrix(matrix(c(1, NA, NA, 1), 2, 2)))
  expect_null(couplr:::cpp_precision_matrix(diag(c(1, 1e-17))))
  expect_error(couplr:::mahalanobis_precision(matrix(0, 3, 3)), "singular")
  expect_error(couplr:::mahalanobis_precision(matrix("a", 2, 2)), "singular")
  # Well inside working precision: reciprocal condition number 1e-6.
  expect_equal(couplr:::mahalanobis_precision(diag(c(1, 1e-12))),
               diag(c(1, 1e12)), tolerance = 1e-12)
})

test_that("Mahalanobis distances are the scalar reference to the last bit", {
  left <- plain_units(23, 5, 1)
  right <- plain_units(41, 5, 2)
  P <- ref_precision(ref_covariance(list(list(left), list(right)), 5))
  D <- couplr:::compute_distance_matrix(left, right, "mahalanobis")
  expect_identical(D, ref_mahalanobis(left, right, P))

  sigma <- stats::cov(rbind(left, right))
  expect_identical(
    couplr:::compute_distance_matrix(left, right, "mahalanobis", sigma = sigma),
    ref_mahalanobis(left, right, ref_precision(sigma)))
})

test_that("a lazy specification reads the same Mahalanobis matrix and distances", {
  left <- plain_units(23, 5, 1)
  right <- plain_units(41, 5, 2)
  spec <- couplr:::new_lazy_cost_spec(left, right, "mahalanobis", NULL, NULL,
                                      paste0("v", 1:5))
  expect_identical(couplr:::lazy_cost_spec_inv_cov(spec),
                   couplr:::mahalanobis_inv_cov(left, right))
  D <- couplr:::compute_distance_matrix(left, right, "mahalanobis")
  rows <- rep(seq_len(23), times = 41)
  cols <- rep(seq_len(41), each = 23)
  expect_identical(couplr:::lazy_pair_distances(spec, rows, cols), as.vector(D))

  expect_error(
    couplr:::lazy_cost_spec_inv_cov(
      couplr:::new_lazy_cost_spec(left, right, "mahalanobis", diag(2), NULL,
                                  paste0("v", 1:5))),
    "sigma must be a 5 x 5 matrix")
})

test_that("scaling statistics of a small case are the hand-computed ones", {
  left <- matrix(c(1, 2, 4, 10, 20, 40), 3, 2)
  right <- matrix(c(9, 100), 1, 2)
  # Column 1 is 1, 2, 4, 9: mean 4, squared deviations 9 + 4 + 0 + 25 = 38.
  st <- couplr:::cpp_scaling_statistics(left, right, "standardize")
  expect_identical(st$center[1], 4)
  expect_identical(st$scale[1], sqrt(38 / 3))

  rg <- couplr:::cpp_scaling_statistics(left, right, "range")
  expect_identical(rg$center, c(1, 10))
  expect_identical(rg$upper, c(9, 100))
  expect_identical(rg$scale, c(8, 90))

  # Median of 1, 2, 4, 9 is 3; absolute deviations 2, 1, 1, 6 have median 1.5.
  rb <- couplr:::cpp_scaling_statistics(left, right, "robust")
  expect_identical(rb$center[1], 3)
  expect_identical(rb$scale[1], 1.4826 * 1.5)
  # An odd count takes the middle value: 1, 2, 4.
  odd <- couplr:::cpp_scaling_statistics(left[, 1, drop = FALSE],
                                         matrix(0, 0, 1), "robust")
  expect_identical(odd$center, 2)
  expect_identical(odd$scale, 1.4826)

  expect_error(couplr:::cpp_scaling_statistics(left, right, "other"), "unknown method")
})

test_that("scaling statistics agree with base R", {
  left <- plain_units(23, 5, 1)
  right <- plain_units(40, 5, 2)
  X <- rbind(left, right)
  st <- couplr:::cpp_scaling_statistics(left, right, "standardize")
  expect_equal(st$center, colMeans(X), tolerance = 1e-12)
  expect_equal(st$scale, apply(X, 2, stats::sd), tolerance = 1e-12)
  rb <- couplr:::cpp_scaling_statistics(left, right, "robust")
  expect_equal(rb$center, apply(X, 2, stats::median), tolerance = 1e-12)
  expect_equal(rb$scale, apply(X, 2, stats::mad), tolerance = 1e-12)
  rg <- couplr:::cpp_scaling_statistics(left, right, "range")
  expect_identical(rg$center, apply(X, 2, min))
  expect_identical(rg$upper, apply(X, 2, max))
})

test_that("standardized Euclidean distances are the scalar reference to the last bit", {
  left <- plain_units(23, 5, 1)
  right <- plain_units(41, 5, 2)
  blocks <- list(left, right)
  n <- nrow(left) + nrow(right)
  centre <- vapply(1:5, function(k) ref_mean(blocks, k), 0)
  spread <- vapply(1:5, function(k) {
    sqrt(ref_cross(blocks, k, k, centre[k], centre[k]) / (n - 1))
  }, 0)
  rescale <- function(m) {
    for (k in 1:5) m[, k] <- (m[, k] - centre[k]) / spread[k]
    m
  }
  scaled <- couplr:::apply_scaling(left, right, "standardize")
  expect_identical(scaled$left, rescale(left))
  expect_identical(scaled$right, rescale(right))
  expect_identical(
    couplr:::compute_distance_matrix(scaled$left, scaled$right, "euclidean"),
    ref_euclidean(rescale(left), rescale(right)))
})

test_that("scaling parameters carry the variable names", {
  left <- plain_units(6, 2, 1)
  right <- plain_units(7, 2, 2)
  colnames(left) <- colnames(right) <- c("age", "income")
  for (m in c("standardize", "range", "robust")) {
    sc <- couplr:::apply_scaling(left, right, m)
    expect_identical(colnames(sc$left), c("age", "income"))
    for (stat in sc$params[-1]) expect_named(stat, c("age", "income"))
  }
})

test_that("the standard deviation of the distances is one number for a matrix and a specification", {
  left <- plain_units(23, 5, 1)
  right <- plain_units(41, 5, 2)
  D <- couplr:::compute_distance_matrix(left, right, "mahalanobis")
  s <- couplr:::cpp_finite_sd(D)
  expect_equal(s, stats::sd(D), tolerance = 1e-12)
  expect_identical(
    s,
    couplr:::cpp_lazy_distance_sd(left, right, "mahalanobis",
                                  couplr:::mahalanobis_inv_cov(left, right)))

  # Welford's update in row order, one scalar operation at a time.
  count <- 0
  mean <- 0
  ss <- 0
  for (i in seq_len(nrow(D))) for (j in seq_len(ncol(D))) {
    count <- count + 1
    delta <- D[i, j] - mean
    mean <- mean + delta / count
    ss <- ss + delta * (D[i, j] - mean)
  }
  expect_identical(s, sqrt(ss / (count - 1)))

  D[2, 3] <- Inf
  D[4, 1] <- NA
  expect_equal(couplr:::cpp_finite_sd(D), stats::sd(D[is.finite(D)]), tolerance = 1e-12)
  expect_true(is.na(couplr:::cpp_finite_sd(matrix(c(1, Inf), 1, 2))))
})

test_that("plain sums are the sum in index order", {
  x <- as.vector(plain_units(40, 3, 5)) / 7
  s <- 0
  for (v in x) s <- s + v
  expect_identical(couplr:::.plain_sum(x), s)
  expect_identical(couplr:::.plain_sum(c(x, NA), na.rm = TRUE), s)
  expect_identical(couplr:::.plain_sum(numeric(0)), 0)
  expect_identical(couplr:::.plain_sum(1:10), 55)

  m <- matrix(x, 40, 3)
  rows <- vapply(seq_len(40), function(i) {
    s <- 0
    for (k in 1:3) s <- s + m[i, k]
    s
  }, 0)
  expect_identical(couplr:::.plain_row_sums(m), rows)
  expect_equal(couplr:::.plain_row_sums(m), rowSums(m), tolerance = 1e-12)
})

test_that("the plain mean returns a constant exactly and agrees with mean()", {
  for (value in c(0.1, 1 / 3, 1e-7, 123456.789)) {
    for (n in c(2L, 3L, 7L, 10L, 185L, 429L)) {
      x <- rep(value, n)
      expect_identical(couplr:::.plain_mean(x), value)
      expect_identical(couplr:::.plain_sd(x), 0)
      wm <- couplr:::.weighted_moments(x, rep(1, n))
      expect_identical(wm$mean, value)
      expect_identical(wm$var, 0)
    }
  }
  x <- as.vector(plain_units(40, 3, 5)) / 7
  expect_equal(couplr:::.plain_mean(x), mean(x), tolerance = 1e-14)
  expect_equal(couplr:::.plain_sd(x), stats::sd(x), tolerance = 1e-12)
  expect_true(is.na(couplr:::.plain_mean(numeric(0))))
})

test_that("weighted moments are the hand-computed and the reference ones", {
  # 1, 2, 4, 9 with unit weights: mean 4, squared deviations 38, variance 38/3.
  wm <- couplr:::.weighted_moments(c(1, 2, 4, 9), rep(1, 4))
  expect_identical(wm$mean, 4)
  expect_equal(wm$var, 38 / 3, tolerance = 1e-15)
  expect_identical(wm$sd, sqrt(wm$var))

  # Weights 1, 3 on 2, 6: mean 5; sum(w) / (sum(w)^2 - sum(w^2)) = 4 / 6;
  # sum(w (x - mean)^2) = 9 + 3 = 12; variance 8.
  wm <- couplr:::.weighted_moments(c(2, 6), c(1, 3))
  expect_identical(wm$mean, 5)
  expect_equal(wm$var, 8, tolerance = 1e-15)

  x <- as.vector(plain_units(30, 2, 3)) / 3
  w <- rep(c(1, 2, 0.5), 20)
  wm <- couplr:::.weighted_moments(x, w)
  m <- sum(w * x) / sum(w)
  expect_equal(wm$mean, m, tolerance = 1e-13)
  expect_equal(wm$var, sum(w) / (sum(w)^2 - sum(w^2)) * sum(w * (x - m)^2),
               tolerance = 1e-12)

  # The scalar reference: one operation at a time, in index order.
  sw <- 0; swx <- 0; sww <- 0
  for (i in seq_along(x)) {
    sw <- sw + w[i]
    swx <- swx + w[i] * x[i]
    sww <- sww + w[i] * w[i]
  }
  m0 <- swx / sw
  t <- 0
  for (i in seq_along(x)) t <- t + w[i] * (x[i] - m0)
  m1 <- m0 + t / sw
  ss <- 0
  for (i in seq_along(x)) {
    d <- x[i] - m1
    ss <- ss + w[i] * (d * d)
  }
  expect_identical(wm$mean, m1)
  expect_identical(wm$var, sw / (sw * sw - sww) * ss)

  empty <- couplr:::.weighted_moments(numeric(0), numeric(0))
  expect_true(is.na(empty$mean) && is.na(empty$var) && is.na(empty$sd))
  one <- couplr:::.weighted_moments(3, 1)
  expect_identical(one$mean, 3)
  expect_true(is.na(one$var))
  none <- couplr:::.weighted_moments(c(1, 2), c(0, 0))
  expect_true(is.na(none$mean) && is.na(none$var))
})

test_that("the reported total distance is the plain sum of the pair distances", {
  left <- data.frame(id = 1:12, plain_units(12, 3, 1))
  right <- data.frame(id = 101:120, plain_units(20, 3, 2))
  m <- match_couples(left, right, vars = c("X1", "X2", "X3"),
                     distance = "mahalanobis")
  s <- 0
  for (d in m$pairs$distance) s <- s + d
  expect_identical(m$info$total_distance, s)
})
