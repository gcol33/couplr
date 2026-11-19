# A partial matching is certified over the units that can be matched, in every
# memory mode and at every point of match_path(): when each unit with an
# admissible partner is matched, the certificate is the one of the assignment
# problem over those units and names them in its scope; when Hall's condition
# fails among them, there is no certificate.

modes <- c("dense", "lazy", "implicit")

test_that("a matching of every unit with a partner is certified in each mode", {
  # The third left unit has no partner within max_distance, nor does the last
  # right unit; the other two left units can both be matched.
  left <- data.frame(id = 1:3, x = c(0, 1, 10))
  right <- data.frame(id = 11:14, x = c(0.1, 0.9, 2, 3))
  for (mode in modes) {
    m <- match_couples(left, right, vars = "x", max_distance = 1.5,
                       certify = TRUE, memory_mode = mode)
    g <- glance(m)
    expect_identical(g$n_pairs, 2L, info = mode)
    expect_equal(g$total_distance, 0.2, info = mode)
    expect_true(g$certified, info = mode)
    cert <- certificate(m)
    expect_s3_class(cert, "assignment_certificate")
    expect_identical(cert$arithmetic, "exact", info = mode)
    expect_identical(cert$max_suboptimality, 0, info = mode)
    expect_identical(cert$scope$left, c("1", "2"), info = mode)
    expect_identical(cert$scope$right, c("11", "12", "13"), info = mode)
    expect_identical(cert$scope$n_left, 3L, info = mode)
    expect_identical(cert$scope$n_right, 4L, info = mode)
    expect_output(print(cert), "over 2 of 3 left and 3 of 4 right units")
    expect_output(print(summary(cert)), "covers +2 of 3 left")
    expect_output(print(m), "yes, over 2 of 3 left")
    expect_true(is.na(potentials(m)$left[["3"]]), info = mode)
  }
})

test_that("a matching short of the units with partners has no certificate in each mode", {
  # The first two left units admit the first right unit only, so one of them
  # stays unmatched although it has an admissible partner.
  left <- data.frame(id = 1:3, x = c(0, 0.05, 5))
  right <- data.frame(id = 11:14, x = c(0.02, 5.1, 4.9, 9))
  for (mode in modes) {
    m <- match_couples(left, right, vars = "x", max_distance = 0.5,
                       certify = TRUE, memory_mode = mode)
    g <- glance(m)
    expect_identical(g$n_pairs, 2L, info = mode)
    expect_equal(g$total_distance, 0.02 + 0.1, info = mode)
    expect_null(certificate(m))
    expect_true(is.na(g$certified), info = mode)
  }
  # The implicit loop's witness is read in the caller's indices: left units 1
  # and 2 reach right unit 11 alone.
  m <- match_couples(left, right, vars = "x", max_distance = 0.5,
                     memory_mode = "implicit")
  expect_identical(m$witness$rows, 1:2)
  expect_identical(m$witness$cols, 1L)
  expect_true(m$witness$verified)
})

test_that("match_path() certifies a partial point as a single call does", {
  left <- data.frame(id = 1:3, x = c(0, 1, 10))
  right <- data.frame(id = 11:14, x = c(0.1, 0.9, 2, 3))
  p <- match_path(left, right, vars = "x", values = c(1.5, 20))
  expect_identical(p$path$status, c("partial", "optimal"))
  expect_identical(p$path$certified, c(TRUE, TRUE))
  cert <- certificate(p)[[1]]
  expect_identical(cert$arithmetic, "exact")
  expect_identical(cert$max_suboptimality, 0)
  expect_identical(cert$scope$left, c("1", "2"))
  expect_null(certificate(p)[[2]]$scope)

  left2 <- data.frame(id = 1:3, x = c(0, 0.05, 5))
  right2 <- data.frame(id = 11:14, x = c(0.02, 5.1, 4.9, 9))
  p2 <- match_path(left2, right2, vars = "x", values = c(0.5, 20))
  expect_identical(p2$path$status, c("partial", "optimal"))
  expect_identical(p2$path$certified, c(FALSE, TRUE))
  expect_null(certificate(p2)[[1]])
  expect_equal(p2$path$total_distance[1], 0.02 + 0.1)
})

test_that("a complete matching that leaves out a partnerless right unit has no scope", {
  # Every left unit is matched and the fourth right unit has no partner; the
  # certificate holds for the whole problem, and the path reports the same.
  left <- data.frame(id = 1:3, x = c(0, 1, 2))
  right <- data.frame(id = 11:14, x = c(0.1, 0.9, 2.2, 30))
  for (mode in modes) {
    m <- match_couples(left, right, vars = "x", max_distance = 1,
                       certify = TRUE, memory_mode = mode)
    expect_true(glance(m)$certified, info = mode)
    expect_null(certificate(m)$scope)
  }
})

test_that("dense, lazy, implicit and path agree on a random problem with a caliper", {
  set.seed(20261119)
  n_left <- 60
  n_right <- 150
  left <- data.frame(id = seq_len(n_left), x = rnorm(n_left, 0.6),
                     y = rnorm(n_left), z = runif(n_left))
  right <- data.frame(id = 1000 + seq_len(n_right), x = rnorm(n_right),
                      y = rnorm(n_right), z = runif(n_right))
  values <- c(0.3, 0.6, 0.8, 1.2)
  p <- match_path(left, right, vars = c("x", "y"), calipers = list(z = 0.3),
                  values = values)
  for (k in seq_along(values)) {
    mv <- p$match[[k]]
    rows <- which(mv > 0)
    path_pairs <- sort(paste(left$id[rows], right$id[mv[rows]]))
    for (mode in modes) {
      m <- suppressWarnings(match_couples(
        left, right, vars = c("x", "y"), max_distance = values[k],
        calipers = list(z = 0.3), certify = TRUE, memory_mode = mode))
      g <- glance(m)
      info <- paste(mode, values[k])
      expect_identical(isTRUE(g$certified), p$path$certified[k], info = info)
      expect_identical(sort(paste(m$pairs$left_id, m$pairs$right_id)),
                       path_pairs, info = info)
      expect_equal(g$total_distance, p$path$total_distance[k],
                   tolerance = 1e-12, info = info)
      expect_identical(certificate(m)$scope$left,
                       certificate(p)[[k]]$scope$left, info = info)
    }
  }
  # The sweep covers both kinds of partial point and a complete one.
  expect_identical(p$path$status, c("partial", "partial", "partial", "optimal"))
  expect_identical(p$path$certified, c(FALSE, TRUE, FALSE, TRUE))
})

test_that("a distance object carries the certificate and its scope", {
  left <- data.frame(id = 1:3, x = c(0, 1, 10))
  right <- data.frame(id = 11:14, x = c(0.1, 0.9, 2, 3))
  d <- compute_distances(left, right, vars = "x")
  complete <- match_couples(d, certify = TRUE)
  expect_true(glance(complete)$certified)
  expect_null(certificate(complete)$scope)
  partial <- match_couples(d, max_distance = 1.5, certify = TRUE)
  expect_true(glance(partial)$certified)
  expect_identical(certificate(partial)$scope$left, c("1", "2"))
  expect_identical(certificate(partial)$scope$right, c("11", "12", "13"))
})

test_that("blocking with certify = TRUE is refused", {
  left <- data.frame(id = 1:4, x = 1:4, b = c("a", "a", "b", "b"))
  right <- data.frame(id = 5:8, x = 1:4 + 0.1, b = c("a", "a", "b", "b"))
  expect_error(match_couples(left, right, vars = "x", block_id = "b",
                             certify = TRUE),
               "one certificate per block")
})
