# ==============================================================================
# The potential-update rule the min-cost-flow traces share
# ==============================================================================
# R/trace_helpers_mcf.R states the same algorithm src/flow/flow_solve.cpp
# states. Both keep Johnson potentials feasible by shifting unreached nodes as
# well as reached ones; a trace that shifts only what Dijkstra labelled goes
# dual-infeasible on any column no admissible pair can reach, and can then show
# an augmenting path the solver would not have taken.
# ==============================================================================

# Reduced cost of every residual edge under potentials h.
.mcf_all_residual_reduced_costs <- function(g, h) {
  vals <- list()
  for (u in seq_len(g$n_nodes)) {
    for (e_idx in g$out_edges[[u]]) {
      e <- g$edges[[e_idx]]
      if (e$cap <= 0) next
      vals[[length(vals) + 1L]] <-
        couplr:::residual_reduced_cost(e$cost, h[u], h[e$to])
    }
  }
  unlist(vals, use.names = FALSE)
}

test_that("mcf_update_potentials shifts reached and unreached nodes apart", {
  h <- c(0, 0, 0, 0)
  dist <- c(0, 2, Inf, 5)

  out <- couplr:::mcf_update_potentials(h, dist)

  expect_equal(out[c(1, 2, 4)], c(0, 2, 5))
  expect_equal(out[3], 5)  # the largest label the search produced
})

test_that("an all-unreached search shifts nothing", {
  expect_equal(couplr:::mcf_update_potentials(c(1, 2), c(Inf, Inf)), c(1, 2))
})

test_that("a negative label never drags an unreached node below zero shift", {
  # The shift is bounded below by zero, so a search whose labels are all
  # negative leaves unreached nodes where they were.
  expect_equal(couplr:::mcf_update_potentials(c(0, 0), c(-3, Inf)), c(-3, 0))
})

test_that("every residual edge stays priced non-negative across a whole solve", {
  # Column 3 is forbidden for both rows, so no augmenting path ever reaches it,
  # while its edge into the sink stays residual for the whole solve. That edge
  # is the one the rule exists for.
  cost <- matrix(c(
    1, 5, Inf,
    4, 2, Inf
  ), nrow = 2, byrow = TRUE)

  mcf <- couplr:::build_lap_mcf(cost)
  g <- mcf$graph

  bf <- couplr:::mcf_bellman_ford(g, mcf$source)
  h <- couplr:::mcf_update_potentials(numeric(g$n_nodes), bf$dist)
  expect_true(all(.mcf_all_residual_reduced_costs(g, h) >= -1e-9))

  for (iter in seq_len(nrow(cost))) {
    dj <- couplr:::mcf_dijkstra(g, mcf$source, h)
    expect_true(is.finite(dj$dist[mcf$sink]))

    path <- couplr:::mcf_walk_back(dj$prev_node, dj$prev_edge,
                                   source = mcf$source, sink = mcf$sink)
    couplr:::mcf_push_path(g, path, couplr:::mcf_path_bottleneck(g, path))
    h <- couplr:::mcf_update_potentials(h, dj$dist)

    expect_true(all(.mcf_all_residual_reduced_costs(g, h) >= -1e-9),
                info = paste("augmentation", iter))
  }

  expect_equal(couplr:::mcf_extract_matching(mcf), c(1L, 2L))
})

test_that("the csflow trace solves a problem with an unreachable column", {
  cost <- matrix(c(
    1, 5, Inf,
    4, 2, Inf
  ), nrow = 2, byrow = TRUE)

  trace <- couplr:::trace_csflow(cost)
  final <- trace$frames[[length(trace$frames)]]$matching

  expect_equal(final, c(1L, 2L))
  expect_equal(sum(cost[cbind(seq_len(2), final)]),
               assignment(cost, method = "jv")$total_cost)
})
