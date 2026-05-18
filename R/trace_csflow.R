# ==============================================================================
# Reference Cost-Scaling Flow (successive shortest paths with Johnson
# potentials) with frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_csflow.cpp. Solves LAP as a unit-capacity min-cost
# max-flow on the standard bipartite graph
#
#         source --> rows --> cols --> sink
#
# using the Edmonds-Karp / Tomizawa 1971 successive-shortest-paths method
# with Johnson potentials: maintain node potentials h such that every
# residual edge has non-negative reduced cost c' = c - h[u] + h[v]. Find the
# shortest source->sink path under reduced costs with Dijkstra, push one
# unit of flow, then update h by adding the dist vector.
#
# Algorithmic outline:
#
#   1. Build the LAP-as-MCF graph (n + m + 2 nodes; row->col edges only
#      where c[i,j] is finite).
#   2. Initialise potentials h via Bellman-Ford from source - this handles
#      negative edge costs that arise under maximize (cost is negated).
#   3. Repeat n times:
#      a. Dijkstra on residual graph from source using reduced costs.
#      b. Walk back from sink to recover the augmenting path.
#      c. Push 1 unit of flow along the path.
#      d. h[v] += dist[v] for all reachable v.
#   4. Recover the row->col matching from saturated row->col edges.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_csflow <- function(cost, maximize = FALSE, ...) {
  v_in <- validate_cost_input(cost, "trace_csflow")
  cost_orig <- v_in$cost
  n <- v_in$n; m <- v_in$m
  if (n > m) {
    stop("trace_csflow: requires nrow <= ncol; got ", n, " x ", m, ".", call. = FALSE)
  }

  mcf <- build_lap_mcf(cost_orig, maximize = maximize)
  g <- mcf$graph

  # Initial potentials via Bellman-Ford (handles negative edges from maximize).
  bf <- mcf_bellman_ford(g, mcf$source)
  h <- ifelse(is.finite(bf$dist), bf$dist, 0)

  frames <- list()
  step <- 0L

  # External dual_u and dual_v from node potentials. For each row i, dual_u[i]
  # is h[row_node(i)] - h[source] (= the "row potential" from MCF perspective).
  # For each col j, dual_v[j] is h[sink] - h[col_node(j)].
  ext_duals <- function() {
    u <- numeric(n); v <- numeric(m)
    for (i in seq_len(n)) u[i] <- h[mcf$row_node(i)] - h[mcf$source]
    for (j in seq_len(m)) v[j] <- h[mcf$sink] - h[mcf$col_node(j)]
    list(u = u, v = v)
  }

  # Helper: translate an edge index into a user-visible (row, col) pair, or
  # NULL if it's not a row->col edge (source/sink edges aren't shown).
  edge_to_rowcol <- function(e_idx) {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        ee <- mcf$row_col_edge[i, j]
        if (!is.na(ee) && (ee == e_idx || g$edges[[ee]]$rev_idx == e_idx)) {
          return(c(i, j))
        }
      }
    }
    NULL
  }

  emit <- function(phase, description, active_edges = list(), path = list()) {
    step <<- step + 1L
    d <- ext_duals()
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = mcf_extract_matching(mcf),
      dual_u       = d$u,
      dual_v       = d$v,
      active_edges = active_edges,
      path         = path
    )
  }

  emit(
    "init",
    sprintf(
      paste0(
        "Built LAP-as-MCF graph: %d source-edges, %d row-col edges (only finite costs), ",
        "%d col-sink edges. Initialised potentials h via Bellman-Ford so every residual ",
        "edge has reduced cost = c - h[u] + h[v] >= 0."
      ),
      n, sum(!is.na(mcf$row_col_edge)), m
    )
  )

  # Main SSP loop: one unit of flow per iteration.
  for (iter in seq_len(n)) {
    dj <- mcf_dijkstra(g, mcf$source, h)
    if (!is.finite(dj$dist[mcf$sink])) {
      stop("trace_csflow: infeasible (no source->sink path found).", call. = FALSE)
    }

    aug_edges <- mcf_walk_back(dj$prev_node, dj$prev_edge,
                               source = mcf$source, sink = mcf$sink)
    # Convert to user-visible (row, col) path: drop source-edge and sink-edge,
    # keep only the row->col edge in the middle.
    path_rc <- list()
    for (e_idx in aug_edges) {
      rc <- edge_to_rowcol(e_idx)
      if (!is.null(rc)) path_rc[[length(path_rc) + 1L]] <- rc
    }

    emit(
      "dijkstra",
      sprintf(
        "Augmentation %d/%d: Dijkstra found shortest source->sink path of reduced length %.4g.",
        iter, n, dj$dist[mcf$sink]
      ),
      path = path_rc
    )

    # Push one unit of flow along the path. For unit-capacity LAP this is
    # always exactly 1.
    delta <- mcf_path_bottleneck(g, aug_edges)
    mcf_push_path(g, aug_edges, delta)

    # Update potentials: h[v] += dist[v] for all reachable v.
    for (v in seq_along(h)) {
      if (is.finite(dj$dist[v])) h[v] <- h[v] + dj$dist[v]
    }

    emit(
      "augment",
      sprintf(
        "Pushed %d unit of flow along the path; updated h by adding dist. Current matching size: %d.",
        delta, sum(mcf_extract_matching(mcf) > 0L)
      ),
      path = path_rc
    )
  }

  matching_final <- mcf_extract_matching(mcf)
  total <- matching_total_cost(cost_orig, matching_final)

  emit(
    "final",
    sprintf("All %d units of flow shipped. Total cost: %.6g.", n, total)
  )

  list(
    meta = make_meta(
      algorithm   = "csflow",
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost_orig,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Cost-scaling flow / successive shortest paths (Tomizawa 1971, Edmonds-Karp 1972). ",
        "Formulates LAP as a unit-capacity min-cost max-flow problem on the bipartite ",
        "graph source -> rows -> cols -> sink. Bellman-Ford supplies initial node ",
        "potentials so reduced costs are non-negative; then n Dijkstra augmentations ",
        "each push one unit of flow along the shortest reduced-cost path. Potentials are ",
        "updated additively after each augmentation - the Johnson trick keeps Dijkstra ",
        "valid even though raw edge costs may be negative."
      )
    ),
    frames = frames
  )
}

register_trace("csflow", trace_csflow)
