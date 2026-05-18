# ==============================================================================
# Shared min-cost-flow infrastructure for trace functions
# ==============================================================================
# Used by trace_csflow, trace_cycle_cancel, trace_push_relabel, trace_csa to
# represent the standard LAP-as-min-cost-flow graph:
#
#         source --(cap 1, cost 0)--> row_i --(cap 1, cost c[i,j])--> col_j
#                                                                       |
#                                                                       v
#                                          col_j --(cap 1, cost 0)--> sink
#
# We use the classic forward+reverse edge pair representation: every edge is
# stored alongside its residual reverse edge, with cap, cost, and a rev_idx
# cross-pointer. Pushing flow on edge e decrements e$cap and increments
# edges[[e$rev_idx]]$cap. Reverse-edge cost is the negation of the forward.
#
# The graph object lives in an environment so mutation is in-place; algorithms
# can grab a graph, run their inner loop, and read back state without copy.
# ==============================================================================

#' Construct an empty min-cost-flow graph
#'
#' Node indices are 1-based. Edges are added with add_mcf_edge(); each call
#' allocates a forward edge and a reverse residual edge, with cross-pointers.
#'
#' @keywords internal
#' @noRd
new_mcf_graph <- function(n_nodes) {
  g <- new.env(parent = emptyenv())
  g$n_nodes <- as.integer(n_nodes)
  g$edges <- list()
  g$out_edges <- vector("list", n_nodes)
  for (i in seq_len(n_nodes)) g$out_edges[[i]] <- integer(0)
  g
}

#' Add a directed edge (from -> to) with capacity and cost.
#'
#' Internally allocates the forward and reverse residual edges.
#' Returns the integer index of the forward edge (so callers can retrieve
#' flow afterwards as `orig_cap - edges[[idx]]$cap`).
#'
#' @keywords internal
#' @noRd
add_mcf_edge <- function(g, fr, to, cap, cost) {
  fr <- as.integer(fr); to <- as.integer(to)
  fwd_idx <- length(g$edges) + 1L
  rev_idx <- fwd_idx + 1L
  g$edges[[fwd_idx]] <- list(to = to, rev_idx = rev_idx,
                             cap = cap, cost = cost, orig_cap = cap)
  g$edges[[rev_idx]] <- list(to = fr, rev_idx = fwd_idx,
                             cap = 0,   cost = -cost, orig_cap = 0)
  g$out_edges[[fr]] <- c(g$out_edges[[fr]], fwd_idx)
  g$out_edges[[to]] <- c(g$out_edges[[to]], rev_idx)
  fwd_idx
}

#' Build the standard LAP-as-MCF graph
#'
#' Returns a list with:
#'   graph         - the mcf_graph environment
#'   source, sink  - node indices
#'   row_node(i)   - converter: row i (1..n)  -> graph node index
#'   col_node(j)   - converter: col j (1..m)  -> graph node index
#'   row_col_edge(i, j) - integer index of the (row_i -> col_j) edge, or NA
#'                       if forbidden (NA/Inf cost)
#'   n_orig, m_orig
#'
#' Sign convention for costs:
#'   maximize = FALSE: cost = c(i, j)
#'   maximize = TRUE:  cost = -c(i, j)
#' Forbidden entries (NA/Inf) are simply not added as edges.
#'
#' @keywords internal
#' @noRd
build_lap_mcf <- function(cost, maximize = FALSE) {
  n <- nrow(cost); m <- ncol(cost)
  source_node <- 1L
  row_node <- function(i) 1L + i
  col_node <- function(j) 1L + n + j
  sink_node <- 2L + n + m
  g <- new_mcf_graph(sink_node)

  # source -> row_i (cap 1, cost 0)
  for (i in seq_len(n)) add_mcf_edge(g, source_node, row_node(i), cap = 1L, cost = 0)
  # col_j -> sink (cap 1, cost 0)
  for (j in seq_len(m)) add_mcf_edge(g, col_node(j), sink_node, cap = 1L, cost = 0)
  # row_i -> col_j (cap 1, cost c[i,j]) for finite cells
  row_col_edge <- matrix(NA_integer_, nrow = n, ncol = m)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      x <- cost[i, j]
      if (is.finite(x)) {
        cc <- if (maximize) -x else x
        row_col_edge[i, j] <- add_mcf_edge(g, row_node(i), col_node(j), cap = 1L, cost = cc)
      }
    }
  }

  list(
    graph        = g,
    source       = source_node,
    sink         = sink_node,
    row_node     = row_node,
    col_node     = col_node,
    row_col_edge = row_col_edge,
    n_orig       = n,
    m_orig       = m,
    maximize     = maximize
  )
}

#' Recover row->col matching from an MCF graph after solving
#'
#' For each row i, finds the unique col_j such that the (row_i -> col_j) edge
#' is fully saturated (cap == 0 after pushing 1 unit). Returns integer vector
#' of length n_orig, with 0 for unmatched rows.
#'
#' @keywords internal
#' @noRd
mcf_extract_matching <- function(mcf) {
  n <- mcf$n_orig; m <- mcf$m_orig
  out <- integer(n)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      e_idx <- mcf$row_col_edge[i, j]
      if (!is.na(e_idx)) {
        e <- mcf$graph$edges[[e_idx]]
        if (e$orig_cap - e$cap > 0L) {   # carrying flow
          out[i] <- j
          break
        }
      }
    }
  }
  out
}

#' Strict matching extractor: zero out double-bookings
#'
#' Preflow algorithms (push-relabel, CSA) can have intermediate states where
#' multiple rows have saturated edges to the same column, with the column
#' holding the resulting excess until it pushes back. Standard
#' mcf_extract_matching would report a "matching" with the same column on
#' two rows. For visualisation we want a strictly-valid partial matching at
#' every frame: any column with more than one incoming saturated edge is
#' shown as unmatched (set to 0) on all the contending rows. Once the
#' algorithm settles (no node excess), this returns the true matching.
#'
#' @keywords internal
#' @noRd
mcf_extract_matching_strict <- function(mcf) {
  base <- mcf_extract_matching(mcf)
  matched <- base[base > 0L]
  if (length(matched) == 0L) return(base)
  tab <- tabulate(matched, nbins = mcf$m_orig)
  bad_cols <- which(tab > 1L)
  if (length(bad_cols) > 0L) {
    base[base %in% bad_cols] <- 0L
  }
  base
}

#' Reduced cost of residual edge e under node potentials h
#'
#' For augmenting-path / Johnson-potentials algorithms. The classical
#' reduced cost is cost + h(from) - h(to) (equivalently cost - h(to) + h(from))
#' and is guaranteed non-negative on residual edges when h is feasible
#' (e.g. h initialised via Bellman-Ford from the source).
#'
#' Sign-convention note: under this formula, after running Dijkstra with
#' reduced costs and updating h_new(v) = h(v) + dist(v), every residual
#' edge's new reduced cost equals old_rc + dist(from) - dist(to), which is
#' nonnegative by Dijkstra's relaxation - i.e. feasibility is preserved.
#'
#' @keywords internal
#' @noRd
residual_reduced_cost <- function(cost, h_from, h_to) {
  cost + h_from - h_to
}

#' Dijkstra on the residual graph with Johnson potentials
#'
#' Inputs:
#'   g       - mcf_graph environment
#'   source  - source node (1-based)
#'   h       - numeric vector of node potentials, length g$n_nodes
#'
#' Returns list(dist, prev_node, prev_edge), each of length g$n_nodes.
#' Unreachable nodes have dist = Inf and prev_node = 0L.
#'
#' Caller is responsible for ensuring h is feasible (all residual reduced
#' costs >= 0); for the very first augmentation, Bellman-Ford should be
#' used to initialise h.
#'
#' @keywords internal
#' @noRd
mcf_dijkstra <- function(g, source, h) {
  N <- g$n_nodes
  dist      <- rep(Inf, N)
  prev_node <- integer(N)
  prev_edge <- integer(N)
  visited   <- logical(N)
  dist[source] <- 0
  for (step in seq_len(N)) {
    unvis <- which(!visited)
    if (length(unvis) == 0L) break
    u <- unvis[which.min(dist[unvis])]
    if (!is.finite(dist[u])) break
    visited[u] <- TRUE
    for (e_idx in g$out_edges[[u]]) {
      e <- g$edges[[e_idx]]
      if (e$cap <= 0) next
      rc <- residual_reduced_cost(e$cost, h[u], h[e$to])
      # Defensive clamp: a tiny negative drift from FP error in h shouldn't
      # poison Dijkstra. If h is actually infeasible the bug is upstream.
      nd <- dist[u] + max(rc, 0)
      if (nd < dist[e$to]) {
        dist[e$to]      <- nd
        prev_node[e$to] <- u
        prev_edge[e$to] <- e_idx
      }
    }
  }
  list(dist = dist, prev_node = prev_node, prev_edge = prev_edge)
}

#' Bellman-Ford on the residual graph (for negative-cycle detection and
#' for initial potentials in min-cost flow).
#'
#' Returns list(dist, prev_node, prev_edge, neg_cycle_node). neg_cycle_node
#' is non-zero iff a negative cycle is reachable from the source - in which
#' case prev_node lets you walk it.
#'
#' @keywords internal
#' @noRd
mcf_bellman_ford <- function(g, source) {
  N <- g$n_nodes
  dist <- rep(Inf, N); prev_node <- integer(N); prev_edge <- integer(N)
  dist[source] <- 0
  last_relaxed <- 0L
  for (iter in seq_len(N)) {
    last_relaxed <- 0L
    for (u in seq_len(N)) {
      if (!is.finite(dist[u])) next
      for (e_idx in g$out_edges[[u]]) {
        e <- g$edges[[e_idx]]
        if (e$cap <= 0) next
        nd <- dist[u] + e$cost
        if (nd < dist[e$to] - 1e-12) {
          dist[e$to]      <- nd
          prev_node[e$to] <- u
          prev_edge[e$to] <- e_idx
          last_relaxed    <- e$to
        }
      }
    }
    if (last_relaxed == 0L) break
  }
  list(dist = dist, prev_node = prev_node, prev_edge = prev_edge,
       neg_cycle_node = last_relaxed)
}

#' Find a negative-cost cycle in the residual graph, or NULL if none.
#'
#' Used by Klein's cycle-canceling algorithm. The cycle is returned as an
#' integer vector of edge indices, in cycle order.
#'
#' @keywords internal
#' @noRd
mcf_find_negative_cycle <- function(g) {
  N <- g$n_nodes
  dist <- numeric(N)           # init to 0 everywhere -> finds any reachable neg cycle
  prev_node <- integer(N)
  prev_edge <- integer(N)
  last_relaxed <- 0L
  for (iter in seq_len(N)) {
    last_relaxed <- 0L
    for (u in seq_len(N)) {
      for (e_idx in g$out_edges[[u]]) {
        e <- g$edges[[e_idx]]
        if (e$cap <= 0) next
        nd <- dist[u] + e$cost
        if (nd < dist[e$to] - 1e-12) {
          dist[e$to]      <- nd
          prev_node[e$to] <- u
          prev_edge[e$to] <- e_idx
          last_relaxed    <- e$to
        }
      }
    }
    if (last_relaxed == 0L) return(NULL)   # no negative cycle
  }
  # Walk back N steps from last_relaxed to land inside the cycle
  v <- last_relaxed
  for (k in seq_len(N)) v <- prev_node[v]
  start <- v
  cycle_edges <- integer(0)
  repeat {
    cycle_edges <- c(prev_edge[v], cycle_edges)
    v <- prev_node[v]
    if (v == start) break
  }
  cycle_edges
}

#' Push delta units of flow along a path (sequence of residual edges)
#'
#' @keywords internal
#' @noRd
mcf_push_path <- function(g, edge_indices, delta) {
  for (e_idx in edge_indices) {
    g$edges[[e_idx]]$cap                          <- g$edges[[e_idx]]$cap - delta
    rev_idx <- g$edges[[e_idx]]$rev_idx
    g$edges[[rev_idx]]$cap                        <- g$edges[[rev_idx]]$cap + delta
  }
  invisible(NULL)
}

#' Bottleneck capacity along a path
#'
#' @keywords internal
#' @noRd
mcf_path_bottleneck <- function(g, edge_indices) {
  min(vapply(edge_indices, function(i) g$edges[[i]]$cap, numeric(1)))
}

#' Walk back the predecessor chain from sink to source, returning a vector of
#' edge indices in source->sink order (the augmenting path).
#'
#' @keywords internal
#' @noRd
mcf_walk_back <- function(prev_node, prev_edge, source, sink) {
  edges <- integer(0)
  v <- sink
  while (v != source) {
    e_idx <- prev_edge[v]
    if (e_idx == 0L) return(integer(0))   # no path
    edges <- c(e_idx, edges)
    v <- prev_node[v]
  }
  edges
}
