suppressPackageStartupMessages({
  library(microbenchmark)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
})

repo_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", winslash = "/", mustWork = TRUE)
} else if (basename(getwd()) == "paper" && file.exists("../DESCRIPTION")) {
  normalizePath("..", winslash = "/", mustWork = TRUE)
} else {
  stop("Run this script from the package root or the paper directory.")
}

pkgload::load_all(repo_root, quiet = TRUE)

TIMES    <- 5L
MAX_COST <- 10000L
SEED     <- 42L

BENCHMARK_TABLE <- file.path(repo_root, "paper", "benchmark-table.csv")
BENCHMARK_FIGURE <- file.path(repo_root, "paper", "figures", "benchmark.png")

# ---------- metadata -------------------------------------------------------

method_family <- c(
  hungarian       = "Classical",
  munkres         = "Classical",
  jv              = "JV / Augmenting path",
  sap             = "JV / Augmenting path",
  lapmod          = "JV / Augmenting path",
  auction         = "Auction",
  auction_gs      = "Auction",
  auction_scaled  = "Auction",
  csa             = "Cost scaling",
  gabow_tarjan    = "Cost scaling",
  ssap_bucket     = "Cost scaling",
  csflow          = "Flow-based",
  cycle_cancel    = "Flow-based",
  network_simplex = "Flow-based",
  push_relabel    = "Flow-based",
  orlin           = "Flow-based",
  ramshaw_tarjan  = "Rectangular / general",
  hk01            = "Special-purpose",
  bruteforce      = "Special-purpose",
  auto            = "Auto"
)

method_labels <- c(
  hungarian       = "Hungarian",
  munkres         = "Munkres",
  jv              = "JV",
  sap             = "SAP",
  lapmod          = "LAPMOD",
  auction         = "Auction",
  auction_gs      = "Auction-GS",
  auction_scaled  = "Auction-S",
  csa             = "CSA",
  gabow_tarjan    = "Gabow-T",
  ssap_bucket     = "SSAP-B",
  csflow          = "CS-flow",
  cycle_cancel    = "Cycle-C",
  network_simplex = "Net-S",
  push_relabel    = "Push-R",
  orlin           = "Orlin",
  ramshaw_tarjan  = "Ramshaw",
  hk01            = "HK-01",
  bruteforce      = "Brute-F",
  auto            = "auto"
)

family_colors <- c(
  "Classical"              = "#E53935",
  "JV / Augmenting path"  = "#1E88E5",
  "Auction"               = "#FB8C00",
  "Cost scaling"          = "#43A047",
  "Flow-based"            = "#8E24AA",
  "Rectangular / general" = "#00ACC1",
  "Special-purpose"       = "#6D4C41"
)

# Slow methods are capped so the published benchmark reaches n = 5000 for the
# scalable solvers without letting cubic/general-flow baselines dominate runtime.
method_max_n <- c(
  auction         = 1000L,
  auction_gs      = 1000L,
  auction_scaled  = 1000L,
  gabow_tarjan    = 1000L,
  hungarian       = 2000L,
  munkres         = 500L,
  sap             = 2000L,
  ssap_bucket     = 2000L,
  csflow          = 1000L,
  cycle_cancel    = 1000L,
  network_simplex = 1000L,
  orlin           = 1000L,
  push_relabel    = 1000L,
  ramshaw_tarjan  = 1000L
)

# ---------- benchmark -------------------------------------------------------

general_sizes   <- c(10L, 25L, 50L, 100L, 200L, 500L, 1000L, 2000L, 5000L)
small_sizes     <- c(4L, 6L, 8L)
binary_sizes    <- general_sizes
general_methods <- setdiff(names(method_family), c("bruteforce", "hk01", "auto"))

save_results <- function(results, path) {
  df <- do.call(rbind, results)
  df$family <- method_family[df$method]
  df$label  <- method_labels[df$method]
  df <- df[order(df$method, df$n), c("method", "label", "family", "n", "median_ms")]
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.csv(df, path, row.names = FALSE)
  invisible(df)
}

record_result <- function(result) {
  if (!is.null(result)) {
    results[[length(results) + 1L]] <<- result
    save_results(results, BENCHMARK_TABLE)
  }
}

bench_one <- function(method, n, times, cost_mat) {
  tryCatch({
    invisible(assignment(cost_mat, method = method))
    mb <- microbenchmark(assignment(cost_mat, method = method),
                         times = times, unit = "ms")
    data.frame(method = method, n = n, median_ms = summary(mb)$median,
               stringsAsFactors = FALSE)
  }, error = function(e) {
    message("  SKIP ", method, " n=", n, ": ", conditionMessage(e))
    NULL
  })
}

results <- list()
if (file.exists(BENCHMARK_TABLE)) {
  old <- read.csv(BENCHMARK_TABLE, stringsAsFactors = FALSE)
  old <- old[, c("method", "n", "median_ms")]
  results <- unname(split(old, seq_len(nrow(old))))
  cat("Loaded ", length(results), " existing benchmark rows from ",
      BENCHMARK_TABLE, "\n", sep = "")
  flush.console()
}

has_result <- function(method, n) {
  any(vapply(results, function(x) x$method == method && x$n == n, logical(1)))
}

cat("--- General methods ---\n"); flush.console()
for (n in general_sizes) {
  set.seed(SEED)
  cost <- matrix(sample.int(MAX_COST, n * n, replace = TRUE), n, n)
  for (m in general_methods) {
    max_n <- method_max_n[m]
    if (!is.na(max_n) && n > max_n) next
    if (has_result(m, n)) next
    cat(sprintf("  n=%3d  %s\n", n, m)); flush.console()
    r <- bench_one(m, n, TIMES, cost)
    record_result(r)
  }
}

cat("--- Auto method ---\n"); flush.console()
for (n in general_sizes) {
  set.seed(SEED)
  cost <- matrix(sample.int(MAX_COST, n * n, replace = TRUE), n, n)
  if (has_result("auto", n)) next
  cat(sprintf("  n=%3d  auto\n", n)); flush.console()
  r <- bench_one("auto", n, TIMES, cost)
  record_result(r)
}

cat("--- Bruteforce (n <= 8) ---\n"); flush.console()
for (n in small_sizes) {
  set.seed(SEED)
  cost <- matrix(sample.int(MAX_COST, n * n, replace = TRUE), n, n)
  if (has_result("bruteforce", n)) next
  cat(sprintf("  n=%d  bruteforce\n", n)); flush.console()
  r <- bench_one("bruteforce", n, TIMES, cost)
  record_result(r)
}

cat("--- HK-01 (binary costs) ---\n"); flush.console()
for (n in binary_sizes) {
  set.seed(SEED)
  cost <- matrix(sample(0L:1L, n * n, replace = TRUE), n, n)
  if (has_result("hk01", n)) next
  cat(sprintf("  n=%3d  hk01\n", n)); flush.console()
  r <- bench_one("hk01", n, TIMES, cost)
  record_result(r)
}

df <- save_results(results, BENCHMARK_TABLE)
cat("Saved ", BENCHMARK_TABLE, "\n", sep = ""); flush.console()

# Keep the figure layout independent of the benchmark run by plotting only from
# the persisted table.
df <- read.csv(BENCHMARK_TABLE, stringsAsFactors = FALSE)

# ---------- shared scales / theme ------------------------------------------

x_breaks <- c(10, 100, 1000)
x_lbls   <- c("10", "100", "1000")
y_breaks <- c(0.01, 1, 100, 10000)
y_lbls   <- c("10 µs", "1 ms", "100 ms", "10 s")
x_dom    <- c(4, 5000)
y_dom    <- c(0.005, 1e8)   # extra headroom for in-panel legends

# Font sizes are scaled up because JOSS scales the figure down to column width
# (~5 in). With base = 12 at a 7.2 in canvas, displayed text lands near 8 pt.
theme_fig <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#ececec", linewidth = 0.35),
      axis.line        = element_line(colour = "#1a1a1a", linewidth = 0.5),
      axis.ticks       = element_line(colour = "#444", linewidth = 0.4),
      axis.text        = element_text(colour = "#333", size = 9),
      axis.title       = element_text(colour = "#1a1a1a", size = 11),
      plot.tag         = element_text(face = "bold", size = 14, colour = "#1a1a1a"),
      plot.tag.position = c(0, 1)
    )
}

# ---------- Panel A: five family small multiples ---------------------------

panel_spec <- list(
  list(title = "JV",
       methods = c("jv", "sap", "lapmod"),
       colour  = "#0072b2"),
  list(title = "Auction",
       methods = c("auction", "auction_gs", "auction_scaled"),
       colour  = "#e69f00"),
  list(title = "Cost-scaling",
       methods = c("csa", "gabow_tarjan", "ssap_bucket"),
       colour  = "#009e73"),
  list(title = "Flow-based",
       methods = c("csflow", "cycle_cancel", "push_relabel",
                   "network_simplex", "orlin"),
       colour  = "#cc79a7"),
  list(title = "Other",
       methods = c("hungarian", "munkres", "ramshaw_tarjan", "hk01", "bruteforce"),
       colour  = "#5a4634")
)

linetype_pool <- c("solid", "dashed", "dotted", "dotdash", "twodash")

build_panel_a <- function(spec, show_y, show_x_lab, show_tag) {
  d <- df[df$method %in% spec$methods, ]
  # Order so the legend matches the spec order
  lbls <- unname(method_labels[spec$methods])
  d$label <- factor(method_labels[d$method], levels = lbls)
  lt_vals <- setNames(linetype_pool[seq_along(spec$methods)], lbls)
  # Default per-method linewidth; bruteforce in "Other" runs into Hungarian's
  # low-n segment, so thicken it to keep the two lines visually separable.
  # The first method in each panel gets the solid linetype; render it at half
  # width so overlaid dash patterns stay visible when traces coincide.
  lw_vals <- setNames(rep(0.85, length(spec$methods)), lbls)
  lw_vals[lbls[1]] <- 0.425
  if ("Bruteforce" %in% lbls) lw_vals["Bruteforce"] <- 1.4

  g <- ggplot(d, aes(x = n, y = median_ms,
                     linetype = label, linewidth = label, group = label)) +
    geom_line(colour = spec$colour, na.rm = TRUE) +
    scale_x_log10(breaks = x_breaks, labels = x_lbls,
                  limits = x_dom, expand = expansion(mult = c(0.04, 0.04))) +
    scale_y_log10(breaks = y_breaks, labels = y_lbls,
                  limits = y_dom, expand = c(0, 0)) +
    scale_linetype_manual(values = lt_vals) +
    scale_linewidth_manual(values = lw_vals) +
    labs(title = spec$title,
         x = if (show_x_lab) "problem size, n" else NULL,
         y = if (show_y)     "median solve time" else NULL,
         tag = if (show_tag) "(a)" else NULL) +
    guides(
      linetype = guide_legend(
        title = NULL,
        ncol = 1,
        override.aes = list(colour = spec$colour),
        keyheight = unit(0.6, "lines"),
        keywidth  = unit(1.8, "lines")
      ),
      linewidth = guide_legend(
        title = NULL,
        ncol = 1,
        keyheight = unit(0.6, "lines"),
        keywidth  = unit(1.8, "lines")
      )
    ) +
    theme_fig() +
    theme(
      plot.title = element_text(size = 11.5, face = "bold",
                                colour = "#1a1a1a",
                                margin = margin(b = 5)),
      legend.position      = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.background    = element_rect(fill = "white", colour = NA),
      legend.key           = element_rect(fill = "white", colour = NA),
      legend.text          = element_text(size = 7.5, colour = "#333"),
      legend.spacing.y     = unit(0.05, "lines"),
      legend.margin        = margin(0, 0, 0, 0),
      plot.margin          = margin(3, 6, 3, 6)
    )

  if (!show_y) {
    g <- g + theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
  }
  g
}

panels_a <- lapply(seq_along(panel_spec), function(i) {
  build_panel_a(
    panel_spec[[i]],
    show_y     = (i == 1),
    show_x_lab = FALSE,
    show_tag   = (i == 1)
  )
})

row_a <- wrap_plots(panels_a, nrow = 1)

# ---------- Panel B: couplr vs R competitors on causal-matching scaling -----

SCALING_RESULTS <- file.path(repo_root, "paper", "scaling-results.csv")
scaling <- read.csv(SCALING_RESULTS, stringsAsFactors = FALSE)
scaling$median_ms <- scaling$median_s * 1000
scaling$package <- factor(scaling$package,
                          levels = c("MatchIt", "optmatch", "couplr"))

# Mark rows where the package failed (NA median_s) so we can flag them on the
# plot — the integer-overflow crash inside MatchIt's optmatch backend at the
# largest size is part of the story.
scaling_ok   <- scaling[!is.na(scaling$median_s) & scaling$status == "ok", ]
scaling_fail <- scaling[is.na(scaling$median_s), ]

b_palette <- c(couplr   = "#0072b2",
               optmatch = "#009e73",
               MatchIt  = "#d55e00")
b_shapes  <- c(couplr   = 22,
               optmatch = 21,
               MatchIt  = 24)

x_breaks_b <- c(500, 2000, 5000, 10000, 20000)
x_lbls_b   <- c("500", "2,000", "5,000", "10,000", "20,000")
# Log-y ticks: 50 ms, 1 s, 1 min, 10 min, with headroom for the "int overflow"
# annotation that sits above the curves at the right edge.
y_breaks_b <- c(0.05, 1, 60, 600) * 1000
y_lbls_b   <- c("50 ms", "1 s", "1 min", "10 min")
y_dom_b    <- c(40, 3.6e6)   # 40 ms to 1 h

last_each <- do.call(rbind, lapply(split(scaling_ok, scaling_ok$package),
  function(d) d[which.max(d$n_total), ]))

# MatchIt failed at n=20,000 with an integer-overflow inside the optmatch
# backend. Show this as a vertical dashed segment from its last data point up
# to a dedicated "int overflow" annotation in MatchIt's colour, so the failure
# stays attached to the right package without colliding with optmatch's point.
fail_anno <- if (nrow(scaling_fail)) {
  matchit_last <- scaling_ok[as.character(scaling_ok$package) == "MatchIt", ]
  matchit_last <- matchit_last[which.max(matchit_last$n_total), ]
  data.frame(n_total   = scaling_fail$n_total[1],
             y_bottom  = matchit_last$median_ms,
             y_top     = 1.2e6,        # ~20 min, above all curves
             stringsAsFactors = FALSE)
} else NULL

p_b <- ggplot(scaling_ok,
              aes(x = n_total, y = median_ms, colour = package,
                  shape = package, group = package)) +
  geom_line(linewidth = 1.0) +
  geom_point(fill = "white", size = 2.6, stroke = 1.0) +
  scale_colour_manual(values = b_palette, guide = "none") +
  scale_shape_manual(values = b_shapes, guide = "none") +
  scale_x_log10(breaks = x_breaks_b, labels = x_lbls_b,
                expand = expansion(mult = c(0.05, 0.16))) +
  scale_y_log10(breaks = y_breaks_b, labels = y_lbls_b,
                limits = y_dom_b, expand = c(0, 0)) +
  labs(x = expression("problem size, " * n[t] + n[c]),
       y = "median wall-clock time",
       tag = "(b)") +
  theme_fig()

if (!is.null(fail_anno)) {
  p_b <- p_b +
    annotate("segment",
             x = fail_anno$n_total, xend = fail_anno$n_total,
             y = fail_anno$y_bottom, yend = fail_anno$y_top,
             colour = b_palette["MatchIt"], linetype = "22",
             linewidth = 0.55) +
    annotate("point",
             x = fail_anno$n_total, y = fail_anno$y_top,
             shape = 4, size = 3.4, stroke = 1.3,
             colour = b_palette["MatchIt"]) +
    annotate("text",
             x = fail_anno$n_total, y = fail_anno$y_top,
             label = "int overflow", hjust = -0.18, vjust = 0.5,
             size = 3.5, fontface = "italic",
             colour = b_palette["MatchIt"])
}

# Right-edge labels. couplr and optmatch both terminate at n=20,000 with
# couplr ~3x below optmatch; MatchIt ends at n=10,000. Place each label
# just to the right of its terminal point at the same y.
p_b <- p_b +
  geom_text(data = last_each,
            aes(x = n_total, y = median_ms,
                colour = package, label = package),
            hjust = -0.20, vjust = 0.5, size = 4.0,
            fontface = "bold",
            inherit.aes = FALSE)

# ---------- combine & save -------------------------------------------------

p_combined <- row_a / p_b +
  plot_layout(heights = c(1, 0.95))

dir.create(dirname(BENCHMARK_FIGURE), recursive = TRUE, showWarnings = FALSE)
# Saved a touch wider than column so it scales only slightly; large in-figure
# text (set in theme_fig) survives the scale-to-\linewidth in the JOSS template.
ggsave(BENCHMARK_FIGURE, p_combined,
       width = 7.2, height = 4.6, dpi = 300, bg = "white")
cat("Saved ", BENCHMARK_FIGURE, "\n", sep = ""); flush.console()
