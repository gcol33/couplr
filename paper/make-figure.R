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
  jv              = "JV",
  sap             = "SAP",
  lapmod          = "LAPMOD",
  auction         = "Auction",
  auction_gs      = "Auction-GS",
  auction_scaled  = "Auction-scaled",
  csa             = "CSA",
  gabow_tarjan    = "Gabow-Tarjan",
  ssap_bucket     = "SSAP-bucket",
  csflow          = "CS-flow",
  cycle_cancel    = "Cycle-cancel",
  network_simplex = "Net-simplex",
  push_relabel    = "Push-relabel",
  orlin           = "Orlin",
  ramshaw_tarjan  = "Ramshaw-Tarjan",
  hk01            = "HK-01*",
  bruteforce      = "Bruteforce†",
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
  csflow          = 500L,
  cycle_cancel    = 100L,
  network_simplex = 500L,
  orlin           = 100L,
  push_relabel    = 500L,
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

cat("--- General methods ---\n"); flush.console()
for (n in general_sizes) {
  set.seed(SEED)
  cost <- matrix(sample.int(MAX_COST, n * n, replace = TRUE), n, n)
  for (m in general_methods) {
    max_n <- method_max_n[m]
    if (!is.na(max_n) && n > max_n) next
    cat(sprintf("  n=%3d  %s\n", n, m)); flush.console()
    r <- bench_one(m, n, TIMES, cost)
    record_result(r)
  }
}

cat("--- Auto method ---\n"); flush.console()
for (n in general_sizes) {
  set.seed(SEED)
  cost <- matrix(sample.int(MAX_COST, n * n, replace = TRUE), n, n)
  cat(sprintf("  n=%3d  auto\n", n)); flush.console()
  r <- bench_one("auto", n, TIMES, cost)
  record_result(r)
}

cat("--- Bruteforce (n <= 8) ---\n"); flush.console()
for (n in small_sizes) {
  set.seed(SEED)
  cost <- matrix(sample.int(MAX_COST, n * n, replace = TRUE), n, n)
  cat(sprintf("  n=%d  bruteforce\n", n)); flush.console()
  r <- bench_one("bruteforce", n, TIMES, cost)
  record_result(r)
}

cat("--- HK-01 (binary costs) ---\n"); flush.console()
for (n in binary_sizes) {
  set.seed(SEED)
  cost <- matrix(sample(0L:1L, n * n, replace = TRUE), n, n)
  cat(sprintf("  n=%3d  hk01\n", n)); flush.console()
  r <- bench_one("hk01", n, TIMES, cost)
  record_result(r)
}

df <- save_results(results, BENCHMARK_TABLE)
cat("Saved ", BENCHMARK_TABLE, "\n", sep = ""); flush.console()

# Keep the figure layout independent of the benchmark run by plotting only from
# the persisted table.
df <- read.csv(BENCHMARK_TABLE, stringsAsFactors = FALSE)

# ---------- Panel A: all general solvers, direct labels ---------------------

df_a <- df[df$method %in% general_methods, ]

family_order <- c("Classical", "JV / Augmenting path", "Auction",
                  "Cost scaling", "Flow-based", "Rectangular / general")
df_a$family <- factor(df_a$family, levels = family_order)

# Label position = last (highest n) observation per solver
df_label_a <- do.call(rbind, lapply(split(df_a, df_a$method), function(d) {
  d[which.max(d$n), ]
}))

p_a <- ggplot(df_a, aes(x = n, y = median_ms, color = family, group = method)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 1.5, na.rm = TRUE) +
  geom_text_repel(
    data    = df_label_a,
    aes(label = label),
    size    = 2.4,
    hjust   = 0,
    xlim    = c(5200, NA),
    direction      = "y",
    segment.size   = 0.3,
    segment.color  = "grey55",
    min.segment.length = 0.1,
    box.padding    = 0.2,
    force          = 1.2,
    max.overlaps   = Inf,
    seed           = 7L
  ) +
  scale_x_log10(
    breaks = general_sizes,
    labels = c("10", "25", "50", "100", "200", "500", "1k", "2k", "5k"),
    expand = expansion(mult = c(0.02, 0.55))
  ) +
  scale_y_log10(
    labels = function(x) {
      ifelse(x < 1,
             paste0(round(x * 1000, 0), "μs"),
             ifelse(x < 1000,
                    paste0(round(x, 1), " ms"),
                    paste0(round(x / 1000, 1), " s")))
    }
  ) +
  scale_color_manual(values = family_colors, name = "Family") +
  labs(
    x   = "Problem size n (n × n matrix)",
    y   = "Median solve time",
    tag = "A"
  ) +
  guides(color = guide_legend(
    override.aes = list(linewidth = 1.5),
    ncol = 2
  )) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    legend.title      = element_text(face = "bold", size = 8),
    legend.text       = element_text(size = 7.5),
    legend.key.width  = unit(1.5, "lines"),
    axis.title        = element_text(size = 9),
    plot.tag          = element_text(face = "bold", size = 10)
  )

# ---------- Panel B: auto vs Hungarian, speedup annotated ------------------

df_b <- df[df$method %in% c("auto", "hungarian"), ]

# Merge to compute speedup at each shared n
t_auto <- df_b[df_b$method == "auto",     c("n", "median_ms")]
t_hung <- df_b[df_b$method == "hungarian", c("n", "median_ms")]
speedup_df <- merge(t_auto, t_hung, by = "n", suffixes = c("_auto", "_hung"))
speedup_df$speedup <- speedup_df$median_ms_hung / speedup_df$median_ms_auto
speedup_df$y_ann   <- sqrt(speedup_df$median_ms_auto * speedup_df$median_ms_hung)

panel_b_colors <- c("auto"      = "#2196F3",
                    "hungarian" = "#E53935")
panel_b_labels <- c("auto"      = "auto  (smart dispatch)",
                    "hungarian" = "Hungarian  (classical baseline)")

p_b <- ggplot(df_b, aes(x = n, y = median_ms, color = method, group = method)) +
  geom_line(linewidth = 1.1, na.rm = TRUE) +
  geom_point(size = 2.2, na.rm = TRUE) +
  geom_text(
    data = speedup_df[speedup_df$speedup >= 1.5, ],
    aes(x = n, y = y_ann,
        label = paste0(round(speedup, 1), "× faster")),
    inherit.aes = FALSE,
    size = 2.5, color = "grey30",
    hjust = -0.15, vjust = 0.4
  ) +
  scale_x_log10(
    breaks = general_sizes,
    labels = c("10", "25", "50", "100", "200", "500", "1k", "2k", "5k"),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  scale_y_log10(
    labels = function(x) {
      ifelse(x < 1,
             paste0(round(x * 1000, 0), "μs"),
             ifelse(x < 1000,
                    paste0(round(x, 1), " ms"),
                    paste0(round(x / 1000, 1), " s")))
    }
  ) +
  scale_color_manual(values = panel_b_colors,
                     labels = panel_b_labels,
                     name   = NULL) +
  labs(
    x        = "Problem size n (n × n matrix)",
    y        = NULL,
    tag      = "B",
    subtitle = "Auto-selection vs. classical baseline"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor  = element_blank(),
    legend.position   = "bottom",
    legend.text       = element_text(size = 7.5),
    axis.title        = element_text(size = 9),
    plot.tag          = element_text(face = "bold", size = 10),
    plot.subtitle     = element_text(size = 8, color = "grey40")
  )

# ---------- combine & save -------------------------------------------------

p_combined <- p_a + p_b + plot_layout(widths = c(3, 2))

dir.create(dirname(BENCHMARK_FIGURE), recursive = TRUE, showWarnings = FALSE)
ggsave(BENCHMARK_FIGURE, p_combined,
       width = 10, height = 5.2, dpi = 200, bg = "white")
cat("Saved ", BENCHMARK_FIGURE, "\n", sep = ""); flush.console()
