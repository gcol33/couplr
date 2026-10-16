benchmark_stages <- function(default_only = FALSE) {
  stages <- list(
    external = list(
      script = "bench_external.R", default = TRUE,
      outputs = c("external-runs.csv", "external-results.csv",
                  "external-lalonde.csv", "external-ENVIRONMENT.txt")),
    scaling = list(
      script = "bench_scaling.R", default = TRUE,
      outputs = c("scaling-results.csv", "scaling-runs.csv")),
    implicit = list(
      script = "bench_implicit.R", default = TRUE,
      outputs = c("implicit-results.csv", "implicit-equivalence.csv")),
    memory = list(
      script = "bench_memory.R", default = TRUE,
      outputs = "memory-results.csv"),
    implicit_grid = list(
      script = "bench_implicit_grid.R", default = TRUE,
      outputs = c("implicit-grid-runs.csv", "implicit-grid-results.csv")),
    path = list(
      script = "bench_path.R", default = TRUE,
      outputs = c("path-results.csv", "path-points.csv", "path-checkpoint.rds")),
    regimes = list(
      script = "bench_regimes.R", default = TRUE,
      outputs = c("regime-runs.csv", "regime-cells.csv",
                  "regime-results.csv", "regime-verdict.csv")),
    scaling_lazy = list(
      script = "bench_scaling_lazy.R", default = FALSE,
      outputs = c("scaling-lazy-results.csv", "scaling-lazy-runs.csv")),
    lalonde = list(
      script = "bench_lalonde.R", default = FALSE,
      outputs = c("lalonde-results.csv", "lalonde-per-covariate.csv")),
    certificate = list(
      script = "bench_certificate.R", default = FALSE,
      outputs = c("certificate-runs.csv", "certificate-results.csv")),
    caliper = list(
      script = "bench_caliper.R", default = TRUE,
      outputs = c("caliper-results.csv", "caliper-runs.csv",
                  "caliper-values.csv")),
    cardinality = list(
      script = "bench_cardinality.R", default = TRUE,
      outputs = c("cardinality-results.csv", "cardinality-runs.csv",
                  "cardinality-values.csv"))
  )
  if (default_only) {
    stages <- Filter(function(stage) stage$default, stages)
  }
  stages
}

benchmark_bundle_files <- function(bench_dir) {
  files <- c("bench_registry.R", "bench_setup.R", "bench_common.R",
             "run_benchmarks.R",
             vapply(benchmark_stages(), `[[`, "", "script"), "results")
  missing <- files[!file.exists(file.path(bench_dir, files))]
  if (length(missing)) {
    stop("Missing benchmark bundle files: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  unname(files)
}
