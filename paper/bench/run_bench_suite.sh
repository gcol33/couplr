#!/bin/sh
# Run every benchmark the article reports, in order, on one machine and one
# build. Sequential by design: they are wall-clock measurements pinned to a
# single core, and two of them running at once would time each other.
#
#   sh paper/bench/run_bench_suite.sh            # the whole suite
#   sh paper/bench/run_bench_suite.sh regimes    # one script, by name
#
# Each script writes its own CSVs under paper/bench/results/ and resumes from
# them, so a killed suite continues where it stopped. FRESH=1 forces a full
# re-measurement by archiving the selected stages' outputs under
# logs/archive-<timestamp>/ once the environment block has been written:
#
#   FRESH=1 sh paper/bench/run_bench_suite.sh "implicit memory"
#
# Clearing them that way rather than by hand keeps the working tree clean at
# the moment the commit stamp below is taken, so the stamp describes the tree
# the numbers come from.
#
# Logs go to logs/<name>-<timestamp>.log, and logs/<name>.done is written when a
# script exits, carrying its exit status. logs/SUITE.done is written only when
# every stage exited zero, and the suite exits non-zero otherwise: a marker
# that appears after a failed stage says the run is complete when it is not,
# and anything waiting on it reads a partial measurement as a finished one.

set -u

cd "$(dirname "$0")/../.." || exit 1
mkdir -p logs

RESULTS=paper/bench/results

STAMP=$(date +%Y%m%d-%H%M%S)
RSCRIPT=${RSCRIPT:-Rscript}

# Order: the three tables the article carries first, so the article can be
# rebuilt before the supplementary grids have finished.
ALL="scaling scaling_lazy implicit path regimes implicit_grid memory lalonde certificate figure"
WHICH=${1:-$ALL}

script_for() {
  case "$1" in
    scaling)       echo "paper/bench/bench_scaling.R" ;;
    scaling_lazy)  echo "paper/bench/bench_scaling_lazy.R" ;;
    implicit)      echo "paper/bench/bench_implicit.R" ;;
    path)          echo "paper/bench/bench_path.R" ;;
    regimes)       echo "paper/bench/bench_regimes.R" ;;
    dispatch_validation) echo "paper/bench/bench_dispatch_validation.R" ;;  # at 85eefde or 853f91f
    implicit_grid) echo "paper/bench/bench_implicit_grid.R" ;;
    memory)        echo "paper/bench/bench_memory.R" ;;
    lalonde)       echo "paper/bench/bench_lalonde.R" ;;
    certificate)   echo "paper/bench/bench_certificate.R" ;;
    figure)        echo "paper/bench/make-figure.R" ;;
    *)             echo "" ;;
  esac
}

# What each stage writes, beside what runs it, so a stage carries its own
# outputs in one place and FRESH has nothing to be told separately.
outputs_for() {
  case "$1" in
    scaling)       echo "$RESULTS/scaling-results.csv $RESULTS/scaling-runs.csv" ;;
    scaling_lazy)  echo "$RESULTS/scaling-lazy-results.csv $RESULTS/scaling-lazy-runs.csv" ;;
    implicit)      echo "$RESULTS/implicit-results.csv $RESULTS/implicit-equivalence.csv" ;;
    path)          echo "$RESULTS/path-results.csv $RESULTS/path-points.csv" ;;
    regimes)       echo "$RESULTS/regime-runs.csv $RESULTS/regime-cells.csv $RESULTS/regime-results.csv $RESULTS/regime-verdict.csv" ;;
    dispatch_validation) echo "$RESULTS/dispatch-validation-2-runs.csv $RESULTS/dispatch-validation-2-results.csv $RESULTS/dispatch-validation-2-verdict.csv" ;;
    implicit_grid) echo "$RESULTS/implicit-grid-runs.csv $RESULTS/implicit-grid-results.csv" ;;
    memory)        echo "$RESULTS/memory-results.csv" ;;
    lalonde)       echo "$RESULTS/lalonde-results.csv $RESULTS/lalonde-per-covariate.csv" ;;
    certificate)   echo "$RESULTS/certificate-runs.csv $RESULTS/certificate-results.csv" ;;
    figure)        echo "$RESULTS/benchmark-table.csv paper/joss/figures/benchmark.png" ;;
    *)             echo "" ;;
  esac
}

for name in $WHICH; do
  if [ -z "$(script_for "$name")" ]; then
    echo "unknown benchmark: $name" >&2
    exit 2
  fi
done

# What the numbers were measured on. The article states the R version, the
# platform and the comparison packages' versions, and this is where it reads
# them from rather than from memory of an earlier run. couplr's row is the
# source tree's version: every benchmark loads the package with
# pkgload::load_all(), so the installed copy is not what gets measured.
{
  echo "run        $STAMP"
  echo "host       $(uname -a)"
  echo "commit     $(git rev-parse --short HEAD 2>/dev/null) $(git status --porcelain 2>/dev/null | wc -l | tr -d ' ') modified paths"
  "$RSCRIPT" --vanilla -e 'cat(R.version.string, "\n", R.version$platform, "\n", sep = "");
    cat(sprintf("%-14s %s\n", "couplr", read.dcf("DESCRIPTION", "Version")[1, 1]));
    for (p in c("MatchIt", "optmatch", "microbenchmark")) {
      v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
      cat(sprintf("%-14s %s\n", p, v))
    }'
} > logs/ENVIRONMENT.txt 2>&1
cat logs/ENVIRONMENT.txt

if [ "${FRESH:-0}" = "1" ]; then
  archive="logs/archive-$STAMP"
  for name in $WHICH; do
    for f in $(outputs_for "$name"); do
      [ -e "$f" ] || continue
      mkdir -p "$archive/$(dirname "$f")"
      mv "$f" "$archive/$f"
    done
  done
  echo "previous outputs archived to $archive"
fi

failed=""
for name in $WHICH; do
  script=$(script_for "$name")
  log="logs/${name}-${STAMP}.log"
  echo "=== $name -> $log ==="
  "$RSCRIPT" "$script" > "$log" 2>&1
  status=$?
  echo "$name exit $status $(date +%Y-%m-%dT%H:%M:%S)" > "logs/${name}.done"
  echo "=== $name exit $status ==="
  [ "$status" -eq 0 ] || failed="$failed $name"
done

if [ -n "$failed" ]; then
  echo "stages failed:$failed" >&2
  echo "logs/SUITE.done not written; the measurement is partial" >&2
  exit 1
fi

echo "suite done $(date +%Y-%m-%dT%H:%M:%S)" > logs/SUITE.done
