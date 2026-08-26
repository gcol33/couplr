#!/bin/sh
# Copy the article's inputs into the submission directory.
#
#   sh paper/rjournal/sync_inputs.sh
#
# The article and its supplementary material read from paper/rjournal/data and
# paper/rjournal/scripts so the submission is self-contained, while the
# benchmarks write into paper/. This is the one place that says which files
# cross, so a new measurement is a line here rather than a copy someone
# remembers to make. A named file that does not exist is an error: a silently
# missing input is an article knitting against a stale number.

set -eu

here=$(cd "$(dirname "$0")" && pwd)
paper=$(cd "$here/.." && pwd)
repo=$(cd "$paper/.." && pwd)

DATA="benchmark-table.csv
lalonde-per-covariate.csv
lalonde-results.csv
scaling-results.csv
scaling-runs.csv
scaling-lazy-results.csv
implicit-results.csv
implicit-equivalence.csv
path-results.csv
path-points.csv
regime-runs.csv
regime-cells.csv
regime-results.csv
regime-verdict.csv
implicit-grid-runs.csv
implicit-grid-results.csv
memory-results.csv"

SCRIPTS="bench_common.R
bench_scaling.R
bench_scaling_lazy.R
bench_scaling_alternatives.R
bench_implicit.R
bench_implicit_grid.R
bench_memory.R
bench_regimes.R
bench_path.R
bench_lalonde.R
make-figure.R
run_bench_suite.sh"

mkdir -p "$here/data" "$here/scripts"

missing=0
for f in $DATA; do
  if [ -f "$paper/$f" ]; then
    cp "$paper/$f" "$here/data/$f"
  else
    echo "missing input: paper/$f" >&2
    missing=1
  fi
done

for f in $SCRIPTS; do
  if [ -f "$paper/$f" ]; then
    cp "$paper/$f" "$here/scripts/$f"
  else
    echo "missing script: paper/$f" >&2
    missing=1
  fi
done

# What the numbers were measured on, written by paper/run_bench_suite.sh. The
# supplementary material prints it verbatim.
if [ -f "$repo/logs/ENVIRONMENT.txt" ]; then
  cp "$repo/logs/ENVIRONMENT.txt" "$here/data/ENVIRONMENT.txt"
else
  echo "no logs/ENVIRONMENT.txt: run paper/run_bench_suite.sh to produce one" >&2
fi

if [ "$missing" -ne 0 ]; then
  echo "sync incomplete" >&2
  exit 1
fi

echo "synced $(echo "$DATA" | wc -l | tr -d ' ') data files and $(echo "$SCRIPTS" | wc -l | tr -d ' ') scripts into $here"
