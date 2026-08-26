#!/bin/sh
# Run every benchmark the article reports, in order, on one machine and one
# build. Sequential by design: they are wall-clock measurements pinned to a
# single core, and two of them running at once would time each other.
#
#   sh paper/run_bench_suite.sh            # the whole suite
#   sh paper/run_bench_suite.sh regimes    # one script, by name
#
# Each script writes its own CSVs under paper/ and resumes from them, so a
# killed suite continues where it stopped. Move the CSVs aside first to force a
# full re-measurement.
#
# Logs go to logs/<name>-<timestamp>.log, and logs/<name>.done is written when a
# script exits, carrying its exit status. logs/SUITE.done marks the end of the
# whole run.

set -u

cd "$(dirname "$0")/.." || exit 1
mkdir -p logs

STAMP=$(date +%Y%m%d-%H%M%S)
RSCRIPT=${RSCRIPT:-Rscript}

# Order: the three tables the article carries first, so the article can be
# rebuilt before the supplementary grids have finished.
ALL="scaling implicit path regimes implicit_grid memory"
WHICH=${1:-$ALL}

script_for() {
  case "$1" in
    scaling)       echo "paper/bench_scaling.R" ;;
    implicit)      echo "paper/bench_implicit.R" ;;
    path)          echo "paper/bench_path.R" ;;
    regimes)       echo "paper/bench_regimes.R" ;;
    implicit_grid) echo "paper/bench_implicit_grid.R" ;;
    memory)        echo "paper/bench_memory.R" ;;
    *)             echo "" ;;
  esac
}

for name in $WHICH; do
  script=$(script_for "$name")
  if [ -z "$script" ]; then
    echo "unknown benchmark: $name" >&2
    exit 2
  fi
  log="logs/${name}-${STAMP}.log"
  echo "=== $name -> $log ==="
  "$RSCRIPT" "$script" > "$log" 2>&1
  status=$?
  echo "$name exit $status $(date +%Y-%m-%dT%H:%M:%S)" > "logs/${name}.done"
  echo "=== $name exit $status ==="
done

echo "suite done $(date +%Y-%m-%dT%H:%M:%S)" > logs/SUITE.done
