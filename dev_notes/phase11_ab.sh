#!/usr/bin/env bash
# Phase 11 A/B: build without 6n pruning (-DGT_DISABLE_6N_PRUNE) vs with
# pruning (no flag, default-on inside solve_gabow_tarjan_inner). Same Rscript
# invocation per variant, same seed schedule, sequential.

set -euo pipefail

REPO="/c/Users/Gilles Colling/documents/dev/couplr"
cd "$REPO"

RSCRIPT="/c/Program Files/R/R-4.6.0/bin/Rscript.exe"
MAKEVARS="src/Makevars.win"

cp "${MAKEVARS}" "${MAKEVARS}.bak"
restore_makevars () { mv "${MAKEVARS}.bak" "${MAKEVARS}"; }
trap restore_makevars EXIT

run_bench () {
  local label="$1"
  local disable_flag="$2"   # "yes" or "no"
  local outfile="dev_notes/phase11_ab_${label}.log"

  echo
  echo "================================================================"
  echo "Building variant: ${label}  (GT_DISABLE_6N_PRUNE=${disable_flag})"
  echo "================================================================"

  cp "${MAKEVARS}.bak" "${MAKEVARS}"
  if [ "${disable_flag}" = "yes" ]; then
    sed -i 's/^PKG_CPPFLAGS = -DNDEBUG/PKG_CPPFLAGS = -DNDEBUG -DGT_DISABLE_6N_PRUNE/' "${MAKEVARS}"
  fi
  grep '^PKG_CPPFLAGS' "${MAKEVARS}"

  "${RSCRIPT}" dev_notes/phase11_ab_run.R 2>&1 | tee "${outfile}"
}

run_bench "no_prune" "yes"
run_bench "with_prune" "no"
