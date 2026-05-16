#!/usr/bin/env bash
# Phase 8 A/B: build with GT_STEP2_BUCKET (Phase 7 flat bucket) then without
# (Phase 8 Dijkstra-Hungarian). Same session.

set -euo pipefail

REPO="/c/Users/Gilles Colling/documents/dev/couplr"
cd "$REPO"

RSCRIPT="/c/Program Files/R/R-4.6.0/bin/Rscript.exe"
MAKEVARS="src/Makevars.win"

cp "${MAKEVARS}" "${MAKEVARS}.bak"

restore_makevars () {
  mv "${MAKEVARS}.bak" "${MAKEVARS}"
}
trap restore_makevars EXIT

run_bench () {
  local label="$1"
  local with_flag="$2"   # "yes" enables GT_STEP2_BUCKET (Phase 7 path)
  local outfile="dev_notes/phase8_ab_${label}.log"

  echo
  echo "================================================================"
  echo "Building variant: ${label}  (GT_STEP2_BUCKET=${with_flag})"
  echo "================================================================"

  cp "${MAKEVARS}.bak" "${MAKEVARS}"
  if [ "${with_flag}" = "yes" ]; then
    sed -i 's/^PKG_CPPFLAGS = -DNDEBUG/PKG_CPPFLAGS = -DNDEBUG -DGT_STEP2_BUCKET/' "${MAKEVARS}"
  fi
  grep '^PKG_CPPFLAGS' "${MAKEVARS}"

  "${RSCRIPT}" dev_notes/phase8_ab_run.R 2>&1 | tee "${outfile}"
}

run_bench "phase7_bucket"   "yes"
run_bench "phase8_dijkstra" "no"
