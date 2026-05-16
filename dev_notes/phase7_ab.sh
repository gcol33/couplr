#!/usr/bin/env bash
# Phase 7 A/B: build with GT_BUCKET_VECTOR (Phase 6 old buckets) then without
# (Phase 7 flat buckets). Each variant runs in its own Rscript invocation.

set -euo pipefail

REPO="/c/Users/Gilles Colling/documents/dev/couplr"
cd "$REPO"

RSCRIPT="/c/Program Files/R/R-4.6.0/bin/Rscript.exe"
MAKEVARS="src/Makevars.win"

# Backup original
cp "${MAKEVARS}" "${MAKEVARS}.bak"

restore_makevars () {
  mv "${MAKEVARS}.bak" "${MAKEVARS}"
}
trap restore_makevars EXIT

run_bench () {
  local label="$1"
  local with_flag="$2"   # "yes" or "no"
  local outfile="dev_notes/phase7_ab_${label}.log"

  echo
  echo "================================================================"
  echo "Building variant: ${label}  (GT_BUCKET_VECTOR=${with_flag})"
  echo "================================================================"

  # Always start from the backup, then optionally inject -DGT_BUCKET_VECTOR.
  cp "${MAKEVARS}.bak" "${MAKEVARS}"
  if [ "${with_flag}" = "yes" ]; then
    sed -i 's/^PKG_CPPFLAGS = -DNDEBUG/PKG_CPPFLAGS = -DNDEBUG -DGT_BUCKET_VECTOR/' "${MAKEVARS}"
  fi
  grep '^PKG_CPPFLAGS' "${MAKEVARS}"

  "${RSCRIPT}" dev_notes/phase7_ab_run.R 2>&1 | tee "${outfile}"
}

run_bench "old_buckets" "yes"
run_bench "flat_buckets" "no"
