#!/bin/sh
# Build the R Journal submission archive.
#
#   sh paper/rjournal/make_submission.sh
#
# Refreshes the article's inputs, then stages the files the R Journal asks for
# into a rjournal/ tree and zips it. The staging list below is the definition
# of the submission: anything not named here does not ship, which is how the
# style corpus under refs/ and the LaTeX logs stay out of the archive.
#
# The build outputs must be newer than the source they came from. A submission
# whose PDF predates the last edit to the Rmd is the failure this guards.

set -eu

here=$(cd "$(dirname "$0")" && pwd)
paper=$(cd "$here/.." && pwd)
out_zip="$paper/couplr-rjournal-submission.zip"

sh "$here/sync_inputs.sh"

# --- article date ----------------------------------------------------------

# rjtools' check_date() requires the article's date to be the day it is
# submitted, so the archive is built on that day and not before.

art_date=$(grep -m1 '^date:' "$here/rjournal.Rmd" | grep -oE '[0-9]{4}-[0-9]{2}-[0-9]{2}')
today=$(date +%Y-%m-%d)
if [ "$art_date" != "$today" ]; then
  echo "article date is $art_date, today is $today" >&2
  echo "set date: \"$today\" in rjournal.Rmd, then re-render both formats" >&2
  exit 1
fi

# --- freshness -------------------------------------------------------------

# Every number in the article is computed from data/, so an output older than a
# refreshed CSV describes an earlier measurement while passing a check that only
# reads the source. The newest input decides, not rjournal.Rmd alone.
newest_input="$here/rjournal.Rmd"
for src in "$here/RJreferences.bib" "$here"/data/*; do
  [ -f "$src" ] || continue
  [ "$src" -nt "$newest_input" ] && newest_input="$src"
done

for built in rjournal.pdf rjournal.html rjournal.tex; do
  if [ ! -f "$here/$built" ]; then
    echo "missing build output: $built" >&2
    echo "run: Rscript -e 'rmarkdown::render(\"rjournal.Rmd\", output_format = \"all\")'" >&2
    exit 1
  fi
  if [ "$newest_input" -nt "$here/$built" ]; then
    echo "$built is older than $(basename "$newest_input")" >&2
    echo "run: Rscript -e 'rmarkdown::render(\"rjournal.Rmd\", output_format = \"all\")'" >&2
    exit 1
  fi
done

sup="$here/supplementary/couplr-supplementary"
sup_newest="$sup.Rmd"
[ "$here/RJreferences.bib" -nt "$sup_newest" ] && sup_newest="$here/RJreferences.bib"
if [ ! -f "$sup.pdf" ] || [ "$sup_newest" -nt "$sup.pdf" ]; then
  echo "supplementary PDF missing or older than $(basename "$sup_newest")" >&2
  echo "run: Rscript -e 'rmarkdown::render(\"couplr-supplementary.Rmd\")' in supplementary/" >&2
  exit 1
fi

# --- staging ---------------------------------------------------------------

FILES="rjournal.Rmd
rjournal.R
rjournal.tex
rjournal.pdf
rjournal.html
RJwrapper.tex
RJournal.sty
RJreferences.bib
_Rpackages.txt"

DIRS="data
scripts
supplementary
motivation-letter"

stage=$(mktemp -d)
trap 'rm -rf "$stage"' EXIT
root="$stage/rjournal"
mkdir -p "$root"

for f in $FILES; do
  if [ ! -f "$here/$f" ]; then
    echo "missing submission file: $f" >&2
    exit 1
  fi
  cp "$here/$f" "$root/$f"
done

for d in $DIRS; do
  if [ ! -d "$here/$d" ]; then
    echo "missing submission directory: $d" >&2
    exit 1
  fi
  mkdir -p "$root/$d"
  find "$here/$d" -type f \
    ! -name '*.log' ! -name '*.aux' ! -name '*.out' \
    -exec cp {} "$root/$d/" \;
done

# --- checks ----------------------------------------------------------------

# Checked on the staged tree, which is what ships. An error stops the build.

if ! Rscript "$here/check_submission.R" \
     "$(cygpath -m "$root" 2>/dev/null || echo "$root")" \
     "$(cygpath -m "$here/WORDLIST" 2>/dev/null || echo "$here/WORDLIST")"; then
  echo "submission checks reported an error" >&2
  exit 1
fi

rm -f "$out_zip"

# R already has to be present to build the article, so its zip package is what
# writes the archive; git-bash on Windows ships no zip(1).
Rscript -e 'a <- commandArgs(TRUE); zip::zip(a[1], "rjournal", root = a[2])'   "$(cygpath -m "$out_zip" 2>/dev/null || echo "$out_zip")"   "$(cygpath -m "$stage" 2>/dev/null || echo "$stage")"

bytes=$(wc -c < "$out_zip" | tr -d ' ')
files=$(unzip -l "$out_zip" | tail -1 | awk '{print $2}')
echo "wrote $out_zip ($(( bytes / 1024 )) KB, $files files)"
if [ "$bytes" -gt 10485760 ]; then
  echo "over the R Journal's 10 MB submission limit" >&2
  exit 1
fi
