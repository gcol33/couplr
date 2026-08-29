# couplr: Optimal Matching with Verifiable Certificates and Sparse Edge Generation

Submission to The R Journal.

## Contents

| Path | What it is |
|---|---|
| `rjournal.pdf` | The article. |
| `rjournal.html` | The article, web version. |
| `rjournal.Rmd` | Article source. |
| `rjournal.tex`, `RJwrapper.tex`, `RJournal.sty`, `RJreferences.bib` | Build files. |
| `supplementary/couplr-supplementary.Rmd` | Supplementary material: the full tables the article summarises. |
| `motivation-letter/motivation-letter.md` | Motivating letter. |
| `data/` | Benchmark output the article and the supplement read. |
| `data/ENVIRONMENT.txt` | Machine, R version and package versions the measurements were taken on. |
| `scripts/` | The benchmark scripts that produced `data/`. |
| `_Rpackages.txt` | Packages needed to reproduce the submission. |

## Building the article

The article reads its numbers from the CSV files in `data/`, so it builds in
well under a minute and needs only couplr, ggplot2, knitr, rmarkdown and
rjtools:

```r
rmarkdown::render("rjournal.Rmd", output_format = "all")
```

## Reproducing the measurements

The scripts in `scripts/` are the ones that wrote `data/`. They load couplr
from a source checkout and write their CSVs under `paper/`, so they run from a
clone of the package repository rather than from this archive:

```sh
git clone https://github.com/gcol33/couplr
cd couplr
sh paper/run_bench_suite.sh              # the whole suite
sh paper/run_bench_suite.sh regimes      # one script, by name
```

Each script resumes from its own CSV, so an interrupted suite continues where
it stopped; move the CSVs aside to force a full re-measurement. The scaling,
implicit, implicit-grid, regimes and memory scripts take `--quick` for a
reduced grid. The full suite is a wall-clock benchmark pinned to one core and
takes hours; the CSVs in `data/` are its output, and `data/ENVIRONMENT.txt`
records what produced them.

couplr is on CRAN and developed at <https://github.com/gcol33/couplr>.
