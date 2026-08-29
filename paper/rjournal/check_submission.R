## Run the R Journal's checks over a staged submission tree.
##
##   Rscript check_submission.R <staged-dir> <wordlist>
##
## The tree that is checked is the one that ships, so the build tooling and the
## LaTeX logs are absent by construction rather than by a filter. The wordlist
## feeds both the spelling check and the title-case check; check_title() reads
## it as a regex alternation, so it holds the article's own vocabulary and not a
## general dictionary. A word the checks report that is not in WORDLIST is a new
## one, and worth reading before it is added.
##
## Two path conventions are in play and they do not agree, so each check gets
## the one it resolves. Most read the article relative to the working directory,
## which is why the checks run from inside the staged tree. check_bib_doi() and
## check_csl() go through here::here(), which resolves against a project root;
## a staged tree has none above it, so a relative path there reaches the
## filesystem root, finds no .bib, and reports an empty bibliography as passing.
## Both are given the absolute path, which here::here() returns unchanged.
##
## Exits non-zero if any check reports an error. A warning does not stop the
## build: check_bib_doi() reports the two references carrying no DOI or URL, a
## 1993 textbook and a 1946 journal article, neither of which has one.

args  <- commandArgs(trailingOnly = TRUE)
stage <- normalizePath(args[1], winslash = "/", mustWork = TRUE)
words <- readLines(args[2], warn = FALSE, encoding = "UTF-8")

options(repos = c(CRAN = "https://cloud.r-project.org"),
        check.log.journal = new.env(parent = emptyenv()))
suppressMessages(library(rjtools))

setwd(stage)

invisible(check_filenames("."))
invisible(check_structure("."))
invisible(check_folder_structure("."))
invisible(check_unnecessary_files("."))
invisible(check_cover_letter("."))
invisible(check_title(".", ignore = words))
invisible(check_section("."))
invisible(check_abstract("."))
invisible(check_spelling(".", ignore = words))
invisible(check_proposed_pkg("couplr", ask = FALSE))
invisible(check_pkg_label("."))
invisible(check_packages_available("."))
invisible(check_bib_doi(stage))
invisible(check_csl(stage))
invisible(check_date("."))

## A bibliography that was never read also reports as passing, so say how many
## references the check actually saw.
cat("\nreferences read:", length(rjtools:::read_bib(stage)), "\n")

ct <- rjtools:::journal_summary(file = NULL)

if (ct[["ERROR"]] > 0) quit(status = 1L)
