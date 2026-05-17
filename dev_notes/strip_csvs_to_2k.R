## Strip rows where n <= 2000 from benchmark-table.csv and
## n_total <= 2000 from scaling-results.csv, leaving larger-n rows in place.
## After this, the resume-safe benchmark scripts will recompute exactly the
## stripped rows on this CPU.

paper_dir <- "paper"

bt_path <- file.path(paper_dir, "benchmark-table.csv")
bt <- read.csv(bt_path, stringsAsFactors = FALSE)
cat("benchmark-table.csv: before =", nrow(bt), "rows\n")
bt <- bt[bt$n > 2000, , drop = FALSE]
write.csv(bt, bt_path, row.names = FALSE)
cat("benchmark-table.csv: after  =", nrow(bt), "rows kept (n > 2000)\n")
print(table(bt$method, bt$n))

sr_path <- file.path(paper_dir, "scaling-results.csv")
sr <- read.csv(sr_path, stringsAsFactors = FALSE)
cat("\nscaling-results.csv: before =", nrow(sr), "rows\n")
sr <- sr[sr$n_total > 2000, , drop = FALSE]
write.csv(sr, sr_path, row.names = FALSE)
cat("scaling-results.csv: after  =", nrow(sr), "rows kept (n_total > 2000)\n")
print(sr)
