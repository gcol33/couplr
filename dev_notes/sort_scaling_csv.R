## Drop the out-of-scope new n_total=50000 optmatch row (user scope was
## "up to n=2000"; the script over-ran the grid before it was killed) and
## re-sort the CSV by (n_total, package) with packages in script order.

sr_path <- "paper/scaling-results.csv"
sr <- read.csv(sr_path, stringsAsFactors = FALSE)
cat("before:", nrow(sr), "rows\n")

## Identify the row added by the over-run: n_total=50000 optmatch (status=timeout).
mask <- !(sr$n_total == 50000 & sr$package == "optmatch")
sr <- sr[mask, , drop = FALSE]

pkg_order <- c("couplr", "optmatch", "MatchIt")
sr$package <- factor(sr$package, levels = pkg_order)
sr <- sr[order(sr$n_total, sr$package), , drop = FALSE]
sr$package <- as.character(sr$package)

write.csv(sr, sr_path, row.names = FALSE)
cat("after :", nrow(sr), "rows\n")
print(sr)
