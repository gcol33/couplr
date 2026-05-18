suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

# Reproduce the 5x5-forbidden + max=TRUE auction discrepancy
set.seed(2026)
. <- matrix(sample.int(20, 9), nrow = 3)        # case 1
. <- matrix(sample.int(5, 16, replace = TRUE), nrow = 4)  # case 2
. <- matrix(sample.int(100, 36), nrow = 6)      # case 3
m <- matrix(sample.int(50, 25), nrow = 5)
m[1, 2] <- NA_real_; m[3, 4] <- Inf; m[5, 1] <- NA_real_

cat("=== 5x5 forbidden matrix ===\n")
print(m)
cat("\n=== auction maximize=TRUE ===\n")
tr <- get_trace_fn("auction")(m, maximize = TRUE)
cat("trace matching:", trace_final <- tr$frames[[length(tr$frames)]]$matching, "\n")
cat("trace total_cost (meta):", tr$meta$total_cost, "\n")
cat("trace matching using cost: rows ->", trace_final, "\n")
for (i in seq_len(5)) cat(sprintf("  row %d -> col %d, cost = %s\n", i, trace_final[i], format(m[i, trace_final[i]])))
oracle <- assignment(m, maximize = TRUE, method = "auction")
cat("oracle matching:", oracle$match, "\n")
cat("oracle total_cost:", oracle$total_cost, "\n")
for (i in seq_len(5)) cat(sprintf("  row %d -> col %d, cost = %s\n", i, oracle$match[i], format(m[i, oracle$match[i]])))

cat("\n=== bottleneck min on 3x3 ===\n")
set.seed(2026)
m3 <- matrix(sample.int(20, 9), nrow = 3)
print(m3)
trb <- tryCatch(get_trace_fn("bottleneck")(m3, maximize = FALSE),
                error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
if (!is.null(trb)) {
  cat("trace bottleneck value:", trb$meta$total_cost, "\n")
  cat("final matching:", trb$frames[[length(trb$frames)]]$matching, "\n")
}
oracle_b <- bottleneck_assignment(m3, maximize = FALSE)
cat("oracle bottleneck:", oracle_b$total_cost, "match:", oracle_b$match, "\n")
