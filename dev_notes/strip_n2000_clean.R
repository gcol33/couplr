## Strip exactly the n=2000 rows from benchmark-table.csv so make-figure.R
## resume logic re-measures them on clean CPU (a stray testthat::test_local
## run had been competing for cores during the first pass).

bt_path <- "paper/benchmark-table.csv"
bt <- read.csv(bt_path, stringsAsFactors = FALSE)
cat("before:", nrow(bt), "rows\n")
bt <- bt[bt$n != 2000, , drop = FALSE]
write.csv(bt, bt_path, row.names = FALSE)
cat("after :", nrow(bt), "rows (n=2000 stripped)\n")
