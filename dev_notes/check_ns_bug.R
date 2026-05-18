devtools::load_all(quiet = TRUE)
m <- matrix(c(38, 16, 6, 23, NA,
              NA, 20, 50, 37, 35,
              44, 15, 21, 36, 43,
              33, 26, Inf, 30, 42,
              19, 13, 46, 27, 48), 5, 5)
print(m)
r1 <- assignment(m, maximize = TRUE, method = "network_simplex")
cat("ns max:", r1$total_cost, "match:", r1$match, "\n")
r2 <- assignment(m, maximize = TRUE, method = "jv")
cat("jv max:", r2$total_cost, "match:", r2$match, "\n")
r3 <- assignment(m, maximize = TRUE, method = "hungarian")
cat("hungarian max:", r3$total_cost, "match:", r3$match, "\n")

# Manual check: what's the work matrix sent to network_simplex?
work <- -m
work[!is.finite(work)] <- Inf
cat("Work matrix sent to C++ network_simplex:\n")
print(work)
