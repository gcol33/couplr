suppressPackageStartupMessages({
  library(couplr)
})

data(hospital_staff)
treated <- transform(hospital_staff$nurses_extended,   id = nurse_id)
control <- transform(hospital_staff$controls_extended, id = nurse_id)

m <- match_couples(
  left  = treated,
  right = control,
  vars  = c("age", "experience_years", "certification_level"),
  auto_scale = TRUE
)

bal <- balance_diagnostics(
  m, treated, control,
  vars = c("age", "experience_years", "certification_level")
)
cat("--- Balance on matching variables ---\n")
print(balance_table(bal))

treated$dept_icu <- as.integer(treated$department == "ICU")
control$dept_icu <- as.integer(control$department == "ICU")
bal_heldout <- balance_diagnostics(m, treated, control, vars = "dept_icu")

cat("\n--- Held-out balance (department == ICU) ---\n")
print(balance_table(bal_heldout))

cat("\n--- Pre-match (unadjusted) SMDs for context ---\n")
prematch_smd <- function(v) {
  l <- treated[[v]]; r <- control[[v]]
  (mean(l) - mean(r)) / sqrt((var(l) + var(r)) / 2)
}
for (v in c("age", "experience_years", "certification_level", "dept_icu")) {
  cat(sprintf("  %-22s  SMD = %+.3f\n", v, prematch_smd(v)))
}
