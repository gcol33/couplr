## The hospital_staff example of README.md: implicit matching with a
## certificate, balance diagnostics, calipers and a path over the maximum
## distance. Prints every number README.md and the SoftwareX article quote.
##
## Reproducible via:  Rscript paper/bench/example_hospital_staff.R
library(couplr)
data(hospital_staff)
treated <- transform(hospital_staff$nurses_extended, id = nurse_id)
control <- transform(hospital_staff$controls_extended, id = nurse_id)
covars <- c("age", "experience_years", "certification_level")

m <- match_couples(treated, control, vars = covars, auto_scale = TRUE,
                   memory_mode = "implicit", certify = TRUE)
print(m$certificate$certified_optimal)
print(unlist(m$search[c("seed_width", "n_rounds", "candidate_edges", "possible_edges")]))
print(m$info$total_distance)
print(nrow(m$pairs))

bal <- balance_diagnostics(m, treated, control, vars = covars)
print(balance_table(bal))

m_cal <- match_couples(treated, control, vars = covars, auto_scale = TRUE,
                       calipers = list(age = 3, experience_years = 2),
                       max_distance = 1.5)
print(nrow(m_cal$pairs))

p <- match_path(treated, control, vars = covars, auto_scale = TRUE,
                vary = "max_distance", values = c(1.0, 1.2, 1.5, 2.0, 3.0))
print(p$path[, c("max_distance", "n_matched", "total_distance", "certified")])
