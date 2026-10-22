# Story: couplr

## Claim
Optimal one-to-one pairing of two groups by linear assignment, solved exactly in C++, with matching designs, balance diagnostics and the assignment solvers themselves.

## Audience
Researchers matching treated and control units (observational studies) or plots, sites and samples; may know MatchIt or optmatch.

## Relation
- to: none (sits beside MatchIt, optmatch, designmatch; interoperates through as_matchit())
- home: vignettes/comparison.Rmd ("Comparison with Alternatives")

## Arcs

### A1 The pairing with the smallest total distance
- question: are these the best pairs?
- answer: match_couples() solves the assignment exactly (Jonker-Volgenant by default); deterministic, certifiable; ties may depend on row order
- home: vignettes/getting-started.Rmd, vignettes/algorithms.Rmd
- also: README (opening, Usage), DESCRIPTION

### A2 Large pools
- answer: memory_mode lazy/implicit keep the solve exact without the full distance matrix; method = "greedy" trades the guarantee for speed
- limits: greedy is heuristic
- home: vignettes/matching-workflows.Rmd (Performance Considerations), vignettes/troubleshooting.Rmd
- also: README

### A3 Constraints and designs
- answer: scaling, calipers, max_distance, blocks, ratio; full, CEM, cardinality, propensity matching and subclassification
- home: vignettes/matching-workflows.Rmd, help pages
- also: README

### A4 Is the match good enough
- answer: balance_diagnostics(), sensitivity_analysis(), as_matchit() for cobalt/marginaleffects
- home: vignettes/matching-workflows.Rmd
- also: README

### A5 The solver layer
- answer: lap_solve() with nineteen named solvers (lap_solvers()), k-best, batch, bottleneck, Sinkhorn; pixel_morph() as visual demo
- home: vignettes/algorithms.Rmd, vignettes/pixel-morphing.Rmd
- also: README

## Documents
- DESCRIPTION: claim, solver list
- README / landing: claim, mechanism, Usage (A1, A4), one section per arc, routing
- Quick Start: tutorial
- Matching Workflows, Algorithm Collection, Comparison, Advanced Topics (pixel-morphing.Rmd), Troubleshooting

## Open
- pixel-morphing.Rmd carries the title "Advanced Topics" although its content is pixel morphing, approximation strategies and optimal transport.
- balance_diagnostics(result, treated, control) without `vars` errors after an auto_scale match ("vars must be specified"); the README passes `vars`.
