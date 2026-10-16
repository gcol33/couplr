# Package index

## LAP Solving

Core functions for solving linear assignment problems

- [`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
  : Solve Linear Assignment Problems with a Tidy Interface
- [`lap_solve_batch()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`print(`*`<lap_solve_batch_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`summary(`*`<lap_solve_batch_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`is_lap_solve_batch_result()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  : Solve Many Assignment Problems
- [`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`print(`*`<lap_solve_kbest_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`summary(`*`<lap_solve_kbest_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`is_lap_solve_kbest_result()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  : Find the k Best Assignments
- [`lap_solve_line_metric()`](https://gillescolling.com/couplr/reference/lap_solve_line_metric.md)
  [`summary(`*`<lap_line_metric_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_line_metric.md)
  : Solve a One-Dimensional Assignment Problem
- [`lap_solvers()`](https://gillescolling.com/couplr/reference/lap_solvers.md)
  [`register_lap_solver()`](https://gillescolling.com/couplr/reference/lap_solvers.md)
  : Assignment Solvers
- [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`print(`*`<lap_solve_result>`*`)`](https://gillescolling.com/couplr/reference/assignment.md)
  [`summary(`*`<lap_solve_result>`*`)`](https://gillescolling.com/couplr/reference/assignment.md)
  [`is_lap_solve_result()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`get_total_cost()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`get_method_used()`](https://gillescolling.com/couplr/reference/assignment.md)
  : Solve a Linear Assignment Problem
- [`assignment_duals()`](https://gillescolling.com/couplr/reference/assignment_duals.md)
  [`print(`*`<assignment_duals_result>`*`)`](https://gillescolling.com/couplr/reference/assignment_duals.md)
  [`summary(`*`<assignment_duals_result>`*`)`](https://gillescolling.com/couplr/reference/assignment_duals.md)
  [`as.data.frame(`*`<assignment_duals_result>`*`)`](https://gillescolling.com/couplr/reference/assignment_duals.md)
  : Solve an Assignment Problem and Return Dual Potentials
- [`bottleneck_assignment()`](https://gillescolling.com/couplr/reference/bottleneck_assignment.md)
  [`summary(`*`<bottleneck_result>`*`)`](https://gillescolling.com/couplr/reference/bottleneck_assignment.md)
  : Solve the Bottleneck Assignment Problem
- [`sinkhorn()`](https://gillescolling.com/couplr/reference/sinkhorn.md)
  [`print(`*`<sinkhorn_result>`*`)`](https://gillescolling.com/couplr/reference/sinkhorn.md)
  [`summary(`*`<sinkhorn_result>`*`)`](https://gillescolling.com/couplr/reference/sinkhorn.md)
  [`as.matrix(`*`<sinkhorn_result>`*`)`](https://gillescolling.com/couplr/reference/sinkhorn.md)
  : Entropic Optimal Transport by Sinkhorn-Knopp Iteration
- [`sinkhorn_to_assignment()`](https://gillescolling.com/couplr/reference/sinkhorn_to_assignment.md)
  : Round a Sinkhorn Transport Plan to an Assignment

## Matching Functions

High-level matching for observational studies

- [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  : Optimal and Greedy Pair Matching
- [`match_path()`](https://gillescolling.com/couplr/reference/match_path.md)
  [`print(`*`<couplr_path>`*`)`](https://gillescolling.com/couplr/reference/match_path.md)
  [`summary(`*`<couplr_path>`*`)`](https://gillescolling.com/couplr/reference/match_path.md)
  [`as.data.frame(`*`<couplr_path>`*`)`](https://gillescolling.com/couplr/reference/match_path.md)
  [`plot(`*`<couplr_path>`*`)`](https://gillescolling.com/couplr/reference/match_path.md)
  : Solve a Sequence of Matchings over One Design Parameter
- [`matchmaker()`](https://gillescolling.com/couplr/reference/matchmaker.md)
  [`print(`*`<matchmaker_result>`*`)`](https://gillescolling.com/couplr/reference/matchmaker.md)
  [`summary(`*`<matchmaker_result>`*`)`](https://gillescolling.com/couplr/reference/matchmaker.md)
  : Create Blocks for Stratified Matching
- [`ps_match()`](https://gillescolling.com/couplr/reference/ps_match.md)
  : Propensity Score Matching
- [`cardinality_match()`](https://gillescolling.com/couplr/reference/cardinality_match.md)
  : Cardinality Matching
- [`full_match()`](https://gillescolling.com/couplr/reference/full_match.md)
  : Full Matching
- [`cem_match()`](https://gillescolling.com/couplr/reference/cem_match.md)
  : Coarsened Exact Matching
- [`subclass_match()`](https://gillescolling.com/couplr/reference/subclass_match.md)
  : Subclassification on Propensity Score

## Balance Diagnostics & Analysis

Assess match quality and sensitivity to hidden bias

- [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  [`print(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  [`summary(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  [`plot(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  [`autoplot(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  : Balance Diagnostics for Matched Samples
- [`balance_table()`](https://gillescolling.com/couplr/reference/balance_table.md)
  : Balance Table of a Balance Diagnostics Object
- [`autoplot(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`sensitivity_analysis()`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`print(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`summary(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`as.data.frame(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`plot(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  : Rosenbaum Sensitivity Analysis

## Optimality Verification

Check a solution against the dual prices that certify it

- [`verify_assignment()`](https://gillescolling.com/couplr/reference/verify_assignment.md)
  [`print(`*`<assignment_certificate>`*`)`](https://gillescolling.com/couplr/reference/verify_assignment.md)
  [`summary(`*`<assignment_certificate>`*`)`](https://gillescolling.com/couplr/reference/verify_assignment.md)
  : Verify That an Assignment Is Optimal
- [`verify_flow()`](https://gillescolling.com/couplr/reference/verify_flow.md)
  [`print(`*`<flow_certificate>`*`)`](https://gillescolling.com/couplr/reference/verify_flow.md)
  [`summary(`*`<flow_certificate>`*`)`](https://gillescolling.com/couplr/reference/verify_flow.md)
  : Verify That a Flow Is Optimal
- [`certificate()`](https://gillescolling.com/couplr/reference/result_accessors.md)
  [`potentials()`](https://gillescolling.com/couplr/reference/result_accessors.md)
  [`unmatched_units()`](https://gillescolling.com/couplr/reference/result_accessors.md)
  [`print(`*`<cardinality_certificate>`*`)`](https://gillescolling.com/couplr/reference/result_accessors.md)
  [`summary(`*`<cardinality_certificate>`*`)`](https://gillescolling.com/couplr/reference/result_accessors.md)
  [`glance(`*`<cardinality_certificate>`*`)`](https://gillescolling.com/couplr/reference/result_accessors.md)
  [`glance(`*`<assignment_certificate>`*`)`](https://gillescolling.com/couplr/reference/result_accessors.md)
  [`glance(`*`<flow_certificate>`*`)`](https://gillescolling.com/couplr/reference/result_accessors.md)
  : Certificate, Potentials and Unmatched Units of a Result

## Distance and Preprocessing

Distance computation and data preparation

- [`compute_distances()`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`is_distance_object()`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`print(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`summary(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`as.matrix(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  : Compute and Cache a Distance Matrix
- [`update_constraints()`](https://gillescolling.com/couplr/reference/update_constraints.md)
  : Update the Constraints of a Distance Object
- [`preprocess_matching_vars()`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  [`print(`*`<variable_health>`*`)`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  [`summary(`*`<variable_health>`*`)`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  [`print(`*`<preprocessing_result>`*`)`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  [`summary(`*`<preprocessing_result>`*`)`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  : Check Matching Variables and Choose Scaling
- [`diagnose_distance_matrix()`](https://gillescolling.com/couplr/reference/diagnose_distance_matrix.md)
  [`print(`*`<distance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/diagnose_distance_matrix.md)
  [`summary(`*`<distance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/diagnose_distance_matrix.md)
  : Diagnose a Distance Matrix
- [`estimate_dense_matrix_mb()`](https://gillescolling.com/couplr/reference/estimate_dense_matrix_mb.md)
  : Estimate the Memory of a Dense Cost Matrix
- [`estimate_dense_solve_mb()`](https://gillescolling.com/couplr/reference/estimate_dense_solve_mb.md)
  : Estimate the Peak Memory of a Dense Solve

## Joined Datasets & Output

Create analysis-ready merged datasets

- [`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md)
  [`augment(`*`<couplr_result>`*`)`](https://gillescolling.com/couplr/reference/join_matched.md)
  : Join Matched Pairs with the Original Data
- [`match_data()`](https://gillescolling.com/couplr/reference/match_data.md)
  : Extract Analysis-Ready Data from a Matching Result

## Ecosystem Integration

Interoperability with MatchIt, cobalt, and marginaleffects

- [`as_matchit()`](https://gillescolling.com/couplr/reference/as_matchit.md)
  : Convert a Matching Result to a matchit Object
- [`bal.tab(`*`<couplr_result>`*`)`](https://gillescolling.com/couplr/reference/bal.tab.couplr_result.md)
  [`bal.tab(`*`<subclass_result>`*`)`](https://gillescolling.com/couplr/reference/bal.tab.couplr_result.md)
  : Balance Tables from cobalt for Matching Results

## Utility Functions

Helper functions for working with results

- [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`print(`*`<lap_solve_result>`*`)`](https://gillescolling.com/couplr/reference/assignment.md)
  [`summary(`*`<lap_solve_result>`*`)`](https://gillescolling.com/couplr/reference/assignment.md)
  [`is_lap_solve_result()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`get_total_cost()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`get_method_used()`](https://gillescolling.com/couplr/reference/assignment.md)
  : Solve a Linear Assignment Problem
- [`explain_dispatch()`](https://gillescolling.com/couplr/reference/explain_dispatch.md)
  [`print(`*`<dispatch_explanation>`*`)`](https://gillescolling.com/couplr/reference/explain_dispatch.md)
  [`summary(`*`<dispatch_explanation>`*`)`](https://gillescolling.com/couplr/reference/explain_dispatch.md)
  : Explain the Automatic Solver Choice
- [`solver_status_values()`](https://gillescolling.com/couplr/reference/solver_status_values.md)
  : Solver Status Values
- [`as_assignment_matrix()`](https://gillescolling.com/couplr/reference/as_assignment_matrix.md)
  : Convert an Assignment Result to a Binary Matrix
- [`lap_solve_batch()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`print(`*`<lap_solve_batch_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`summary(`*`<lap_solve_batch_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`is_lap_solve_batch_result()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  : Solve Many Assignment Problems
- [`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`print(`*`<lap_solve_kbest_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`summary(`*`<lap_solve_kbest_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`is_lap_solve_kbest_result()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  : Find the k Best Assignments
- [`compute_distances()`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`is_distance_object()`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`print(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`summary(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`as.matrix(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  : Compute and Cache a Distance Matrix

## Example Data

Built-in datasets for examples and testing

- [`hospital_staff`](https://gillescolling.com/couplr/reference/hospital_staff.md)
  : Hospital Staff Example Data
- [`example_costs`](https://gillescolling.com/couplr/reference/example_costs.md)
  : Example Cost Matrices
- [`example_df`](https://gillescolling.com/couplr/reference/example_df.md)
  : Example Assignment Data Frame

## Pixel Morphing

Visual demonstrations and image processing

- [`pixel_morph()`](https://gillescolling.com/couplr/reference/pixel_morph.md)
  : Pixel-Level Image Morphing
- [`pixel_morph_animate()`](https://gillescolling.com/couplr/reference/pixel_morph_animate.md)
  [`print(`*`<pixel_morph_animation>`*`)`](https://gillescolling.com/couplr/reference/pixel_morph_animate.md)
  [`summary(`*`<pixel_morph_animation>`*`)`](https://gillescolling.com/couplr/reference/pixel_morph_animate.md)
  : Animated Pixel-Level Image Morphing

## Algorithm Animation

Step-by-step visualization of LAP algorithms

- [`lap_animate()`](https://gillescolling.com/couplr/reference/lap_animate.md)
  : Animate an Assignment Algorithm
- [`animated_methods()`](https://gillescolling.com/couplr/reference/animated_methods.md)
  : List Methods That Support Animation

## Print, Summary and Plot Methods

S3 methods for displaying and visualizing results

- [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`print(`*`<lap_solve_result>`*`)`](https://gillescolling.com/couplr/reference/assignment.md)
  [`summary(`*`<lap_solve_result>`*`)`](https://gillescolling.com/couplr/reference/assignment.md)
  [`is_lap_solve_result()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`get_total_cost()`](https://gillescolling.com/couplr/reference/assignment.md)
  [`get_method_used()`](https://gillescolling.com/couplr/reference/assignment.md)
  : Solve a Linear Assignment Problem
- [`lap_solve_batch()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`print(`*`<lap_solve_batch_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`summary(`*`<lap_solve_batch_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  [`is_lap_solve_batch_result()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  : Solve Many Assignment Problems
- [`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`print(`*`<lap_solve_kbest_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`summary(`*`<lap_solve_kbest_result>`*`)`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  [`is_lap_solve_kbest_result()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  : Find the k Best Assignments
- [`plot(`*`<matching_result>`*`)`](https://gillescolling.com/couplr/reference/couplr_result.md)
  [`autoplot(`*`<matching_result>`*`)`](https://gillescolling.com/couplr/reference/couplr_result.md)
  [`glance(`*`<couplr_result>`*`)`](https://gillescolling.com/couplr/reference/couplr_result.md)
  [`tidy(`*`<couplr_result>`*`)`](https://gillescolling.com/couplr/reference/couplr_result.md)
  [`as.data.frame(`*`<couplr_result>`*`)`](https://gillescolling.com/couplr/reference/couplr_result.md)
  [`print(`*`<couplr_result>`*`)`](https://gillescolling.com/couplr/reference/couplr_result.md)
  [`summary(`*`<couplr_result>`*`)`](https://gillescolling.com/couplr/reference/couplr_result.md)
  : Matching Results
- [`match_path()`](https://gillescolling.com/couplr/reference/match_path.md)
  [`print(`*`<couplr_path>`*`)`](https://gillescolling.com/couplr/reference/match_path.md)
  [`summary(`*`<couplr_path>`*`)`](https://gillescolling.com/couplr/reference/match_path.md)
  [`as.data.frame(`*`<couplr_path>`*`)`](https://gillescolling.com/couplr/reference/match_path.md)
  [`plot(`*`<couplr_path>`*`)`](https://gillescolling.com/couplr/reference/match_path.md)
  : Solve a Sequence of Matchings over One Design Parameter
- [`matchmaker()`](https://gillescolling.com/couplr/reference/matchmaker.md)
  [`print(`*`<matchmaker_result>`*`)`](https://gillescolling.com/couplr/reference/matchmaker.md)
  [`summary(`*`<matchmaker_result>`*`)`](https://gillescolling.com/couplr/reference/matchmaker.md)
  : Create Blocks for Stratified Matching
- [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  [`print(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  [`summary(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  [`plot(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  [`autoplot(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  : Balance Diagnostics for Matched Samples
- [`compute_distances()`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`is_distance_object()`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`print(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`summary(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  [`as.matrix(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/compute_distances.md)
  : Compute and Cache a Distance Matrix
- [`preprocess_matching_vars()`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  [`print(`*`<variable_health>`*`)`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  [`summary(`*`<variable_health>`*`)`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  [`print(`*`<preprocessing_result>`*`)`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  [`summary(`*`<preprocessing_result>`*`)`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  : Check Matching Variables and Choose Scaling
- [`autoplot(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`sensitivity_analysis()`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`print(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`summary(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`as.data.frame(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  [`plot(`*`<sensitivity_analysis>`*`)`](https://gillescolling.com/couplr/reference/sensitivity_analysis.md)
  : Rosenbaum Sensitivity Analysis
