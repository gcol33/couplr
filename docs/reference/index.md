# Package index

## LAP Solving

Core functions for solving linear assignment problems

- [`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
  : Solve linear assignment problems
- [`lap_solve_batch()`](https://gillescolling.com/couplr/reference/lap_solve_batch.md)
  : Solve multiple assignment problems efficiently
- [`lap_solve_kbest()`](https://gillescolling.com/couplr/reference/lap_solve_kbest.md)
  : Find k-best optimal assignments
- [`lap_solve_line_metric()`](https://gillescolling.com/couplr/reference/lap_solve_line_metric.md)
  : Solve 1-D Line Assignment Problem
- [`assignment()`](https://gillescolling.com/couplr/reference/assignment.md)
  : Linear assignment solver
- [`assignment_duals()`](https://gillescolling.com/couplr/reference/assignment_duals.md)
  : Solve assignment problem and return dual variables
- [`bottleneck_assignment()`](https://gillescolling.com/couplr/reference/bottleneck_assignment.md)
  : Solve the Bottleneck Assignment Problem
- [`sinkhorn()`](https://gillescolling.com/couplr/reference/sinkhorn.md)
  : 'Sinkhorn-Knopp' optimal transport solver
- [`sinkhorn_to_assignment()`](https://gillescolling.com/couplr/reference/sinkhorn_to_assignment.md)
  : Round 'Sinkhorn' transport plan to hard assignment

## Matching Functions

High-level matching for observational studies

- [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
  : Optimal matching using linear assignment
- [`greedy_couples()`](https://gillescolling.com/couplr/reference/greedy_couples.md)
  : Fast approximate matching using greedy algorithm
- [`matchmaker()`](https://gillescolling.com/couplr/reference/matchmaker.md)
  : Create blocks for stratified matching

## Balance Diagnostics

Assess and report match quality

- [`balance_diagnostics()`](https://gillescolling.com/couplr/reference/balance_diagnostics.md)
  : Balance Diagnostics for Matched Pairs
- [`balance_table()`](https://gillescolling.com/couplr/reference/balance_table.md)
  : Create Balance Table

## Distance and Preprocessing

Distance computation and data preparation

- [`compute_distances()`](https://gillescolling.com/couplr/reference/compute_distances.md)
  : Compute and Cache Distance Matrix for Reuse
- [`update_constraints()`](https://gillescolling.com/couplr/reference/update_constraints.md)
  : Update Constraints on Distance Object
- [`preprocess_matching_vars()`](https://gillescolling.com/couplr/reference/preprocess_matching_vars.md)
  : Preprocess matching variables with automatic checks and scaling
- [`diagnose_distance_matrix()`](https://gillescolling.com/couplr/reference/diagnose_distance_matrix.md)
  : Diagnose distance matrix and suggest fixes

## Joined Datasets

Create analysis-ready merged datasets

- [`join_matched()`](https://gillescolling.com/couplr/reference/join_matched.md)
  : Join Matched Pairs with Original Data
- [`augment(`*`<matching_result>`*`)`](https://gillescolling.com/couplr/reference/augment.matching_result.md)
  : Augment Matching Results with Original Data (broom-style)
- [`augment()`](https://gillescolling.com/couplr/reference/augment.md) :
  Generic Augment Function

## Utility Functions

Helper functions for working with results

- [`get_total_cost()`](https://gillescolling.com/couplr/reference/get_total_cost.md)
  : Extract total cost from assignment result
- [`get_method_used()`](https://gillescolling.com/couplr/reference/get_method_used.md)
  : Extract method used from assignment result
- [`as_assignment_matrix()`](https://gillescolling.com/couplr/reference/as_assignment_matrix.md)
  : Convert assignment result to a binary matrix
- [`is_lap_solve_result()`](https://gillescolling.com/couplr/reference/is_lap_solve_result.md)
  : Check if object is an assignment result
- [`is_lap_solve_batch_result()`](https://gillescolling.com/couplr/reference/is_lap_solve_batch_result.md)
  : Check if object is a batch assignment result
- [`is_lap_solve_kbest_result()`](https://gillescolling.com/couplr/reference/is_lap_solve_kbest_result.md)
  : Check if object is a k-best assignment result
- [`is_distance_object()`](https://gillescolling.com/couplr/reference/is_distance_object.md)
  : Check if Object is a Distance Object

## Example Data

Built-in datasets for examples and testing

- [`hospital_staff`](https://gillescolling.com/couplr/reference/hospital_staff.md)
  : Hospital staff scheduling example dataset
- [`example_costs`](https://gillescolling.com/couplr/reference/example_costs.md)
  : Example cost matrices for assignment problems
- [`example_df`](https://gillescolling.com/couplr/reference/example_df.md)
  : Example assignment problem data frame

## Pixel Morphing

Visual demonstrations and image processing

- [`pixel_morph()`](https://gillescolling.com/couplr/reference/pixel_morph.md)
  : Pixel-level image morphing (final frame only)
- [`pixel_morph_animate()`](https://gillescolling.com/couplr/reference/pixel_morph_animate.md)
  : Pixel-level image morphing (animation)

## Print, Summary and Plot Methods

S3 methods for displaying and visualizing results

- [`print(`*`<lap_solve_result>`*`)`](https://gillescolling.com/couplr/reference/print.lap_solve_result.md)
  : Print method for assignment results
- [`print(`*`<lap_solve_batch_result>`*`)`](https://gillescolling.com/couplr/reference/print.lap_solve_batch_result.md)
  : Print method for batch assignment results
- [`print(`*`<lap_solve_kbest_result>`*`)`](https://gillescolling.com/couplr/reference/print.lap_solve_kbest_result.md)
  : Print method for k-best assignment results
- [`print(`*`<matching_result>`*`)`](https://gillescolling.com/couplr/reference/print.matching_result.md)
  : Print method for matching results
- [`print(`*`<matchmaker_result>`*`)`](https://gillescolling.com/couplr/reference/print.matchmaker_result.md)
  : Print method for matchmaker results
- [`print(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/print.balance_diagnostics.md)
  : Print Method for Balance Diagnostics
- [`print(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/print.distance_object.md)
  : Print Method for Distance Objects
- [`print(`*`<preprocessing_result>`*`)`](https://gillescolling.com/couplr/reference/print.preprocessing_result.md)
  : Print method for preprocessing result
- [`print(`*`<variable_health>`*`)`](https://gillescolling.com/couplr/reference/print.variable_health.md)
  : Print method for variable health
- [`summary(`*`<lap_solve_kbest_result>`*`)`](https://gillescolling.com/couplr/reference/summary.lap_solve_kbest_result.md)
  : Get summary of k-best results
- [`summary(`*`<distance_object>`*`)`](https://gillescolling.com/couplr/reference/summary.distance_object.md)
  : Summary Method for Distance Objects
- [`summary(`*`<matching_result>`*`)`](https://gillescolling.com/couplr/reference/summary.matching_result.md)
  : Summary method for matching results
- [`summary(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/summary.balance_diagnostics.md)
  : Summary method for balance diagnostics
- [`plot(`*`<matching_result>`*`)`](https://gillescolling.com/couplr/reference/plot.matching_result.md)
  : Plot method for matching results
- [`plot(`*`<balance_diagnostics>`*`)`](https://gillescolling.com/couplr/reference/plot.balance_diagnostics.md)
  : Plot method for balance diagnostics
