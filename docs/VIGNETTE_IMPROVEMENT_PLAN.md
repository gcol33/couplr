# Vignette Improvement Plan for couplr

## Executive Summary

This document outlines a comprehensive plan to address the structural,
narrative, and meta-explanatory weaknesses identified in the four couplr
vignettes. The improvements focus on:

1.  **Unified documentation ecosystem** with cross-references and
    consistent structure
2.  **Shared example dataset** that recurs across vignettes
3.  **Explicit audience guidance** and difficulty indicators
4.  **Progressive narrative flow** from concept to example to
    interpretation
5.  **Discussion of limitations, edge cases, and failure modes**
6.  **Clear positioning** of the package in the R ecosystem

------------------------------------------------------------------------

## Cross-Cutting Improvements (All Vignettes)

### 1. Create a Unified Introduction Section

Add a consistent introductory block to each vignette that:

- States the purpose and target audience
- Positions the vignette within the documentation ecosystem
- Provides difficulty/prerequisite indicators
- Links to related vignettes

**Template to add at the start of each vignette (after Overview):**

``` markdown
### Who This Vignette Is For

**Audience**: [Beginners | Intermediate users | Advanced users / Algorithm developers]

**Prerequisites**:
- [List any required vignettes or knowledge]
- [e.g., "Basic R knowledge", "Familiarity with matching concepts"]

**What You'll Learn**:
- [3-5 specific takeaways]

**Time to Complete**: [Approximate reading/working time]

### Documentation Roadmap

The couplr documentation is organized as follows:

| Vignette | Focus | Difficulty |
|----------|-------|------------|
| **Getting Started** | Basic LAP solving, API introduction | Beginner |
| **Algorithms** | Mathematical foundations, solver selection | Intermediate |
| **Matching Workflows** | Production matching pipelines | Intermediate |
| **Pixel Morphing** | Scientific applications, approximations | Advanced |

*You are here: [Current vignette]*
```

### 2. Introduce a Shared Example Dataset

Create a recurring dataset that appears across all vignettes,
establishing coherence:

**Proposal**: “Hospital Staff Scheduling” dataset

- **Basic form** (getting-started): 10 nurses × 10 shifts, simple cost
  matrix
- **Extended form** (algorithms): Same problem with different cost
  structures to demonstrate algorithm differences
- **Matching form** (matching-workflows): 200 nurses needing to be
  matched with 300 controls for a study
- **Large-scale form** (pixel-morphing): Analogy to visual matching,
  demonstrate when approximations are needed

**Implementation**: Add to `data/` as `hospital_example` with
documentation in `R/data.R`

### 3. Add “Limitations and Edge Cases” Sections

Each vignette should include a dedicated section discussing:

- When the method breaks down
- Common failure modes
- Performance bottlenecks
- Infeasibility conditions
- Numerical issues

### 4. Establish Narrative Transitions

Add explicit transition paragraphs between major sections that:

- Summarize what was just covered
- Preview what comes next
- Explain *why* the transition makes sense

------------------------------------------------------------------------

## Vignette-Specific Improvements

------------------------------------------------------------------------

## 1. Getting Started (getting-started.Rmd)

### Current State

- 394 lines
- Jumps directly into examples
- Good technical content but reads like reference documentation
- Missing orientation and positioning

### Improvements

#### A. Add Explicit Orientation (New Section After Line 22)

**Insert after “## Overview”:**

``` markdown
### Why couplr?

The linear assignment problem (LAP) appears throughout data science, operations research, and scientific computing. While other R packages address LAP (see Comparison below), couplr distinguishes itself through:

1. **Tidy integration**: First-class support for tibbles, dplyr workflows, and grouped data
2. **Algorithm selection**: 12+ algorithms with automatic selection based on problem structure
3. **Production matching**: High-level matching functions for observational studies (v1.0.0)
4. **Visual applications**: Pixel morphing and transport visualization

**Alternative packages**:
- `clue`: General purpose optimization (LAP is one feature among many)
- `lpSolve`: Linear programming focus, less specialized for assignment
- `RcppHungarian`: Single algorithm, no tidy interface

couplr focuses specifically on assignment problems with a user-friendly API and modern R idioms.

### Who This Vignette Is For

**Audience**: Beginners to couplr, R users familiar with basic matrix operations

**Prerequisites**:
- Basic R knowledge (data frames, functions)
- Understanding of what a "cost" or "distance" matrix means

**What You'll Learn**:
- How to solve basic assignment problems with `lap_solve()`
- Working with data frames, rectangular problems, and forbidden assignments
- Batch solving and finding multiple solutions
- When to use different algorithms

**Time to Complete**: 20-30 minutes
```

#### B. Add Narrative Frame: Follow One Problem Through

Restructure the vignette around a single problem that evolves:

1.  **Introduction**: Hospital shift scheduling (simple 3×3)
2.  **Data frame input**: Same problem from a database
3.  **Rectangular**: More shifts than nurses
4.  **Forbidden**: Some nurses can’t work certain shifts
5.  **Maximization**: Preference scores instead of costs
6.  **Batch**: Multiple days of scheduling
7.  **K-best**: Alternative schedules for flexibility

**Modification to existing content**: Keep the current examples but
frame them as variations of the hospital problem. Add narrative
transitions like:

> “Now that we can solve the basic problem, what happens when we have
> more shifts than nurses to fill them?”

#### C. Add “Common Pitfalls” Section (Before Summary)

``` markdown
## Common Pitfalls and Troubleshooting

### Problem: "All assignments have Inf cost"

**Cause**: Too many forbidden entries (NA/Inf) make the problem infeasible.

**Solution**: Check that a feasible solution exists. For rectangular problems with forbidden entries, ensure at least one valid assignment exists for each source.

```r
# Check feasibility
feasible <- rowSums(is.finite(cost)) > 0
if (!all(feasible)) {
  warning("Sources ", which(!feasible), " have no valid targets")
}
```

### Problem: Unexpected assignments

**Cause**: Cost matrix orientation wrong (rows vs columns swapped).

**Solution**: Remember: rows = sources, columns = targets. The result
`match` vector has length = nrow, and `match[i]` is the column assigned
to row i.

### Problem: Different results with different methods

**Cause**: Multiple optimal solutions may exist. Different algorithms
may find different optima with the same total cost.

**Solution**: If you need deterministic results, set a seed or use
`method = "hungarian"` which has deterministic tie-breaking.

### Problem: Slow performance on large problems

**Cause**: O(n³) complexity for exact algorithms.

**Solution**: - For n \> 1000: Consider `method = "auction"` - For n \>
5000: Use blocking or greedy matching
([`vignette("matching-workflows")`](https://gillescolling.com/couplr/articles/matching-workflows.md)) -
For n \> 10000: Use approximation strategies
([`vignette("pixel-morphing")`](https://gillescolling.com/couplr/articles/pixel-morphing.md))


    #### D. Improve Section Transitions

    Add transition paragraphs. Example between "Basic Usage" and "Working with Rectangular Problems":

    > "The examples above assumed equal numbers of sources and targets. But real-world problems rarely have this symmetry—a hospital may have 20 nurses but need to cover 30 shifts, or have 50 tasks but only budget for 40 workers. couplr handles these rectangular problems automatically."

    ---

    ## 2. Algorithms (algorithms.Rmd)

    ### Current State
    - 667 lines
    - Good mathematical content
    - Stays at intuitive level, lacks formal structure
    - No explicit comparison between algorithms
    - No failure modes discussed

    ### Improvements

    #### A. Add Structured Algorithm Template

    Restructure each algorithm section to follow a consistent pattern:

    ```markdown
    ### [Algorithm Name]

    **Complexity**: [Big-O notation]

    **When to Use**:
    - [Bullet points of ideal conditions]

    **When NOT to Use**:
    - [Bullet points of poor conditions]

    #### Core Idea

    [1-2 paragraph intuitive explanation]

    #### Formal Description

    [Mathematical formulation with LaTeX]

    **Input**: [Describe input requirements]

    **Output**: [Describe output guarantees]

    **Algorithm Steps**:
    1. [Numbered steps with mathematical notation]

    #### Implementation Notes

    [Practical considerations, numerical issues]

    #### Example

    [Code demonstrating the algorithm]

    #### Failure Modes and Edge Cases

    - [When the algorithm may fail or perform poorly]
    - [Numerical stability concerns]
    - [Special cases to watch for]

#### B. Add Comparative Analysis Section

**Insert after “## Algorithms in couplr” (around line 68):**

``` markdown
### Algorithm Selection Guide

Before diving into individual algorithms, here's a decision framework:
```

``` R
                Is the cost matrix binary (0/1)?
                          |
          ┌───────────────┴───────────────┐
          Yes                             No
          |                               |
      Use HK01                    Is sparsity > 50%?
                                          |
                          ┌───────────────┴───────────────┐
                          Yes                             No
                          |                               |
                      Use SAP                      Is n > 1000?
                                                          |
                                          ┌───────────────┴───────────────┐
                                          Yes                             No
                                          |                               |
                                      Use Auction              Use JV (default)
                                      or consider
                                      approximations
```


    ### Head-to-Head Comparisons

    | Scenario | Hungarian | JV | Auction | SAP | HK01 |
    |----------|-----------|----|---------|----|------|
    | Dense 100×100 | ✓✓✓ | ✓✓✓ | ✓✓ | ✓ | N/A |
    | Dense 1000×1000 | ✓ | ✓✓✓ | ✓✓✓ | ✓ | N/A |
    | Sparse 80% forbidden | ✓ | ✓✓ | ✓ | ✓✓✓ | N/A |
    | Binary costs | ✓ | ✓ | ✓ | ✓ | ✓✓✓ |
    | Rectangular n×2n | ✗ | ✓✓ | ✗ | ✓✓✓ | ✓✓ |
    | Numerical precision | ✓✓✓ | ✓✓ | ✓ | ✓✓ | ✓✓✓ |

    ✓✓✓ = Excellent, ✓✓ = Good, ✓ = Acceptable, ✗ = Not recommended

#### C. Add “Limitations and Numerical Issues” Section

``` markdown
## Limitations and Numerical Issues

### When LAP Algorithms Fail

**1. Infeasible Problems**

If too many entries are forbidden (NA/Inf), no valid assignment may exist:

```r
# Infeasible: no column reachable from row 2
cost <- matrix(c(1, 2, Inf, Inf), nrow = 2)
# lap_solve(cost) will error or return partial solution
```

**Detection**: Check `rowSums(is.finite(cost)) > 0` for all rows.

**2. Nearly Degenerate Problems**

When many costs are nearly equal, small numerical errors can cause: -
Different algorithms finding different solutions - Cycling in auction
algorithms - Incorrect optimality certification

**Mitigation**: Use `method = "hungarian"` for maximum numerical
stability, or add small random perturbations.

**3. Overflow in Dual Variables**

For very large cost ranges (\> 10^10), dual variable updates may
overflow:

``` r

# Problematic: cost range is 10^15
cost <- matrix(c(1e-5, 1e10, 1e10, 1e-5), nrow = 2)
```

**Mitigation**: Scale costs to a reasonable range (0 to 10^6) before
solving.

### Algorithm-Specific Issues

| Algorithm | Known Issues                       | Mitigation               |
|-----------|------------------------------------|--------------------------|
| Hungarian | Slow for n \> 500                  | Use JV or Auction        |
| JV        | May cycle with ties                | Use tie-breaking epsilon |
| Auction   | Slow convergence for small epsilon | Use scaled variant       |
| SAP       | Inefficient for dense problems     | Use JV instead           |
| HK01      | Only for binary costs              | Not applicable otherwise |


    #### D. Add Conceptual Diagram Section

    **Insert after Formal Definition (around line 45):**

    ```markdown
    ### Visualizing the Assignment Problem

    **Bipartite Graph Representation**

    Imagine the LAP as a weighted bipartite graph:

Sources (rows) Targets (columns)

``` R
S₁ ───────2──────── T₁
 │ ╲     ╱         ╱│
 │  ╲4  ╱3        ╱ │
 3   ╲ ╱        5   1
 │    ╳          ╱  │
 │   ╱ ╲       ╱    │
S₂ ─╱───╲──1──╱──── T₂
   ╱     ╲   ╱     ╱
  ╱       ╲ ╱    2
S₃ ────3───╳───── T₃
```

Edge weights = costs Goal: Select one edge per source, one per target,
minimizing total weight


    **Complementary Slackness Intuition**

    At optimality, think of dual variables as "prices":
    - $u_i$ = how much source $i$ is willing to pay
    - $v_j$ = how much target $j$ is worth

    An assignment $(i,j)$ is used only if $u_i + v_j = c_{ij}$ (price equals cost).

------------------------------------------------------------------------

## 3. Matching Workflows (matching-workflows.Rmd)

### Current State

- 1134 lines (longest vignette)
- Applied and concrete
- Reads like isolated steps rather than cohesive journey
- Missing interpretation guidance and pitfall discussion
- Good real-world example at end but comes late

### Improvements

#### A. Move Real-World Example Earlier

Restructure to follow the real-world example throughout:

1.  **Introduction**: Present the job training evaluation scenario
2.  **Basic matching**: First attempt with
    [`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
3.  **Preprocessing**: Why we need `auto_scale = TRUE`
4.  **Balance assessment**: Is our match good enough?
5.  **Refinements**: Adding calipers, trying greedy
6.  **Publication output**: Tables and effect estimates

This makes the vignette a narrative journey rather than a feature
catalog.

#### B. Add “What Can Go Wrong” Section

**Insert after “## Automatic Preprocessing” (around line 189):**

``` markdown
### What Can Go Wrong

#### 1. Poor Match Quality Despite Convergence

**Symptom**: `match_couples()` completes but balance diagnostics show |std diff| > 0.25.

**Causes**:
- Fundamental differences between groups that matching cannot overcome
- Important confounders not included in matching variables
- Caliper too loose

**Solutions**:
- Add more matching variables
- Tighten caliper (accept fewer matches)
- Consider different estimand (ATT vs ATE)
- Report sensitivity analysis

#### 2. Few or No Matches

**Symptom**: `n_matched` is much smaller than expected.

**Causes**:
- Caliper too strict
- Non-overlapping support (groups truly different)
- Blocking variables create small strata

**Diagnosis**:
```r
# Check overlap
ggplot(bind_rows(left, right), aes(x = age, fill = group)) +
  geom_density(alpha = 0.5)
```

**Solutions**: - Relax caliper - Use coarser blocking - Consider
propensity score matching with wider bands

#### 3. Imbalanced but Optimal

**Symptom**: Optimal matching completes but balance is worse than
greedy.

**Reality**: Optimal minimizes *total distance*, not balance metrics. A
globally optimal solution may sacrifice balance on one variable to gain
on another.

**Solutions**: - Try greedy matching (may find different local
optimum) - Add explicit balance constraints (future feature) - Use
matching with replacement (different estimand)

#### 4. Computational Issues

**Symptom**:
[`match_couples()`](https://gillescolling.com/couplr/reference/match_couples.md)
takes \> 1 minute or crashes.

**Causes**: - n \> 5000 for optimal matching - Full cost matrix doesn’t
fit in memory

**Solutions**:

``` r

# For n > 5000: use greedy
result <- greedy_couples(left, right, vars = vars, strategy = "sorted")

# For n > 10000: use blocking
blocks <- matchmaker(left, right, block_type = "cluster", n_blocks = 20)
result <- match_couples(blocks$left, blocks$right, vars = vars, block_id = "block_id")
```


    #### C. Add Interpretation Guidance Throughout

    After each balance diagnostic output, add interpretation:

    ```markdown
    ### Interpreting Balance Results

    ```r
    print(balance)
    #> Balance Diagnostics
    #> -------------------
    #> Variables: age, income, education
    #>
    #> Variable Statistics:
    #>   variable mean_left mean_right std_diff var_ratio ks_stat ks_p
    #>   age      45.2      45.8       -0.08    0.95      0.06    0.89
    #>   income   58000     56500      0.12     1.08      0.09    0.45
    #>   education 2.1      2.0        0.15     0.98      0.08    0.62

**How to read this**:

| Metric | Value | Interpretation |
|----|----|----|
| std_diff | -0.08 for age | Excellent (\<0.1), means differ by 0.08 pooled SDs |
| std_diff | 0.12 for income | Good (0.1-0.25), minor imbalance acceptable |
| var_ratio | 0.95-1.08 | All close to 1.0, spread is similar |
| ks_p | \> 0.05 for all | Distributions not significantly different |

**Overall assessment**: This is a well-balanced match. Proceed with
analysis.

**If balance were poor** (\|std_diff\| \> 0.25): 1. Check for outliers
in the problematic variable 2. Consider adding the variable to blocking
3. Tighten caliper 4. Report and discuss in limitations


    #### D. Add Section Transitions

    Example transition after "Basic Usage":

    > "We've created our first matched sample, but how do we know it's any good? A match that pairs units with wildly different characteristics defeats the purpose. In the next section, we examine how automatic preprocessing prevents common data problems from degrading match quality."

    ---

    ## 4. Pixel Morphing (pixel-morphing.Rmd)

    ### Current State
    - 884 lines
    - Excellent pedagogical intent
    - Conceptually rich but jumps between intuition and application
    - Purpose not fully clear (teach intuition? show feature? provide motivation?)
    - No connection to practical workflows

    ### Improvements

    #### A. Clarify Purpose Upfront

    **Replace current Overview with:**

    ```markdown
    ## Overview

    This vignette serves three purposes:

    1. **Visual intuition**: Use image morphing to build intuition for assignment problems. Pixels are entities, colors are features, positions are spatial coordinates—a direct analog to scientific matching.

    2. **Scalability strategies**: Demonstrate three approximation approaches when exact LAP becomes infeasible (n > 1000).

    3. **Scientific applications**: Show how the same algorithms apply to ecology, physics, and chemistry.

    **This vignette is unusual**: Unlike the other couplr documentation, it emphasizes *understanding* over *doing*. If you're looking to solve a matching problem, start with `vignette("getting-started")` or `vignette("matching-workflows")`. Come here when you want to understand *why* these algorithms work and *when* approximations are appropriate.

    ### Who This Vignette Is For

    **Audience**: Advanced users, researchers, algorithm developers

    **Prerequisites**:
    - Familiarity with `lap_solve()` (`vignette("getting-started")`)
    - Basic understanding of complexity (Big-O notation)
    - Interest in algorithm design or scientific applications

    **What You'll Learn**:
    - Why exact LAP becomes infeasible for large n
    - Three approximation strategies and their trade-offs
    - How matching problems appear in ecology, physics, and chemistry
    - Mathematical connections to optimal transport theory

    **Time to Complete**: 45-60 minutes (conceptual reading)

#### B. Add Explicit Conceptual Scaffolding

**Insert after Overview, before “The General Matching Problem”:**

``` markdown
## How This Vignette Is Organized

We follow a specific arc:
```

Exact Solution Approximation Strategies Applications (Foundation) (When
exact fails) (Real-world) │ │ │ ▼ ▼ ▼ What is the 1. Feature
quantization Ecology: plots matching problem? 2. Hierarchical decomp.
Physics: particles │ 3. Resolution reduction Chemistry: atoms │ │ │ ▼ ▼
▼ Visual demo: Visual comparison Pseudocode for pixel morphing of
approximations each domain


    **Key insight**: The pixel morph is not just decoration—it's a computational testbed. Each pixel is an entity with features (color) and position, making the abstract matching problem concrete and visual.

#### C. Add “Connection to Workflows” Section

**Insert before Summary:**

``` markdown
## Connection to couplr Workflows

### When Do These Strategies Apply?

The approximation strategies in this vignette become relevant in couplr's matching functions:

| Strategy | couplr Implementation | When to Use |
|----------|----------------------|-------------|
| Exact LAP | `match_couples(method = "hungarian")` | n < 3000 |
| Feature quantization | Implicit in `scale = "robust"` | Reduces effective feature space |
| Hierarchical | `matchmaker(block_type = "cluster")` | n > 3000, use blocking |
| Resolution reduction | Future feature | Very large n |

### Practical Recommendations

**For n < 3000**: Use `match_couples()` with exact algorithms. Runtime is seconds.

**For 3000 < n < 10000**:
```r
# Use blocking to create smaller subproblems
blocks <- matchmaker(left, right, block_type = "cluster", n_blocks = 10)
result <- match_couples(blocks$left, blocks$right, vars, block_id = "block_id")
```

**For n \> 10000**:

``` r

# Use greedy matching
result <- greedy_couples(left, right, vars, strategy = "sorted")
```

**For n \> 50000**: Consider the approximation strategies in this
vignette, implemented via custom code using
[`lap_solve()`](https://gillescolling.com/couplr/reference/lap_solve.md)
on subproblems.


    #### D. Add Limitations Section

    ```markdown
    ## Limitations of Approximation Strategies

    ### Feature Quantization

    **Works well when**: Features cluster naturally into distinct groups (e.g., vegetation types, particle species, atom types).

    **Fails when**:
    - Feature space is continuous with no natural clusters
    - Important distinctions exist within clusters
    - Spatial structure doesn't align with feature groups

    **Artifacts**: Band-like motion, loss of individual trajectories, discrete jumps.

    ### Hierarchical Decomposition

    **Works well when**: Spatial locality is meaningful (nearby entities should match nearby).

    **Fails when**:
    - Optimal matches cross spatial boundaries (e.g., long-distance particle transport)
    - Patch boundaries cut through important structures
    - Hierarchy depth is mismatched to problem structure

    **Artifacts**: Boundary discontinuities, suboptimal cross-region matches.

    ### Resolution Reduction

    **Works well when**: Coarse structure is sufficient, fine detail is noise.

    **Fails when**:
    - Fine-grained structure matters (individual atom positions, exact pixel colors)
    - Upscaling introduces ambiguity

    **Artifacts**: Blocky results, non-bijective mappings (multiple fine entities per coarse cell).

------------------------------------------------------------------------

## Implementation Checklist

### Phase 1: Cross-Cutting (Do First)

Create shared example dataset (`hospital_example`)

Write standard documentation roadmap block

Create transition paragraph templates

### Phase 2: Getting Started

Add orientation paragraph (why couplr, positioning)

Add “Who This Vignette Is For” block

Restructure around hospital scheduling narrative

Add “Common Pitfalls” section

Add section transitions

Review and improve cross-references

### Phase 3: Algorithms

Apply structured algorithm template to each algorithm

Add comparative analysis section with decision tree

Add “Limitations and Numerical Issues” section

Add conceptual diagram (bipartite graph)

Add algorithm-specific failure modes

Improve cross-references

### Phase 4: Matching Workflows

Restructure to follow job training example throughout

Add “What Can Go Wrong” section

Add interpretation guidance after each diagnostic

Add section transitions

Shorten/consolidate redundant sections

Improve cross-references

### Phase 5: Pixel Morphing

Clarify purpose in Overview

Add conceptual scaffolding diagram

Add “Connection to Workflows” section

Add limitations of approximation strategies

Improve cross-references

### Phase 6: Final Integration

Verify all cross-references work

Check shared dataset is used consistently

Build vignettes and verify rendering

Review narrative flow across all vignettes

Final copyedit pass

------------------------------------------------------------------------

## Estimated Effort

| Vignette           | Current Lines | Estimated Change            | Effort |
|--------------------|---------------|-----------------------------|--------|
| Getting Started    | 394           | +100-150 lines              | Medium |
| Algorithms         | 667           | +150-200 lines              | High   |
| Matching Workflows | 1134          | +50-100 lines (restructure) | High   |
| Pixel Morphing     | 884           | +100-150 lines              | Medium |
| Shared dataset     | N/A           | New file                    | Low    |

**Total estimated effort**: 3-5 focused work sessions

------------------------------------------------------------------------

## Success Criteria

After improvements, each vignette should:

1.  **Clearly state** who it’s for and what they’ll learn
2.  **Position itself** within the documentation ecosystem
3.  **Follow a narrative** that builds logically
4.  **Provide interpretation** not just mechanics
5.  **Discuss limitations** honestly
6.  **Cross-reference** related vignettes appropriately
7.  **Use the shared dataset** where appropriate for coherence

The documentation as a whole should feel like chapters of a book rather
than independent reference documents.
