# Contributing to couplr

Thank you for considering contributing to couplr! This document provides guidelines for contributing to the project.

## Code of Conduct

Be respectful and constructive. We're building tools for the R community.

## Documentation Standards

### No Fabricated Data

**CRITICAL**: Never include made-up numbers, benchmarks, or example outputs in documentation.

- ❌ **Never** fabricate benchmark results
- ❌ **Never** invent example outputs
- ❌ **Never** guess performance numbers
- ✅ **Always** run actual code to get real results
- ✅ **Always** ask maintainers if you need benchmark data
- ✅ **Always** mark placeholder values clearly: `# TODO: Run actual benchmark`

**Why this matters**: Fabricated data misleads users about package performance and creates trust issues. If you need benchmark results but can't run them, ask maintainers or leave a clear TODO marker.

**Example - WRONG**:
```r
# Made-up numbers
bench::mark(...)
#> median: 850ms  # ← This is fake!
```

**Example - RIGHT**:
```r
# TODO: Run actual benchmark with n=1000
# Expected to be faster than baseline by ~10x based on algorithm complexity
```

## Getting Started

### Prerequisites

- R (>= 3.5.0)
- C++17 compiler (Rtools on Windows, Xcode on macOS, g++ on Linux)
- Required R packages: `devtools`, `testthat`, `Rcpp`, `RcppEigen`

```r
install.packages(c("devtools", "testthat", "Rcpp", "RcppEigen"))
```

### Setting Up Your Development Environment

1. Fork the repository on GitHub
2. Clone your fork locally:
   ```bash
   git clone https://github.com/YOUR-USERNAME/couplr.git
   cd couplr
   ```

3. Install dependencies and load the package:
   ```r
   devtools::install_deps()
   devtools::load_all()
   ```

4. Run tests to verify setup:
   ```r
   devtools::test()
   ```

## Development Workflow

### Making Changes

1. Create a new branch for your feature or fix:
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. Make your changes and test them:
   ```r
   devtools::load_all()    # Reload package
   devtools::test()         # Run all tests
   devtools::check()        # Full package check
   ```

3. Document your changes:
   - Add roxygen comments to new functions
   - Update NEWS.md for user-facing changes
   - Run `devtools::document()` to update man pages

4. Commit with descriptive messages:
   ```bash
   git add .
   git commit -m "Add feature: brief description"
   ```

### C++ Development

After modifying C++ code:

```r
# Regenerate Rcpp exports
Rcpp::compileAttributes()

# Update documentation
devtools::document()

# Reload and test
devtools::load_all()
devtools::test()
```

**Windows users:** If you encounter compilation errors, clean stale object files:
```powershell
Remove-Item src\*.o, src\*.dll -Force
```

Then rebuild:
```r
devtools::clean_dll()
devtools::load_all()
```

## What to Contribute

### Bug Reports

Open an issue at https://github.com/gcol33/couplr/issues with:
- Minimal reproducible example
- Expected vs. actual behavior
- Session info: `sessionInfo()`

### Feature Requests

Propose new features by opening an issue. Describe:
- Use case and motivation
- Proposed API/interface
- Any implementation ideas

### Code Contributions

We welcome:
- Bug fixes
- New LAP algorithm implementations
- Performance improvements
- Documentation improvements
- Test coverage expansion

**Priority areas:**
- Additional distance metrics
- New greedy strategies
- Improved auto-selection heuristics
- Vignettes with real-world examples

## Code Style

### R Code

- Follow tidyverse style guide
- Use `|>` pipe operator (not `%>%`)
- Prefix internal functions with `.`
- Use snake_case for function names
- Add roxygen documentation for all exported functions

```r
#' Match couples using optimal assignment
#'
#' @param left Data frame with left group
#' @param right Data frame with right group
#' @param vars Character vector of matching variables
#' @return A matching_result object
#' @export
match_couples <- function(left, right, vars) {
  # Implementation
}
```

### C++ Code

- Use C++17 features
- Follow existing naming conventions
- Add comments for complex algorithms
- Export functions with `// [[Rcpp::export]]`

```cpp
// [[Rcpp::export]]
Rcpp::IntegerVector lap_solve_hungarian(const Rcpp::NumericMatrix& cost) {
  // Implementation
}
```

## Testing

### Writing Tests

Add tests for all new functionality:

```r
test_that("match_couples handles empty datasets", {
  left <- data.frame(x = numeric(0))
  right <- data.frame(x = numeric(0))

  expect_error(match_couples(left, right, vars = "x"))
})
```

### Running Tests

```r
# All tests (1382+ tests)
devtools::test()

# Specific test file
testthat::test_file("tests/testthat/test-matching.R")

# Filter by pattern
devtools::test(filter = "balance")

# Parallel execution
devtools::test(parallel = TRUE)
```

## Adding New LAP Solvers

1. Create `src/solvers/solve_yourmethod.cpp`:
   ```cpp
   #include <Rcpp.h>

   Rcpp::IntegerVector solve_yourmethod_impl(const Rcpp::NumericMatrix& cost) {
     // Algorithm implementation
     // Return 0-based assignment vector
   }
   ```

2. Export in `src/interface/rcpp_interface.cpp`:
   ```cpp
   // [[Rcpp::export]]
   Rcpp::IntegerVector lap_solve_yourmethod(const Rcpp::NumericMatrix& cost) {
     return solve_yourmethod_impl(cost);
   }
   ```

3. Add to method selection in `R/assignment.R`:
   ```r
   assignment <- function(..., method = "auto") {
     result <- switch(method,
       hungarian = lap_solve_hungarian(cost),
       yourmethod = lap_solve_yourmethod(cost),
       # ...
     )
   }
   ```

4. Generate exports and document:
   ```r
   Rcpp::compileAttributes()
   devtools::document()
   ```

5. Add tests in `tests/testthat/test-yourmethod.R`

## Documentation

### Function Documentation

Use roxygen2 comments:

```r
#' @param x Numeric vector
#' @param na.rm Logical; remove NA values?
#' @return Numeric scalar
#' @examples
#' compute_mean(c(1, 2, 3))
#' @export
```

### Vignettes

Vignettes live in `vignettes/`. Create new ones with:

```r
usethis::use_vignette("topic-name")
```

## Pull Request Process

1. Push your branch to GitHub:
   ```bash
   git push origin feature/your-feature-name
   ```

2. Open a pull request at https://github.com/gcol33/couplr/pulls

3. Ensure:
   - All tests pass (`devtools::check()`)
   - Code follows style guidelines
   - Documentation is updated
   - NEWS.md includes your changes

4. Respond to review feedback

5. Once approved, maintainers will merge

## Release Process

Maintainers handle releases. The process:

1. Update version in DESCRIPTION
2. Update NEWS.md with release date
3. Run `devtools::check()`
4. Commit and tag: `git tag v1.0.1`
5. Push: `git push --tags`
6. Submit to CRAN (if applicable)

## Common Issues

### Compilation Errors on Windows

- Install/update Rtools: https://cran.r-project.org/bin/windows/Rtools/
- Verify C++17 support: `Rcpp::evalCpp("__cplusplus")`
- Clean and rebuild if binaries are stale

### Test Failures

- Run single test for debugging: `testthat::test_file()`
- Check for floating-point tolerance: use `expect_equal(..., tolerance = 1e-6)`
- Verify test independence: tests should not depend on execution order

### Documentation Not Updating

```r
# Force regeneration
devtools::clean_docs()
devtools::document()
```

## Getting Help

- Open an issue: https://github.com/gcol33/couplr/issues
- Read CLAUDE.md for comprehensive development documentation
- Check existing issues and pull requests

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

**For detailed architecture, testing strategies, and development workflows, see [CLAUDE.md](CLAUDE.md).**
