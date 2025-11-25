# Contributing to couplr

Thank you for your interest in contributing to couplr.

## Reporting Issues

Please open an issue at <https://github.com/gcol33/couplr/issues> with:

- A minimal reproducible example
- Expected vs. actual behavior
- Output of [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)

## Development Setup

``` r
# Install dependencies
install.packages(c("devtools", "testthat", "Rcpp", "RcppEigen"))

# Clone and install
git clone https://github.com/gcol33/couplr.git
cd couplr
devtools::install_deps()
devtools::load_all()
devtools::test()
```

## Pull Requests

1.  Fork the repository
2.  Create a feature branch
3.  Make your changes
4.  Run
    [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)
    and ensure all tests pass
5.  Submit a pull request

## Code Style

- Follow tidyverse style for R code
- Use C++17 for C++ code
- Add tests for new functionality
- Update documentation as needed

## License

By contributing, you agree that your contributions will be licensed
under the MIT License.
