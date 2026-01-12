# Rcpp Interface Refactoring Plan

## Goal
Separate pure C++ algorithm code from Rcpp wrapper layer to enable direct C++ unit testing.

## Current State

**Files**: 29 .cpp files, 41 total source files
**Rcpp contamination**: All solvers depend on Rcpp types and `LAP_ERROR` macro

**Dependency pattern in solvers**:
```cpp
#include <Rcpp.h>
Rcpp::List solve_foo_impl(Rcpp::NumericMatrix cost, bool maximize) {
    // Convert to std::vector (already done internally!)
    std::vector<double> work_cost(cost_nv.begin(), cost_nv.end());

    // Pure algorithm using std::vector
    ...

    // Convert back to Rcpp
    return make_result(match, total);
}
```

The algorithms already use `std::vector` internally - they just have Rcpp at the boundaries.

## Target Architecture

```
src/
├── core/                    # Pure C++ (NO Rcpp)
│   ├── lap_types.h          # NEW: CostMatrix, Assignment, LapResult
│   ├── lap_error.h          # NEW: LapException, error handling
│   ├── lap_utils_pure.h     # Pure C++ utilities (split from lap_utils.h)
│   └── lap_utils_pure.cpp
│
├── solvers/                 # Pure C++ algorithms (NO Rcpp)
│   ├── solve_jv_pure.cpp    # Uses lap_types.h, throws LapException
│   ├── solve_hungarian_pure.cpp
│   └── ...
│
├── interface/               # Rcpp layer ONLY
│   ├── lap_utils_rcpp.h     # Rcpp-specific utilities
│   ├── lap_utils_rcpp.cpp   # Type conversion, make_result, etc.
│   └── prepare_cost_matrix.cpp
│
├── rcpp_interface.cpp       # [[Rcpp::export]] wrappers
└── RcppExports.cpp          # Auto-generated
```

## New Pure C++ Types (lap_types.h)

```cpp
#pragma once
#include <vector>
#include <string>

namespace lap {

// Cost matrix: row-major flat vector + dimensions
struct CostMatrix {
    std::vector<double> data;  // row-major
    std::vector<int> mask;     // 1=allowed, 0=forbidden
    int nrow;
    int ncol;

    double& at(int i, int j) { return data[i * ncol + j]; }
    double at(int i, int j) const { return data[i * ncol + j]; }
    bool allowed(int i, int j) const { return mask[i * ncol + j] != 0; }
};

// Result of LAP solver
struct LapResult {
    std::vector<int> assignment;  // 0-based column indices, -1 = unmatched
    double total_cost;
    std::string status;           // "optimal", "infeasible", etc.
};

// For k-best
struct KBestResult {
    std::vector<LapResult> solutions;
};

}  // namespace lap
```

## New Error Handling (lap_error.h)

```cpp
#pragma once
#include <stdexcept>
#include <string>

namespace lap {

class LapException : public std::runtime_error {
public:
    explicit LapException(const std::string& msg) : std::runtime_error(msg) {}
};

// Replaces LAP_ERROR macro in pure C++ code
#define LAP_THROW(msg) throw lap::LapException(msg)

}  // namespace lap
```

## Refactoring Steps

### Phase 1: Create Pure C++ Infrastructure
1. Create `src/core/lap_types.h` with pure C++ types
2. Create `src/core/lap_error.h` with exception handling
3. Create `src/core/lap_utils_pure.h` - pure C++ utilities split from lap_utils.h

### Phase 2: Refactor Solvers (start with one as template)
Pick `solve_jv.cpp` as the pilot:

**Before** (solve_jv.cpp):
```cpp
#include <Rcpp.h>
Rcpp::List solve_jv_impl(Rcpp::NumericMatrix cost, bool maximize) {
    // ... uses Rcpp types
}
```

**After** (solve_jv_pure.cpp):
```cpp
#include "core/lap_types.h"
#include "core/lap_error.h"

namespace lap {
LapResult solve_jv(const CostMatrix& cost, bool maximize) {
    // Pure C++ algorithm
    if (cost.nrow > cost.ncol) LAP_THROW("Infeasible: rows > cols");
    // ... algorithm ...
    return LapResult{assignment, total, "optimal"};
}
}
```

**Wrapper** (rcpp_interface.cpp):
```cpp
#include <Rcpp.h>
#include "solvers/solve_jv_pure.h"
#include "interface/lap_utils_rcpp.h"

// [[Rcpp::export]]
Rcpp::List solve_jv_impl(Rcpp::NumericMatrix cost, bool maximize) {
    try {
        lap::CostMatrix cm = rcpp_to_cost_matrix(cost, maximize);
        lap::LapResult result = lap::solve_jv(cm, maximize);
        return lap_result_to_rcpp(result, cost);
    } catch (const lap::LapException& e) {
        Rcpp::stop(e.what());
    }
}
```

### Phase 3: Refactor All Solvers
Apply same pattern to all ~20 solvers:
- solve_hungarian, solve_ssp, solve_csflow, solve_auction (x3)
- solve_hk01, solve_line_metric, solve_ssap_bucket
- solve_csa, solve_cycle_cancel, solve_push_relabel
- solve_network_simplex, solve_orlin, solve_ramshaw_tarjan
- solve_lapmod, solve_bruteforce, solve_bottleneck
- solve_murty, solve_kbest_lawler
- solve_gabow_tarjan, solve_sinkhorn
- greedy_matching

### Phase 4: Refactor Utilities
Split `lap_utils.h/cpp`:
- `lap_utils_pure.h/cpp` - Pure C++ (compute_total_cost, build_allowed, etc.)
- `lap_utils_rcpp.h/cpp` - Rcpp wrappers (make_result, transpose, type conversion)

### Phase 5: Create C++ Test Suite
```
cpp_tests/
├── CMakeLists.txt
├── tests/
│   ├── test_jv.cpp           # Tests lap::solve_jv directly
│   ├── test_hungarian.cpp
│   ├── test_auction.cpp
│   └── ...
└── include/                  # Symlinks or includes to src/core, src/solvers
```

## Execution Order

| Priority | Files | Effort | Rationale |
|----------|-------|--------|-----------|
| 1 | lap_types.h, lap_error.h | Small | Foundation for everything |
| 2 | solve_jv_pure.cpp | Medium | Template for other solvers |
| 3 | lap_utils_pure.cpp | Medium | Shared utilities |
| 4 | Remaining solvers | Large | Apply template |
| 5 | cpp_tests/ | Medium | Actually test the code |

## Risks and Mitigations

1. **Breaking changes during refactor**
   - Mitigation: Keep R tests passing at each step
   - Run `devtools::test()` after each solver

2. **Performance regression**
   - Mitigation: Benchmark before/after
   - Extra type conversions should be negligible

3. **Subtle behavior differences**
   - Mitigation: Exception handling matches LAP_ERROR behavior
   - Rcpp::stop() uses R's longjmp, exceptions use C++ stack unwinding

## Success Criteria

- [ ] All R tests still pass (88% coverage maintained)
- [ ] Can `#include "solvers/solve_jv_pure.h"` in standalone C++ test
- [ ] C++ tests run and pass via CMake/Catch2
- [ ] No `#include <Rcpp.h>` in `src/core/` or `src/solvers/`
