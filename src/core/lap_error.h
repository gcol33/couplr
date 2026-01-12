// src/core/lap_error.h
// Pure C++ error handling for LAP solvers - NO Rcpp dependencies
#pragma once

#include <stdexcept>
#include <string>

namespace lap {

// Custom exception for LAP solver errors
class LapException : public std::runtime_error {
public:
    explicit LapException(const std::string& msg)
        : std::runtime_error(msg) {}

    explicit LapException(const char* msg)
        : std::runtime_error(msg) {}
};

// Specific exception types for different error conditions
class InfeasibleException : public LapException {
public:
    explicit InfeasibleException(const std::string& msg = "No feasible solution exists")
        : LapException(msg) {}
};

class DimensionException : public LapException {
public:
    explicit DimensionException(const std::string& msg = "Invalid matrix dimensions")
        : LapException(msg) {}
};

class ConvergenceException : public LapException {
public:
    explicit ConvergenceException(const std::string& msg = "Algorithm did not converge")
        : LapException(msg) {}
};

}  // namespace lap

// Macro for throwing LAP exceptions in pure C++ code
// Use this instead of LAP_ERROR in solver implementations
#define LAP_THROW(msg) throw lap::LapException(msg)
#define LAP_THROW_INFEASIBLE(msg) throw lap::InfeasibleException(msg)
#define LAP_THROW_DIMENSION(msg) throw lap::DimensionException(msg)
#define LAP_THROW_CONVERGENCE(msg) throw lap::ConvergenceException(msg)
