// cpp_tests/tests/test_calipers.h
// Calipers on one coordinate column, for tests whose coordinates are the raw
// variables. lap::CaliperSpec holds the variable's values per unit; these
// read them out of the row-major coordinate blocks a LazyCostMatrix takes.
#pragma once

#include "core/lap_lazy_types.h"

#include <cstddef>
#include <cstdint>
#include <vector>

struct ColumnCaliper {
    int64_t var;
    double threshold;
};

inline std::vector<double> coordinate_column(const std::vector<double>& rowmajor,
                                             int64_t n_vars, int64_t var) {
    const std::size_t stride = static_cast<std::size_t>(n_vars);
    std::vector<double> out;
    out.reserve(rowmajor.size() / stride);
    for (std::size_t k = static_cast<std::size_t>(var); k < rowmajor.size(); k += stride) {
        out.push_back(rowmajor[k]);
    }
    return out;
}

inline std::vector<lap::CaliperSpec> column_calipers(
    const std::vector<ColumnCaliper>& calipers, const std::vector<double>& left_rowmajor,
    const std::vector<double>& right_rowmajor, int64_t n_vars) {
    std::vector<lap::CaliperSpec> out;
    out.reserve(calipers.size());
    for (const ColumnCaliper& cal : calipers) {
        out.push_back(lap::CaliperSpec{cal.threshold,
                                       coordinate_column(left_rowmajor, n_vars, cal.var),
                                       coordinate_column(right_rowmajor, n_vars, cal.var)});
    }
    return out;
}
