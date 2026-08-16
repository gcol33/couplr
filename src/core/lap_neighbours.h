// src/core/lap_neighbours.h
// Walking row i's admissible columns, over the grid or over a named superset.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// The cost-source concept answers allowed(i, j) one pair at a time, so anything
// needing a row's neighbourhood scans all ncol columns for it. That is the right
// loop for a dense source and the wrong one for a graph holding a fraction of
// its pairs: a restricted master's admissibility graph carries a few candidates
// per row, and a matching over it should cost its edges rather than its grid.
//
// A source that knows where its pairs are says so by exposing
//
//     const int32_t* allowed_begin(int64_t i) const;
//     const int32_t* allowed_end(int64_t i) const;
//
// an ascending superset of row i's admissible columns: every column outside it
// is inadmissible, and every column inside it is still put to allowed(i, j). A
// superset rather than the exact set, because the object that knows where the
// pairs are is not usually the object that knows which of them the cost source
// forbids.
//
// Everything else keeps the grid scan, in the same ascending column order, so a
// loop rewritten in these terms walks a dense source exactly as it did before.
#pragma once

#include <cstdint>
#include <type_traits>
#include <utility>

namespace lap {

namespace detail {

template <class T, class = void>
struct has_allowed_range : std::false_type {};

template <class T>
struct has_allowed_range<
    T, std::void_t<
           decltype(std::declval<const T&>().allowed_begin(std::declval<int64_t>())),
           decltype(std::declval<const T&>().allowed_end(std::declval<int64_t>()))>>
    : std::true_type {};

}  // namespace detail

// Call `fn(j)` for each admissible column of row i, ascending. `fn` returns
// whether to carry on, so a caller that has found what it came for stops there
// rather than finishing the row.
template <class Source, class Fn>
inline void for_each_allowed(const Source& src, int64_t i, Fn&& fn) {
    if constexpr (detail::has_allowed_range<Source>::value) {
        const int32_t* const end = src.allowed_end(i);
        for (const int32_t* p = src.allowed_begin(i); p != end; ++p) {
            const int64_t j = static_cast<int64_t>(*p);
            if (!src.allowed(i, j)) continue;
            if (!fn(j)) return;
        }
    } else {
        for (int64_t j = 0; j < src.ncol; ++j) {
            if (!src.allowed(i, j)) continue;
            if (!fn(j)) return;
        }
    }
}

}  // namespace lap
