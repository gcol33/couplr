cmake_minimum_required(VERSION 3.14)
project(couplr_cpp_tests CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Options
option(ENABLE_COVERAGE "Enable code coverage" OFF)

# Coverage configuration
if(ENABLE_COVERAGE)
    message(STATUS "Code coverage enabled")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O0 -g --coverage -fprofile-arcs -ftest-coverage")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} --coverage")
endif()

# Fetch Catch2 v3
include(FetchContent)
FetchContent_Declare(
    Catch2
    GIT_REPOSITORY https://github.com/catchorg/Catch2.git
    GIT_TAG v3.5.2
)
FetchContent_MakeAvailable(Catch2)
list(APPEND CMAKE_MODULE_PATH ${catch2_SOURCE_DIR}/extras)

# Source directory (parent src/)
set(SRC_DIR "${CMAKE_CURRENT_SOURCE_DIR}/../src")

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${SRC_DIR}
    ${SRC_DIR}/core
    ${SRC_DIR}/solvers
    ${SRC_DIR}/solvers/network_simplex
    ${SRC_DIR}/solvers/orlin_ahuja
)

# Pure C++ implementation sources (NO Rcpp dependencies)
# These are the main algorithm implementations (foo.cpp)
# Rcpp wrappers are in foo_rcpp.cpp files (not needed for C++ tests)
set(CPP_IMPL_SOURCES
    # Core utilities
    ${SRC_DIR}/core/lap_utils.cpp

    # Solvers
    ${SRC_DIR}/solvers/solve_jv.cpp
    ${SRC_DIR}/solvers/solve_hungarian.cpp
    ${SRC_DIR}/solvers/solve_auction.cpp
    ${SRC_DIR}/solvers/solve_ssp.cpp
    ${SRC_DIR}/solvers/solve_bruteforce.cpp
    ${SRC_DIR}/solvers/solve_csa.cpp
    ${SRC_DIR}/solvers/solve_csflow.cpp
    ${SRC_DIR}/solvers/solve_bottleneck.cpp
    ${SRC_DIR}/solvers/greedy_matching.cpp
    ${SRC_DIR}/solvers/solve_hk01.cpp
    ${SRC_DIR}/solvers/solve_lapmod.cpp
    ${SRC_DIR}/solvers/solve_line_metric.cpp
    ${SRC_DIR}/solvers/solve_push_relabel.cpp
    ${SRC_DIR}/solvers/solve_ramshaw_tarjan.cpp
    ${SRC_DIR}/solvers/solve_sinkhorn.cpp
    ${SRC_DIR}/solvers/solve_ssap_bucket.cpp
    ${SRC_DIR}/solvers/solve_cycle_cancel.cpp
    ${SRC_DIR}/solvers/solve_jv_duals.cpp

    # Subdirectory solvers
    ${SRC_DIR}/solvers/network_simplex/solve_network_simplex.cpp
    ${SRC_DIR}/solvers/orlin_ahuja/orlin_solve.cpp
)

# Test sources
set(TEST_SOURCES
    tests/test_jv.cpp
    tests/test_hungarian.cpp
    tests/test_auction.cpp
    tests/test_greedy.cpp
    tests/test_bottleneck.cpp
    tests/test_ssp.cpp
    tests/test_bruteforce.cpp
    tests/test_csa.cpp
    tests/test_csflow.cpp
    tests/test_cycle_cancel.cpp
    tests/test_hk01.cpp
    tests/test_lapmod.cpp
    tests/test_network_simplex.cpp
    tests/test_orlin.cpp
    tests/test_push_relabel.cpp
    tests/test_ramshaw_tarjan.cpp
    tests/test_sinkhorn.cpp
    tests/test_ssap_bucket.cpp
)

# Main test executable
add_executable(couplr_tests
    ${TEST_SOURCES}
    ${CPP_IMPL_SOURCES}
)

target_link_libraries(couplr_tests PRIVATE Catch2::Catch2WithMain)

# Register tests with CTest
include(CTest)
include(Catch)
catch_discover_tests(couplr_tests)

# Coverage target
if(ENABLE_COVERAGE)
    find_program(LCOV lcov)
    find_program(GENHTML genhtml)

    if(LCOV AND GENHTML)
        add_custom_target(coverage
            COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/coverage
            COMMAND couplr_tests
            COMMAND ${LCOV} --capture --directory . --output-file coverage.info --ignore-errors mismatch
            COMMAND ${LCOV} --remove coverage.info '/usr/*' '*/Catch2/*' '*/catch2/*' '*/_deps/*' --output-file coverage.info
            COMMAND ${GENHTML} coverage.info --output-directory ${CMAKE_BINARY_DIR}/coverage
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
            DEPENDS couplr_tests
            COMMENT "Generating coverage report"
        )
    endif()
endif()

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Coverage: ${ENABLE_COVERAGE}")
message(STATUS "Source dir: ${SRC_DIR}")
