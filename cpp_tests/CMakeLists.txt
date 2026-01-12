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
)

# Pure C++ sources from the package (NO Rcpp dependencies)
set(PURE_CPP_SOURCES
    ${SRC_DIR}/core/lap_utils_pure.cpp
    ${SRC_DIR}/solvers/solve_jv_pure.cpp
)

# Test sources
set(TEST_SOURCES
    tests/test_jv.cpp
)

# Main test executable
add_executable(couplr_tests
    ${TEST_SOURCES}
    ${PURE_CPP_SOURCES}
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
