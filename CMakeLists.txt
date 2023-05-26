cmake_minimum_required(VERSION 3.0)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Add source files
include_directories(headers)

set(SRC_FILES
    src/clm
    src/cmus
)

# Add library target
add_library(common-tones OBJECT ${SRC_FILES})