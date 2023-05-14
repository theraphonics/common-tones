cmake_minimum_required(VERSION 3.0)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
# Add source files
include_directories(headers)

set(SRC_FILES
    src/xen
    src/io
    src/headers
    src/audio
    src/sound
    src/clm
    src/vct
    src/cmus
)

# Add library target
add_library(common-tones OBJECT ${SRC_FILES})
