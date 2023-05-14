cmake_minimum_required(VERSION 3.0)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
# Add source files
set(SRC_FILES
    xen
    io
    headers
    audio
    sound
    clm
    vct
    cmus
)

# Add library target
add_library(common-tones OBJECT ${SRC_FILES})
