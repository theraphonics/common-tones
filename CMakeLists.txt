cmake_minimum_required(VERSION 3.0)

project(MyProject)

# Add source files
set(SRC_FILES
    src/xen.c
    src/io.c
    src/headers.c
    src/audio.c
    src/sound.c
    src/clm.c
    src/vct.c
    src/cmus.c
)

# Add library target
add_library(mylib OBJECT ${SRC_FILES})
