file(GLOB LIBSRC *.f90)
add_library(ORION ${LIBSRC})
target_link_libraries(ORION tecio)
