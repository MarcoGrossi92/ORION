file(GLOB LIBSRC *.f90)
add_library(ORION ${LIBSRC})
if (USE_TECIO)
  add_compile_definitions(TECIO)
  # The flavour is chosen in the top-level CMakeLists: "tecio" (serial) or
  # "teciompi" (MPI). Each exports its own namespaced target, so the link name
  # has to follow the flavour instead of being hard-coded to tecio::tecio.
  target_link_libraries(ORION PRIVATE ${basename}::${basename})
endif()
