# Application build
set(APP converter)
add_executable(${APP} "conversion.f90")
target_link_libraries(${APP} ORION {LINKLIBS})
target_include_directories(${APP}
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
)
set_target_properties(${APP} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin/app/
)
