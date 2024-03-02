# Application build
set(APP vts2tec)
add_executable(${APP} "vts2tec.f90")
target_link_libraries(${APP} ORION)
if(UNIX AND NOT APPLE)
  target_link_libraries(${APP} ${TECIO})
endif()
target_include_directories(${APP}
    INTERFACE
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
)
set_target_properties(${APP} PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin/app/
)