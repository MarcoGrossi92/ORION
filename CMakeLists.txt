# CMake project file for ORION

cmake_minimum_required(VERSION 3.13)
# if(UNIX AND APPLE)
#   set(CMAKE_Fortran_COMPILER "gfortran")
# endif()

project (ORION LANGUAGES Fortran)

set(CMAKE_VERBOSE_MAKEFILE FALSE)
# seach path for cmake modules
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake/")

# Set some options the user may choose
# Uncomment the below if you want the user to choose a parallelization library
#OPTION(USE_MPI "Use the MPI library for parallelization" OFF)
OPTION(USE_OPENMP "Use OpenMP for parallelization" OFF)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and TESTING. You should review this file and make sure the flags 
# are to your liking.
INCLUDE(${CMAKE_MODULE_PATH}/SetFortranFlags.cmake) 
# Locate and set parallelization libraries. There are some CMake peculiarities
# taken care of here, such as the fact that the FindOpenMP routine doesn't know
# about Fortran.
INCLUDE(${CMAKE_MODULE_PATH}/SetParallelizationLibrary.cmake)

# Include libraries
# TecIO
OPTION(USE_TECIO "Use TecIO" ON)
OPTION(TECIO_PREBUILD "Build TecIO independently" ON)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/lib/TecIO/teciosrc)
set(LINKLIBS -lpthread -lm -lstdc++)
if (TECIO_PREBUILD)
  file(GLOB TECIO "${CMAKE_CURRENT_SOURCE_DIR}/lib/TecIO/build/libtecio.a")
else()
  add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/lib/TecIO/teciosrc)
endif()

include(GNUInstallDirs)

# Set directories
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")

# Build library
add_subdirectory(src/fortran/lib)

# Build tests and applications (only if ORION build has not been called by another program)
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    message("-- Full build allowed")
    add_subdirectory(src/fortran/test)
    add_subdirectory(src/fortran/app)
endif()

