# CMake project file for ORION

cmake_minimum_required(VERSION 3.13)
#set(CMAKE_Fortran_COMPILER "gfortran")

project (ORION LANGUAGES Fortran)

set(CMAKE_VERBOSE_MAKEFILE FALSE)
# seach path for cmake modules
set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/")

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
include_directories(${CMAKE_SOURCE_DIR}/lib/64bit/TecIO)
file(GLOB TECIO "lib/64bit/TecIO/lib*")
if(UNIX AND NOT APPLE)
  add_compile_definitions(TECIO)
endif()

include(GNUInstallDirs)

# Set directories
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules")

add_subdirectory(src/lib)
add_subdirectory(src/test)
add_subdirectory(src/app)

