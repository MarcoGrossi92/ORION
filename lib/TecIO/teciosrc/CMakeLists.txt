cmake_minimum_required(VERSION 2.8.12) # requiring 2.8.12 sets the CMP0022 policy to new (non-transitive linking, i.e. just the interfaces) by default

if (NOT CMAKE_BUILD_TYPE)
    set (CMAKE_BUILD_TYPE Release CACHE STRING "CMake build type (Debug|Release)" FORCE)
endif ()

if(UNIX AND APPLE)
  set(CMAKE_CXX_COMPILER "/opt/homebrew/bin/g++-13")
  set(CMAKE_C_COMPILER "/opt/homebrew/bin/gcc-13")
endif()

project (tecio C CXX Fortran)

FIND_PACKAGE(Boost)
IF (Boost_FOUND)
    INCLUDE_DIRECTORIES(${Boost_INCLUDE_DIR})
ELSE()
    # Check if the "boost" directory doesn't exist, and if so, extract boost.tar.gz
    ADD_CUSTOM_TARGET(ExtractBoost
        COMMAND ${CMAKE_COMMAND} -E chdir ${CMAKE_CURRENT_SOURCE_DIR}/.. tar xzf ${CMAKE_CURRENT_SOURCE_DIR}/../boost.tar.gz
        COMMENT "Extracting Boost library"
        VERBATIM)
    ADD_CUSTOM_TARGET(CreateBoostLink ALL
        COMMAND ${CMAKE_COMMAND} -E create_symlink ${CMAKE_CURRENT_SOURCE_DIR}/../boost ${CMAKE_CURRENT_SOURCE_DIR}/boost
        COMMENT "Creating symbolic link to Boost directory"
        VERBATIM)
    # Add dependencies between the targets
    add_dependencies(CreateBoostLink ExtractBoost)

    Set (Boost_INCLUDE_DIR "boost")
    message ("Warning:  Cannot find boost include directories.  Hardcoding to ${Boost_INCLUDE_DIR}")
    INCLUDE_DIRECTORIES(SYSTEM ${Boost_INCLUDE_DIR})
ENDIF()
include_directories(.)

IF (WIN32)
    set(BaseFlags "/EHsc /MP /wd\"4996\" /D MSWIN /D TP_PROJECT_USES_BOOST /D BOOST_ALL_NO_LIB /D MAKEARCHIVE /D NO_THIRD_PARTY_LIBS /D NO_ASSERTS")
ELSE ()
	set(BaseFlags "-DLINUX -DLINUX64 -DTP_PROJECT_USES_BOOST -DBOOST_ALL_NO_LIB -DMAKEARCHIVE -DNO_THIRD_PARTY_LIBS -DNO_ASSERTS -fmessage-length=0 -fPIC -fvisibility=hidden -w")
ENDIF ()
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${BaseFlags}")
                    
FILE(GLOB mainFiles "*.cpp" "*.h")
LIST(REMOVE_ITEM mainFiles "${CMAKE_CURRENT_SOURCE_DIR}/szcombine.cpp")

add_library(tecio STATIC ${mainFiles})
add_dependencies(tecio CreateBoostLink)

add_executable(szcombine "szcombine.cpp")
if (WIN32)
    target_link_libraries(szcombine tecio)
else ()
    target_link_libraries(szcombine tecio pthread)
endif ()

set_target_properties(tecio PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(szcombine PROPERTIES LINKER_LANGUAGE Fortran)
