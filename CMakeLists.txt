PROJECT(Fortran) 
enable_language (Fortran) 
 
# Check version of cmake 
cmake_minimum_required(VERSION 2.8)

SET(CMAKE_VERBOSE_MAKEFILE OFF) 
SET(CMAKE_COLOR_MAKEFILE ON) 

 
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin) 
SET(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib) 
SET(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_SOURCE_DIR}/includes) 
 
# Type of build  
message( STATUS "Build type: " ${CMAKE_BUILD_TYPE}) 

# Compiler options
message( STATUS "Fortran compiler: " ${CMAKE_Fortran_COMPILER} )
#Compiler gfortran Flags
#set(CMAKE_Fortran_FLAGS "-fbounds-check -llapack ")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -march=native -ffast-math -funroll-loops -std=legacy -fopenmp")
set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -Warray-bounds  -std=legacy -Wall -Wno-unused ")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ")
#set(DOUBLE "-r8")

#to change
set(LAPACKLIB "/usr/lib/lapack/liblapack.so.3.6.0")

if (${CMAKE_BUILD_TYPE} MATCHES "Release")
  message( STATUS "Fortran compiler flags:" ${CMAKE_Fortran_FLAGS_RELEASE})
elseif (${CMAKE_BUILD_TYPE} MATCHES "Debug")
  message( STATUS "Fortran compiler flags:" ${CMAKE_Fortran_FLAGS_DEBUG})
endif()


# Libraries

#SET (TOOLS tools)
#ADD_LIBRARY(${TOOLS} STATIC src/tools.f90)

SET(PRECISION precision)
ADD_LIBRARY(${PRECISION} STATIC src/tools/precision.f90)

SET(CLASSLINEARSOLVER classlinearsolver)
ADD_LIBRARY(${CLASSLINEARSOLVER} STATIC src/class_linear_solver/class_linear_solver.f90)
target_link_libraries(${CLASSLINEARSOLVER} ${PRECISION} ${LAPACKLIB})

SET(MODINTEGRATION modintegration)
ADD_LIBRARY(${MODINTEGRATION} STATIC src/class_integration/class_integration_module.f90)
target_link_libraries(${MODINTEGRATION} ${PRECISION})

SET(New_CLASS_DERIV Derivativeclass)
ADD_LIBRARY(${New_CLASS_DERIV} STATIC Mywork/New_class_deriv.f90)
target_link_libraries(${New_CLASS_DERIV} ${PRECISION})
#SET (CLASS_FIELD class_field)

#SET (CLASS_FIELD class_field)
#ADD_LIBRARY(${CLASS_FIELD} STATIC src/class_field/class_field.f90)
#target_link_libraries(${CLASS_FIELD} ${CLASS_IO} ${PRECISION})

#SET (CLASS_IO class_io)
#ADD_LIBRARY(${CLASS_IO} STATIC src/class_io/class_io.f90)

# Executables

# main program
#SET(MAIN main)
#ADD_EXECUTABLE( ${MAIN} src/main.f90)
#target_link_libraries(${MAIN} ${TOOLS})
# AHMED Program
SET(NEW_CLASS_DERIV derivative_class)
ADD_EXECUTABLE( ${DERIVATIVE_TEST} Mywork/Drivative_test.f90)
target_link_libraries(${DERIVATIVE_TEST} ${NEW_DERIVATIVE_CLASS} ${PRECISION})
#SET(GENERALTEST generalTest)

# Fannie program
SET(TESTLSSOLVER testlssolver)
ADD_EXECUTABLE( ${TESTLSSOLVER} src/class_linear_solver/testlssolver.f90)
target_link_libraries(${TESTLSSOLVER} ${CLASSLINEARSOLVER} ${PRECISION})

# Alex program
SET(INTEGRATION integration)
ADD_EXECUTABLE( ${INTEGRATION} src/class_integration/class_integration_test.f90)
target_link_libraries(${INTEGRATION} ${MODINTEGRATION} ${PRECISION})

#SET(GENERALTEST generalTest)
#ADD_EXECUTABLE( ${GENERALTEST} src/generalTest.f90)
#target_link_libraries(${MAIN} ${CLASS_FIELD})
