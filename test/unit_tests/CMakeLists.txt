
execute_process(
  COMMAND ${PYTHON_CMD} make_test_suite.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

file(GLOB fortran_files
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     *.F90 )

#get_property(moduleDir TARGET swblib PROPERTY Fortran_MODULE_DIRECTORY)

add_executable(swbtest
              ${fortran_files})

target_link_libraries(swbtest swblib proj4)

# see https://stackoverflow.com/questions/43917443/fortran-module-files-not-found-by-cmake
target_include_directories(swbtest PUBLIC $<TARGET_PROPERTY:swblib,Fortran_MODULE_DIRECTORY>)

