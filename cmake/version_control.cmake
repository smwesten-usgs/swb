# get the current git branch name
execute_process(
  COMMAND git rev-parse --abbrev-ref HEAD
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the latest abbreviated commit hash of the working branch
execute_process(
  COMMAND git log -1 --format=%h
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMIT_HASH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

# Get the total number of commits on this branch
execute_process(
	COMMAND git rev-list ${SWB_LATEST_VERSION_TAG}.. --count 
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
  OUTPUT_VARIABLE GIT_COMMITS_ON_BRANCH
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

add_definitions("-DGIT_COMMIT_HASH=${GIT_COMMIT_HASH}")
add_definitions("-DGIT_BRANCH=${GIT_BRANCH}")
add_definitions("-DGIT_COMMITS_ON_BRANCH=${GIT_COMMITS_ON_BRANCH}")

# Copy a file to another location and modify its contents.
# information in the *.tpl file is populated with CMake
# variable values, then copied to the 'generated' subdir
configure_file(
  ${CMAKE_SOURCE_DIR}/src/version_control.tpl
  ${CMAKE_SOURCE_DIR}/src/generated/version_control.F90
)

include_directories(${CMAKE_SOURCE_DIR}/src/generated)
