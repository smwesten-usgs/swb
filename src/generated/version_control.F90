module version_control

  implicit none

#ifndef VERSION_H
#define VERSION_H

#define QUOTE(str) "str"
#define EXPAND_AND_QUOTE(str) QUOTE(str)


#define PLATFORM_NAME EXPAND_AND_QUOTE( Darwin-18.7.0 )
#define GIT_BRANCH_VALUE EXPAND_AND_QUOTE( master )
#define GIT_COMMIT_HASH_VALUE EXPAND_AND_QUOTE( f47cbd0 )
#define GIT_COMMITS_ON_BRANCH_VALUE EXPAND_AND_QUOTE( 513 )

  character (len=20), parameter :: GIT_COMMIT_HASH_STRING = GIT_COMMIT_HASH_VALUE
  character (len=30), parameter :: GIT_BRANCH_STRING = GIT_BRANCH_VALUE

  character (len=*), parameter :: MAJOR_VERSION_NUMBER = "1"
  character (len=*), parameter :: MINOR_VERSION_NUMBER = "2"
  character (len=*), parameter :: BUILD_NUMBER = GIT_COMMITS_ON_BRANCH_VALUE

  character (len=*), parameter :: SWB_VERSION = trim(MAJOR_VERSION_NUMBER)//"."       &
                                                //trim(MINOR_VERSION_NUMBER)          &
                                                //", build "                          &
                                                //trim(adjustl(BUILD_NUMBER))

  character (len=*), parameter :: SYSTEM_NAME = adjustl(trim(PLATFORM_NAME))
  character (len=15), parameter :: COMPILE_DATE = trim(__DATE__)
  character (len=15), parameter :: COMPILE_TIME = trim(__TIME__)
  character (len=*), parameter :: COMPILATION_TIMESTAMP = trim(COMPILE_DATE)//"  "//trim(COMPILE_TIME)

#endif

end module version_control
