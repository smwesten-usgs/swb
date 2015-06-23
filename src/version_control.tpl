module version_control
  
  use types 
  implicit none
  
#ifndef VERSION_H
#define VERSION_H

#define QUOTE(str) "str"
#define EXPAND_AND_QUOTE(str) QUOTE(str)

#define GIT_BRANCH_VALUE EXPAND_AND_QUOTE( @GIT_BRANCH@ )
#define GIT_COMMIT_HASH_VALUE EXPAND_AND_QUOTE( @GIT_COMMIT_HASH@ )

  character (len=20), parameter :: GIT_COMMIT_HASH_STRING = GIT_COMMIT_HASH_VALUE
  character (len=30), parameter :: GIT_BRANCH_STRING = GIT_BRANCH_VALUE

  character(len=45), public, parameter :: &
      SWB_VERSION = "1.2 BETA (geographic transformations enabled)"
  character (len=15) :: COMPILE_DATE = trim(__DATE__)
  character (len=15) :: COMPILE_TIME = trim(__TIME__)
  
#endif

end module version_control