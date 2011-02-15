@if not "%1" == ""  goto START
@ECHO    /****************************************************************/
@ECHO    /**                       C L I N K                            **/
@ECHO    /**                                                            **/
@ECHO    /** CLINK links programs using the DISLIN C-library.           **/
@ECHO    /**                                                            **/
@ECHO    /** Command:  CLINK  [option] [-r8]   main                     **/
@ECHO    /**                                                            **/
@ECHO    /** option    is an optional qualifier that may have one of    **/
@ECHO    /**           the following values:                            **/
@ECHO    /**      -c   to compile programs before linking               **/
@ECHO    /**      -cpp to compile C++ programs before linking           **/
@ECHO    /**      -r   to run programs after linking                    **/
@ECHO    /**      -a   to compile, link and run programs.               **/
@ECHO    /**                                                            **/
@ECHO    /** -r8       is an optional parameter for using the double    **/
@ECHO    /**           precision library of DISLIN.                     **/
@ECHO    /**                                                            **/
@ECHO    /** main      is the name of the main program.                 **/
@ECHO    /**                                                            **/
@ECHO    /** Example:  CLINK  -a   TEST                                 **/
@ECHO    /** Version:  gcc (GFortran)                                   **/
@ECHO    /****************************************************************/
@goto EXIT
:START
@set _dislin=%DISLIN%
@if "%DISLIN%" == "" set _dislin=c:\dislin
@set _opt1=%1
@if %1 ==  -c	shift
@if %1 ==  -cpp	shift
@if %1 ==  -a	shift
@if %1 ==  -r	shift
@set _opt2=%1
@if %1 ==  -r8	shift
@rem
@set _cc=gcc
@if %_opt1% ==  -cpp  set _cc=g++
@if %_opt1% ==  -c    goto COMP
@if %_opt1% ==  -cpp  goto COMP
@if %_opt1% ==  -a    goto COMP
@goto  LINK
:COMP
@set _ext=c
@set _inc=%_dislin%
@if %_opt1% ==  -cpp  set _ext=cpp
@if %_opt2% == -r8 set _inc=%_dislin%\real64
%_cc% -c -I%_inc%  %1.%_ext%
@set _ext=
@set _inc=
@if errorlevel 1 goto ENDE
:LINK
@set _lib=disgf.a
@if %_opt2% == -r8      set _lib=disgf_d.a
%_cc% -o %1.exe %1.o %2 %3 %4 %5 %6  %_dislin%\%_lib% -luser32 -lgdi32 -lopengl32
@set _lib=
@if %_opt1% ==  -a  goto RUN
@if %_opt1% ==  -r  goto RUN
@goto ENDE
:RUN
@if errorlevel 1 goto ENDE
%1
:ENDE
@set _dislin=
@set _opt1=
@set _opt2=
@set _cc=
:EXIT
