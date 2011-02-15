@if not "%1" == ""  goto START
@ECHO    /****************************************************************/
@ECHO    /**                       D L I N K                            **/
@ECHO    /**                                                            **/
@ECHO    /** DLINK links programs using the DISLIN Fortran library.     **/
@ECHO    /**                                                            **/
@ECHO    /** Command:  DLINK  [option]   [-r8]   main                   **/
@ECHO    /**                                                            **/
@ECHO    /** option    is an optional qualifier that may have one of    **/
@ECHO    /**           the following values:                            **/
@ECHO    /**      -c   to compile programs before linking               **/
@ECHO    /**      -r   to run programs after linking                    **/
@ECHO    /**      -a   to compile, link and run programs.               **/
@ECHO    /**                                                            **/
@ECHO    /** -r8       is an optional parameter for using the double    **/
@ECHO    /**           precision library of DISLIN.                     **/
@ECHO    /**                                                            **/
@ECHO    /** main      is the name of the main program.                 **/
@ECHO    /**                                                            **/
@ECHO    /** Example:  DLINK  -a   TEST                                 **/
@ECHO    /** Version:  GFortran (f77)                                   **/
@ECHO    /****************************************************************/
@goto EXIT
:START
@set _dislin=%DISLIN%
@if "%DISLIN%" == "" set _dislin=c:\dislin
@set _opt1=%1
@if %1 ==  -c	shift
@if %1 ==  -a	shift
@if %1 ==  -r	shift
@set _opt2=%1
@if %1 ==  -r8	shift
@rem
@if %_opt1% ==  -c    goto COMP
@if %_opt1% ==  -a    goto COMP
@goto  LINK
:COMP
gfortran -c  %1.for
@if errorlevel 1 goto ENDE
:LINK
@set _lib=disgf.a
@if %_opt2% == -r8 set _lib=disgf_d.a
gfortran -o %1.exe %1.o %2 %3 %4 %5 %6  %_dislin%\%_lib% -luser32 -lgdi32 -lopengl32
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
:EXIT
