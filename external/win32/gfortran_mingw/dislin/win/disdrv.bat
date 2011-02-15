@if not "%1" == ""  goto M1
@disdrv_w
@goto EXIT
:M1
@if "%2" == ""  goto M2
@if %2 == PRT1  goto M3
@if %2 == prt1  goto M3
@if %2 == HPJ1  goto M4
@if %2 == hpj1  goto M4
@if %2 == HPJ2  goto M4
@if %2 == hpj2  goto M4
:M2
@disdrv_x %1 %2 %3 %4 %5 %6 %7 %8 %9
@goto EXIT
:M3
@disgdi_x %1 %2 %3 %4 %5 %6 %7 %8 %9
@goto EXIT
:M4
@dishpj_x %1 %2 %3 %4 %5 %6 %7 %8 %9
@goto EXIT
:EXIT
