@if not "%1" == ""  goto M1:
@dismov_w
@goto EXIT
:M1
@dismov_x %1 %2 %3 %4 %5 %6 %7 %8 %9
:EXIT
