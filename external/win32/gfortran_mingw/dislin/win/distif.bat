@if not "%1" == ""  goto M1:
@distif_w
@goto EXIT
:M1
@distif_x %1 %2 %3 %4 %5 %6 %7 %8 %9
:EXIT
