module test_datetime

  use fruit
  use datetime
  implicit none

contains

  subroutine test_datetime_basic_dateparse
  ! datetime: parse with default mm/dd/yyyy date format
    type (T_DATETIME) :: dt

    call dt%parseDate("03/15/2011")
    call assert_equals (3, dt%iMonth)
    call assert_equals (15, dt%iDay)
    call assert_equals (2011, dt%iYear)

  end subroutine test_datetime_basic_dateparse

!-------------------------------------------------------------------------------

  subroutine test_datetime_basic_mangled_dateparse
  ! datetime: parse with default mm/dd/yyyy date format, missing '0' values in month and day
    type (T_DATETIME) :: dt

    call dt%parseDate("3/2/2011")
    call assert_equals (3, dt%iMonth)
    call assert_equals (2, dt%iDay)
    call assert_equals (2011, dt%iYear)

  end subroutine test_datetime_basic_mangled_dateparse

!-------------------------------------------------------------------------------

  subroutine test_datetime_custom_dateparse
  ! datetime: parse with custom yyyy-mm-dd date format

    type (T_DATETIME) :: dt

    call dt%setDateFormat("YYYY-MM-DD")
    call dt%parseDate("1776-07-4")
    call assert_equals (7, dt%iMonth)
    call assert_equals (4, dt%iDay)
    call assert_equals (1776, dt%iYear)

  end subroutine test_datetime_custom_dateparse

!-------------------------------------------------------------------------------

  subroutine test_datetime_addition
  ! datetime: add 5 to Julian day and return the correct Gregorian date
    type (T_DATETIME)    :: dt
    integer              :: indx

    call dt%calcJulianDay(iMonth=2, iDay=29, iYear=2000)

     do indx=1,5
      call dt%addDay()
    enddo

    call assert_equals (3, dt%iMonth)
    call assert_equals (5, dt%iDay)
    call assert_equals (2000, dt%iYear)

  end subroutine test_datetime_addition

end module test_datetime
