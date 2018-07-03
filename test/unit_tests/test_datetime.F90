module test_datetime

  use fruit
  use datetime
  implicit none

contains

  subroutine test_datetime_basic_dateparse

    type (T_DATETIME) :: dt

    call dt%parseDate("03/04/2011")
    call assert_equals (3, dt%iMonth)
    call assert_equals (4, dt%iDay)
    call assert_equals (2011, dt%iYear)

  end subroutine test_datetime_basic_dateparse

end module test_datetime
