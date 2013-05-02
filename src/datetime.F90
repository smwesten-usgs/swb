module datetime

  use types

  implicit none
  private

  type T_DATETIME

    integer (kind=T_INT) :: iMonth = 1
    integer (kind=T_INT) :: iDay = 1
    integer (kind=T_INT) :: iYear = 1
    integer (kind=T_INT) :: iHour = 0
    integer (kind=T_INT) :: iMinute = 0
    integer (kind=T_INT) :: iSecond = 0
    integer (kind=T_INT) :: iWaterYearHigh
    integer (kind=T_INT) :: iWaterYearLow
    logical (kind=T_LOGICAL) :: lIsLeapYear = lFALSE
    integer (kind=T_INT) :: iJulianDay
    real (kind=T_SGL) :: rFractionOfDay = rZERO

  contains

    procedure, public  :: calcJulianDay => calc_julian_day_sub
!    procedure, public :: populateJulianDay => populate_julian_day_sub
!    generic, public :: calcJulianDay => calc_julian_day_sub, &
!                                           populate_julian_day_sub

    procedure, public :: dayspermonth => get_days_in_month_fn
    procedure, public :: daysperyear => get_days_in_year_fn

    procedure :: calcGregorianDate => calc_gregorian_date_sub
    procedure, public :: calcWaterYear => calc_water_year_sub
    procedure, public :: parseDate => parse_text_to_date_sub
    procedure, public :: parseTime => parse_text_to_time_sub
    procedure, public :: isLeapYear => is_leap_year

    procedure, public :: setTimeFormat => set_time_format_indices
    procedure, public :: setDateFormat => set_Date_format_indices

    ! define operators that will work with datetime objects
    procedure, public :: is_date_greater_than
    generic :: operator( > ) => is_date_greater_than

    procedure, public :: is_date_less_than
    generic :: operator( < ) => is_date_less_than

    procedure, public :: is_date_GT_or_equal_to
    generic :: operator( >= ) => is_date_GT_or_equal_to

    procedure :: is_date_LT_or_equal_to
    generic, public :: operator( <= ) => is_date_LT_or_equal_to

    procedure, public :: is_date_equal_to
    generic, public :: operator( == ) => is_date_equal_to

    procedure, public :: date_minus_date_fn
    generic, public :: operator( - ) => date_minus_date_fn

    procedure, public :: addDay => date_plus_day_sub
    procedure, public :: subtractDay => date_minus_day_sub

    procedure, public :: prettydate => write_pretty_date_fn
    procedure, public :: listdatetime => write_list_datetime_fn
    procedure, public :: listdate => write_list_date_fn
    procedure, public :: listtime => write_list_time_fn
    procedure, public :: systime => system_time_to_date_sub
    procedure, public :: getJulianDay => get_julian_day_float_fn

  end type T_DATETIME

  type T_DATERANGE

    type (T_DATETIME) :: tStartDate
    type (T_DATETIME) :: tEndDate

  contains

    procedure, public :: new_daterange_fm_text_sub, new_daterange_fm_datetime_sub
    generic :: new => new_daterange_fm_text_sub, new_daterange_fm_datetime_sub
    procedure, public :: newFmText => new_daterange_fm_text_sub
    procedure, public :: newFmDT => new_daterange_fm_datetime_sub

  end type T_DATERANGE

  type, extends (T_DATETIME) :: T_MODEL_SIM
    type (T_DATETIME) :: tStartDate
    type (T_DATETIME) :: tEndDate

  end type T_MODEL_SIM


  ! the following values are determined by the date format string; defaults to MM/DD/YYYY
  character (len=14), private :: sDATE_FORMAT = "MM/DD/YYYY"
  character (len=14), public  :: sDEFAULT_DATE_FORMAT = "MM/DD/YYYY"
  integer (kind=T_INT), private :: iScanMM1 = 1
  integer (kind=T_INT), private :: iScanMM2 = 2
  integer (kind=T_INT), private :: iScanDelim1 = 3
  integer (kind=T_INT), private :: iScanDD1 = 4
  integer (kind=T_INT), private :: iScanDD2 = 5
  integer (kind=T_INT), private :: iScanDelim2 = 6
  integer (kind=T_INT), private :: iScanYYYY1 = 7
  integer (kind=T_INT), private :: iScanYYYY2 = 10

  character (len=14), private :: sTIME_FORMAT = "HH:MM:SS"
  character (len=14), public  :: sDEFAULT_TIME_FORMAT = "HH:MM:SS"
  integer (kind=T_INT), private :: iScanHour1 = 1
  integer (kind=T_INT), private :: iScanHour2 = 2
  integer (kind=T_INT), private :: iScanMin1 = 4
  integer (kind=T_INT), private :: iScanMin2 = 5
  integer (kind=T_INT), private :: iScanSec1 = 7
  integer (kind=T_INT), private :: iScanSec2 = 8

  type (T_MODEL_SIM), public :: MODEL_SIM

contains

!------------------------------------------------------------------------------

subroutine set_default_date_format(sDateFormat)

  character (len=*), intent(in) :: sDateFormat

  sDEFAULT_DATE_FORMAT = sDateFormat

end subroutine set_default_date_format

!------------------------------------------------------------------------------

subroutine set_default_time_format(sTimeFormat)

  character (len=*), intent(in) :: sTimeFormat

  sDEFAULT_TIME_FORMAT = sTimeFormat

end subroutine set_default_time_format

!------------------------------------------------------------------------------

subroutine set_date_format_indices(this, sDateFormat)

  class (T_DATETIME), intent(inout) :: this
  character (len=*), intent(in), optional :: sDateFormat

  ! [ LOCALS ]
  character (len=14) :: sDateFmt
  character (len=6), parameter :: DELIMITERS = "/-_\. "

  if(present(sDateFormat) ) then
    sDateFmt = sDateFormat
    sDATE_FORMAT = sDateFormat
  else
    sDateFmt = sDEFAULT_DATE_FORMAT
    sDATE_FORMAT = sDEFAULT_DATE_FORMAT
  endif

  iScanMM1 = scan(string=sDateFmt,set="M")
  iScanMM2 = scan(string=sDateFmt,set="M", back=lTRUE )
  iScanDD1 = scan(string=sDateFmt,set="D")
  iScanDD2 = scan(string=sDateFmt,set="D", back=lTRUE )
  iScanYYYY1 = scan(string=sDateFmt,set="Y")
  iScanYYYY2 = scan(string=sDateFmt,set="Y", back=lTRUE )
  iScanDelim1 = scan(string=trim(sDateFmt), set=DELIMITERS)
  iScanDelim2 = scan(string=trim(sDateFmt), set=DELIMITERS, back=lTRUE)

  call Assert(iScanMM1 > 0 .and. iScanMM2 > 0 &
    .and. iScanDD1 > 0 .and. iScanDD2 > 0 &
    .and. iScanYYYY1 > 0 .and. iScanYYYY2 > 0, &
    "Failed to properly parse the date format string "//dquote(sDateFormat), &
    trim(__FILE__), __LINE__)
    ! perhaps there are no delimiters? if not, these values CAN be zero
!  call assert(iScanDelim1 > 0 .and. iScanDelim2 > 0, &
!    "Failed to properly parse the delimiters in the date format string "//dquote(sDateFormat), &
!    trim(__FILE__), __LINE__)

end subroutine set_date_format_indices

!------------------------------------------------------------------------------

subroutine set_time_format_indices(this, sTimeFormat)

  class (T_DATETIME), intent(inout) :: this
  character (len=*), intent(in), optional :: sTimeFormat

  ! [ LOCALS ]
  character (len=14) :: sTimeFmt

  if(present(sTimeFormat) ) then
    sTimeFmt = sTimeFormat
    sTIME_FORMAT = sTimeFormat
  else
    sTimeFmt = sTIME_FORMAT     ! if no arg supplied, default to module variable
  endif

  iScanHour1 = scan(string=sTimeFmt,set="H")
  iScanHour2 = scan(string=sTimeFmt,set="H", back=lTRUE )
  iScanMin1 = scan(string=sTimeFmt,set="M")
  iScanMin2 = scan(string=sTimeFmt,set="M", back=lTRUE )
  iScanSec1 = scan(string=sTimeFmt,set="S")
  iScanSec2 = scan(string=sTimeFmt,set="S", back=lTRUE )

  call Assert(iScanHour1 > 0 .and. iScanHour2 > 0 &
        .and. iScanMin1 > 0 .and. iScanMin2 > 0 &
        .and. iScanSec1 > 0 .and. iScanSec2 > 0, &
        "Failed to properly parse the time format string "//dquote(sTimeFormat), &
        trim(__FILE__), __LINE__)

end subroutine set_time_format_indices

!------------------------------------------------------------------------------

subroutine parse_text_to_date_sub(this, sString)

  class (T_DATETIME), intent(inout) :: this
  character (len=*), intent(in) :: sString

  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iMonth
  integer (kind=T_INT) :: iDay
  integer (kind=T_INT) :: iYear
  integer (kind=T_INT) :: iMonthOffset, iDayOffset
  character (len=256) :: sStr
  character (len=256) :: sMonth, sDay, sYear, sBuf

  ! these offset amounts have value of 1 if the program detects a single-digit date value
  iMonthOffset = 0; iDayOffset = 0

  sStr = trim(adjustl(sString))

  sMonth = sStr(iScanMM1 : iScanMM2 )
  sBuf = clean(sMonth)
  if(len_trim(sBuf) /= len_trim(sMonth)) then   ! we have a case where there is no leading zero
    iMonthOffset = 1
    sMonth = trim(sBuf)
  endif
  read(sMonth,fmt=*, iostat = iStat) iMonth
  call Assert(iStat==0 .and. (iMonth > 0 .and. iMonth <= 12), &
    "Error parsing month value from text file - got "//trim(sMonth)//";"// &
    " date text: "//trim(sStr), TRIM(__FILE__),__LINE__)

  sDay = sStr( iScanDD1 - iMonthOffset : iScanDD2 -iMonthOffset )
  sBuf = clean(sDay)
  if(len_trim(sBuf) /= len_trim(sDay)) then   ! we have a case where there is no leading zero
    iDayOffset = 1
    sDay = trim(sBuf)
  endif
  read(sDay, fmt=*, iostat = iStat) iDay
  call Assert(iStat==0 .and. (iDay > 0 .and. iDay <= 31), &
    "Error parsing day value from text file - got "//trim(sDay)//";"// &
    " date text: "//trim(sStr), TRIM(__FILE__),__LINE__)

  sYear = sStr( iScanYYYY1 - iMonthOffset - iDayOffset: iScanYYYY2 - iMonthOffset - iDayOffset)
  read(sYear,fmt=*, iostat = iStat) iYear
  call Assert(iStat==0, "Error parsing year value from text file - got "//trim(sYear)//";"// &
    " date text: "//trim(sStr), TRIM(__FILE__),__LINE__)

!  if(iYear <= 99 ) iYear = iYear + 1900    ! this might be a lethal assumption

  this%iMonth = iMonth
  this%iYear = iYear
  this%iDay = iDay

end subroutine parse_text_to_date_sub

!------------------------------------------------------------------------------

subroutine parse_text_to_time_sub(this, sString)

  class (T_DATETIME), intent(inout) :: this
  character (len=*), intent(in) :: sString


  ! [ LOCALS ]
  integer (kind=T_INT) :: iStat
  integer (kind=T_INT) :: iHour
  integer (kind=T_INT) :: iMinute
  integer (kind=T_INT) :: iSecond
  integer (kind=T_INT) :: iOffset

  character (len=256) :: sHour, sMinute, sSecond


  character (len=256) :: sTimeFmt
  character (len=256) :: sStr
  character (len=256) :: sBuf

  iOffset = 0

  sStr = trim(adjustl(sString))

  sHour =   sStr( iScanHour1 : iScanHour2 )

  sBuf = clean(sHour)
  if(len_trim(sBuf) /= len_trim(sHour)) then   ! we have a case where there is no leading zero
    iOffset = 1
    sHour = trim(sBuf)
  endif
  read(sHour,fmt=*, iostat = iStat) iHour
  call Assert(iStat==0, "Error parsing hour value from text file - got "//trim(sHour)//";"// &
    " time text: "//trim(sStr), TRIM(__FILE__),__LINE__)

  sMinute = sStr(iScanMin1 - iOffset : iScanMin2 - iOffset )
  read(sMinute,fmt=*, iostat = iStat) iMinute
  call Assert(iStat==0, "Error parsing minutes value from text file - got "//trim(sMinute)//";"// &
    " time text: "//trim(sStr), TRIM(__FILE__),__LINE__)

  if(iScanSec1 /= 0) then
    sSecond = sStr(iScanSec1 - iOffset : iScanSec2 - iOffset )
    read(sSecond,fmt=*, iostat = iStat) iSecond
    call Assert(iStat==0, "Error parsing hour value from text file - got "//trim(sSecond)//";"// &
      " time text: "//trim(sStr), TRIM(__FILE__),__LINE__)
  else
    iSecond = 0
  endif

  this%iHour = iHour
  this%iMinute = iMinute
  this%iSecond = iSecond

end subroutine parse_text_to_time_sub

!--------------------------------------------------------------------------

subroutine calc_water_year_sub(this)

  class (T_DATETIME) :: this

    if(this%iMonth > 9) then
       this%iWaterYearHigh = this%iYear + 1
    else
       this%iWaterYearHigh = &
       this%iYear
    end if

    if(this%iMonth < 4) then
      this%iWaterYearLow = this%iYear - 1
    else
      this%iWaterYearLow = this%iYear
    endif

end subroutine calc_water_year_sub

!--------------------------------------------------------------------------

! subroutine populate_julian_day_sub(this, iMonth, iDay, iYear, &
!                                 iHour, iMinute, iSecond)
!
!   class (T_DATETIME) :: this
!   integer (kind=T_INT), intent(in) :: iMonth
!   integer (kind=T_INT), intent(in) :: iDay
!   integer (kind=T_INT), intent(in) :: iYear
!   integer (kind=T_INT), intent(in) :: iHour
!   integer (kind=T_INT), intent(in) :: iMinute
!   integer (kind=T_INT), intent(in) :: iSecond
!
!   ! [LOCALS]
! !  integer (kind=T_INT) :: iJulianDay
! !  real (kind=T_DBL) :: rFractionOfDay
!
!   this%iMonth = iMonth
!   this%iDay = iDay
!   this%iYear = iYear
!   this%iHour = iHour
!   this%iMinute = iMinute
!   this%iSecond = iSecond
!
! !  this%iJulianDay = julian_day( this%iYear, this%iMonth, this%iDay)
!   this%iJulianDay = julian_day( int(this%iYear, kind=T_INT), &
!                                 int(this%iMonth, kind=T_INT), &
!                                 int(this%iDay, kind=T_INT))
!
!   this%rFractionOfDay = real(this%iHour, kind=T_DBL) / 24_T_DBL + &
!                    real(this%iMinute, kind=T_DBL) / 1440_T_DBL + &
!                    real(this%iSecond, kind=T_DBL) / 86400_T_DBL
!
! !  this%rJulianDay = real(iJulianDay, kind=T_DBL) + rFractionOfDay !&
! !                                     - 2400000.5_T_DBL
!
!   ! 2400000.5 is subtracted to yield one definition of a "MODIFIED JUILAN DAY"
!
! end subroutine populate_julian_day_sub

!--------------------------------------------------------------------------

subroutine calc_julian_day_sub(this, iMonth, iDay, iYear, &
                                iHour, iMinute, iSecond)

  class (T_DATETIME) :: this
  integer (kind=T_INT), intent(in), optional :: iMonth
  integer (kind=T_INT), intent(in), optional :: iDay
  integer (kind=T_INT), intent(in), optional :: iYear
  integer (kind=T_INT), intent(in), optional :: iHour
  integer (kind=T_INT), intent(in), optional :: iMinute
  integer (kind=T_INT), intent(in), optional :: iSecond

  if(present(iMonth) ) this%iMonth = iMonth
  if(present(iDay) ) this%iDay = iDay
  if(present(iYear) ) this%iYear = iYear
  if(present(iHour) ) this%iHour = iHour
  if(present(iMinute) ) this%iMinute = iMinute
  if(present(iSecond) ) this%iSecond = iSecond

  this%iJulianDay = julian_day( int(this%iYear, kind=T_INT), &
                                int(this%iMonth, kind=T_INT), &
                                int(this%iDay, kind=T_INT))
  this%rFractionOfDay = real(this%iHour, kind=T_DBL) / 24_T_DBL + &
                   real(this%iMinute, kind=T_DBL) / 1440_T_DBL + &
                   real(this%iSecond, kind=T_DBL) / 86400_T_DBL

!  this%rJulianDay = real(iJulianDay, kind=T_DBL) + rFractionOfDay ! - 2400000.5_T_DBL

end subroutine calc_julian_day_sub

!--------------------------------------------------------------------------

subroutine calc_gregorian_date_sub(this)

  class (T_DATETIME) :: this

  ! [ LOCALS ]
  integer (kind=T_INT) :: iMonth
  integer (kind=T_INT) :: iDay
  integer (kind=T_INT) :: iYear
  integer (kind=T_INT) :: iHour
  integer (kind=T_INT) :: iMinute
  integer (kind=T_INT) :: iSecond

  real(kind=T_SGL) :: rHour, rMinute, rSecond

  call gregorian_date(this%iJulianDay, iYear, iMonth, iDay)

  this%iYear = iYear
  this%iMonth = iMonth
  this%iDay = iDay

  rHour = this%rFractionOfDay * 24_T_DBL
  iHour = iHour

  rMinute = (rHour - real(iHour, kind=T_SGL) ) * 1440_T_DBL
  iMinute = int(rMinute, kind=T_INT)

  rSecond = ( rMinute - real(iMinute, kind=T_SGL) ) * 86400_T_DBL
  iSecond = int(rSecond, kind=T_INT)

  this%iHour = iHour
  this%iMinute = iMinute
  this%iHour = iSecond

end subroutine calc_gregorian_date_sub

!!***

!--------------------------------------------------------------------------
!!****f* types/gregorian_date
! NAME
!   gregorian_date - Convert from a Julian day number to a Gregorian date.
!
! SYNOPSIS
!   Conversion to a Gregorian calendar date from a Julian date.
!   Valid for any Gregorian calendar date producing a Julian day number
!   greater than zero.
!
! INPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
! OUTPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! NOTES
!   Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
!   Communications of the ACM, Vol. 11, No. 10 (October, 1968).
!   Modified from code found at:
!       http://aa.usno.navy.mil/faq/docs/JD_Formula.html
!
! SOURCE

! subroutine gregorian_date(iJD, iYear, iMonth, iDay, iOrigin)
!
! !! COMPUTES THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY)
! !! GIVEN THE JULIAN DATE (JD).
!
!   ! [ ARGUMENTS ]
!   integer (kind=T_INT) :: iJD
!   integer (kind=T_INT), intent(inout) :: iYear, iMonth, iDay
!   integer (kind=T_INT), optional :: iOrigin
!   ! [ LOCALS ]
!   integer (kind=T_INT) iI,iJ,iK,iL,iN
!   integer (kind=T_INT) :: iOffset
!
!   if(present(iOrigin)) then
!     iOffset = iOrigin
!   else
!     iOffset = 0
!   endif
!
!   ! allow for an alternate "origin" to be specified... technically,
!   ! this is no longer a "Julian" day, but alas... This modification
!   ! was required in order to process the "time" variables from global
!   ! climate models, which seem to be defined as something like this:
!   ! time:units = "days since 1960-01-01 00:00:00"
!   !
!   ! for the above example, JD = 2436935 on the first day; the NetCDF "time"
!   ! variable will be equal to 0.  Thus, in order to get the conversion
!   ! right, we must add 0 + 2436935 to yield a true Julian Day.
!
!   iJD = iJD + iOffset
!
!   iL= iJD + 68569_T_INT
!   iN= 4*iL / 146097_T_INT
!   iL= iL - (146097_T_INT * iN + 3_T_INT)/4_T_INT
!   iI= 4000_T_INT * (iL + 1_T_INT) / 1461001_T_INT
!   iL= iL - 1461_T_INT * iI / 4_T_INT + 31_T_INT
!   iJ= 80_T_INT * iL / 2447_T_INT
!   iK= iL - 2447_T_INT * iJ / 80_T_INT
!   iL= iJ / 11_T_INT
!   iJ= iJ + 2_T_INT - 12_T_INT * iL
!   iI= 100_T_INT * (iN - 49_T_INT) + iI + iL
!
!   iYear = iI
!   iMonth = iJ
!   iDay = iK
!
!   return
!
! end subroutine gregorian_date
!
!
! !--------------------------------------------------------------------------
! !!****f* types/julian_day
! ! NAME
! !   julian_day - Convert from a Gregorian calendar date to a Julian day number.
! !
! ! SYNOPSIS
! !   Conversion from a Gregorian calendar date to a Julian day number.
! !   Valid for any Gregorian calendar date producing a Julian day
! !   greater than zero.
! !
! ! INPUTS
! !   iYear   4-digit year
! !   iMonth  2-digit month (1-12)
! !   iDay    2-digit day (1-31)
! !
! ! OUTPUTS
! !   iJD     integer number of days that have elapsed since noon
! !           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
! !
! ! SOURCE
!
! function julian_day ( iYear, iMonth, iDay, iOrigin ) result(iJD)
!
!   ! [ ARGUMENTS ]
!   integer (kind=T_INT), intent(in) :: iYear, iMonth, iDay
!   integer (kind=T_INT), optional :: iOrigin
!
!   ! [ LOCALS ]
!   integer (kind=T_INT) i,j,k
!   integer (kind=T_INT) :: iOffset
!   character (len=256) :: sBuf
!
!   ! [ RETURN VALUE ]
!   integer (kind=T_INT) :: iJD
!
!   i= iYear
!   j= iMonth
!   k= iDay
!
!   if(.not. (iMonth >= 1 .and. iMonth <= 12)) then
!     write(sBuf,fmt="('Illegal month value given: ',i4)") iMonth
!     call Assert( lFALSE, trim(sBuf), TRIM(__FILE__), __LINE__)
!   elseif(.not. (iDay >= 1 .and. iDay <= 31)) then
!     write(sBuf,fmt="('Illegal day value given: ',i4)") iDay
!     call Assert( lFALSE, trim(sBuf), TRIM(__FILE__), __LINE__)
!   endif
!
!   if(present(iOrigin)) then
!     iOffset = iOrigin
!   else
!     iOffset = 0
!   endif
!
!   iJD= ( k-32075_T_INT + 1461_T_INT * (i + 4800_T_INT + (j - 14_T_INT) / 12_T_INT) &
!         /4_T_INT + 367_T_INT * (j - 2_T_INT - (j - 14_T_INT)/ 12_T_INT * 12_T_INT) &
!         /12_T_INT - 3_T_INT *((i + 4900_T_INT + (j - 14_T_INT) &
!         /12_T_INT)/100_T_INT)/4_T_INT ) - iOffset
!
!   return
!
! end function julian_day

!------------------------------------------------------------------------------

function is_date_greater_than(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

!  if(date2%iJulianDay == date1%iJulianDay &
!     .and. date1%rFractionOfDay > date2%rFractionOfDay) then
!     lResult = lTRUE
!  elseif(date1%iJulianDay > date2%iJulianDay) then
!    lResult = lTRUE
!  endif

  if( date1%getJulianDay() > date2%getJulianDay() )  lResult = lTRUE

end function is_date_greater_than

!------------------------------------------------------------------------------

function is_date_less_than(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

!  if(date1%iJulianDay == date2%iJulianDay &
!     .and. date1%rFractionOfDay < date2%rFractionOfDay) then
!     lResult = lTRUE
!  elseif(date1%iJulianDay < date2%iJulianDay) then
!    lResult = lTRUE
!  endif

  if( date1%getJulianDay() < date2%getJulianDay() )  lResult = lTRUE

end function is_date_less_than

!------------------------------------------------------------------------------

function is_date_LT_or_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if( date1%getJulianDay() <= date2%getJulianDay() )  lResult = lTRUE

end function is_date_LT_or_equal_to

!------------------------------------------------------------------------------

function is_date_GT_or_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if( date1%getJulianDay() >= date2%getJulianDay() )  lResult = lTRUE

end function is_date_GT_or_equal_to

!------------------------------------------------------------------------------

function is_date_equal_to(date1, date2)   result(lResult)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=T_LOGICAL ) :: lResult

  lResult = lFALSE

  if( date1%iJulianDay == date2%iJulianDay .and. &
             date1%iHour == date2%iHour .and. &
             date1%iMinute == date2%iMinute .and. &
             date1%iSecond == date2%iSecond) then

     lResult = lTRUE

  endif

end function is_date_equal_to

!------------------------------------------------------------------------------

function date_minus_date_fn(date1, date2)  result(rDelta)

  class(T_DATETIME), intent(in) :: date1
  class(T_DATETIME), intent(in) :: date2
  real (kind=T_DBL) :: rDelta

  rDelta = date1%getJulianDay() - date2%getJulianDay()

end function date_minus_date_fn

!------------------------------------------------------------------------------


!------------------------------------------------------------------------------


!------------------------------------------------------------------------------

function write_pretty_date_fn(this)     result(sDateText)

  class(T_DATETIME) :: this
  character (len=10) :: sDateText

  write(sDateText, fmt="(i2.2,'/',i2.2,'/',i4.4)") &
    this%iMonth, this%iDay, this%iYear

end function write_pretty_date_fn

!------------------------------------------------------------------------------

function write_list_date_fn(this)                     result(sDateText)

  class(T_DATETIME) :: this
  character (len=10) :: sDateText

  ! [ LOCALS ]
  integer (kind=T_INT), dimension(5) :: iStat
!  sDateText = this%listdatetime()

  write(sDateText(iScanMM1:iScanMM2),fmt="(i2.2)", iostat=iStat(1)) this%iMonth
  write(sDateText(iScanDD1:iScanDD2),fmt="(i2.2)", iostat=iStat(2)) this%iDay
  write(sDateText(iScanYYYY1:iScanYYYY2),fmt="(i4.4)",iostat=iStat(3)) this%iYear
  if(iScanDelim1 > 0) write(sDateText(iScanDelim1:iScanDelim1), &
     fmt="(a1)",iostat=iStat(4)) &
     sDATE_FORMAT(iScanDelim1:iScanDelim1)
  if(iScanDelim2 > 0) write(sDateText(iScanDelim2:iScanDelim2), &
     fmt="(a1)",iostat=iStat(5)) &
     sDATE_FORMAT(iScanDelim2:iScanDelim2)

  call Assert(all(iStat==0),"Problem parsing the date format '"// &
     trim(sDATE_FORMAT)//"' for output", &
    trim(__FILE__), __LINE__)

end function write_list_date_fn

!------------------------------------------------------------------------------

function write_list_time_fn(this)                     result(sTimeText)

  class(T_DATETIME) :: this
  character (len=8) :: sTimeText

  write(sTimeText,fmt="(i2.2,':',i2.2':',i2.2)") this%iHour, this%iMinute, this%iSecond

end function write_list_time_fn

!------------------------------------------------------------------------------

function write_list_datetime_fn(this)    result(sDatetimeText)

  class(T_DATETIME) :: this
!  character(len=*), optional :: sDateFormat
!  logical (kind=T_LOGICAL), optional :: lDateOnly
  character (len=19) :: sDatetimeText

  ! [ LOCALS ]
!  character(len=25) sDateFmt
!  integer (kind=T_INT) :: iScanMM1, iScanMM2
!  integer (kind=T_INT) :: iScanDD1, iScanDD2
!  integer (kind=T_INT) :: iScanYYYY1, iScanYYYY2
!  integer (kind=T_INT) :: iScanDelim1, iScanDelim2
  character (len=32) :: sBuf
!  character (len=6), parameter :: DELIMITERS = "/-_\. "
  integer (kind=T_INT), dimension(5) :: iStat
!  logical (kind=T_LOGICAL) lListTime

  sDateTimeText = ""

!  if(present(sDateFormat)) then
!    sDateFmt = uppercase(trim(adjustl(sDateFormat)))
!  else
!    sDateFmt = "MM/DD/YYYY"
!  endif

!  if(present(lDateOnly)) then
!    lListTime = .not. lDateOnly
!  else
!    lListTime = lTRUE
!  endif

!  iScanMM1 = scan(string=sDateFmt,set="M")
!  iScanMM2 = scan(string=sDateFmt,set="M", back=lTRUE )

!  iScanDD1 = scan(string=sDateFmt,set="D")
!  iScanDD2 = scan(string=sDateFmt,set="D", back=lTRUE )

!  iScanYYYY1 = scan(string=sDateFmt,set="Y")
!  iScanYYYY2 = scan(string=sDateFmt,set="Y", back=lTRUE )

!  iScanDelim1 = scan(string=trim(sDateFmt), set=DELIMITERS)
!  iScanDelim2 = scan(string=trim(sDateFmt), set=DELIMITERS, back=lTRUE)

  write(sDateTimeText(iScanMM1:iScanMM2),fmt="(i2.2)", iostat=iStat(1)) this%iMonth
  write(sDateTimeText(iScanDD1:iScanDD2),fmt="(i2.2)", iostat=iStat(2)) this%iDay
  write(sDateTimeText(iScanYYYY1:iScanYYYY2),fmt="(i4.4)",iostat=iStat(3)) this%iYear
  if(iScanDelim1 > 0) write(sDateTimeText(iScanDelim1:iScanDelim1), &
     fmt="(a1)",iostat=iStat(4)) &
     sDATE_FORMAT(iScanDelim1:iScanDelim1)
  if(iScanDelim2 > 0) write(sDateTimeText(iScanDelim2:iScanDelim2), &
     fmt="(a1)",iostat=iStat(5)) &
     sDATE_FORMAT(iScanDelim2:iScanDelim2)

  call Assert(all(iStat==0),"Problem parsing the date format '"// &
     trim(sDATE_FORMAT)//"' for output", &
    trim(__FILE__), __LINE__)

  write(sBuf,fmt="(1x,i2.2,':',i2.2':',i2.2)") this%iHour, this%iMinute, this%iSecond

!  if(lListTime) then
    sDateTimeText = trim(sDateTimeText) // trim(sBuf)
!  else
!    sDateTimeText = trim(sDateTimeText)
!  endif

end function write_list_datetime_fn

!------------------------------------------------------------------------------

subroutine system_time_to_date_sub(this)

  class (T_DATETIME) :: this

  ! [ LOCALS ]
  character (len=16) :: sDateText
  character (len=16) :: sTimeText
  integer (kind=T_INT), dimension(8) :: iValues

  call DATE_AND_TIME(sDateText, sTimeText)
  call DATE_AND_TIME(VALUES = iValues)

  call this%setDateFormat("YYYYMMDD")
  call this%setTimeFormat("HHMMSS")

  call this%parseDate(sDateText)
  call this%parseTime(sTimeText)
  call this%calcJulianDay()
  this%rFractionOfDay = this%rFractionOfDay + &
      (real(iValues(8), kind=T_DBL) / 86400_T_DBL / 1000_T_DBL) ! milliseconds

  call this%setDateFormat()
  call this%setTimeFormat()

end subroutine system_time_to_date_sub

!------------------------------------------------------------------------------

subroutine new_daterange_fm_text_sub(this, sStartDate, sStartTime, sEndDate, sEndTime, &
     sDateFormat)

  class (T_DATERANGE) :: this
  character (len=*), intent(in) :: sStartDate
  character (len=*), intent(in) :: sStartTime
  character (len=*), intent(in) :: sEndDate
  character (len=*), intent(in) :: sEndTime
  character (len=*), intent(in), optional :: sDateFormat

  ! [ LOCALS ]
  character (len=14) :: sDateFormatText

  if(present(sDateFormat)) then
    sDateFormatText = trim(sDateFormat)
  else
    sDateFormatText = "MM/DD/YYYY"
  endif

  call this%tStartDate%setDateFormat(sDateFormatText)

  call this%tStartDate%parseDate(sStartDate)
  call this%tStartDate%parseTime(sStartTime)
  call this%tStartDate%calcJulianDay()

  call this%tEndDate%parseDate(sEndDate)
  call this%tEndDate%parseTime(sEndTime)
  call this%tEndDate%calcJulianDay()

end subroutine new_daterange_fm_text_sub

!--------------------------------------------------------------------------

subroutine new_daterange_fm_datetime_sub(this, tStartDate, tEndDate )

  class (T_DATERANGE) :: this
  type (T_DATETIME), intent(in) :: tStartDate
  type (T_DATETIME), intent(in) :: tEndDate

  ! [ LOCALS ]

  this%tStartDate = tStartDate
  this%tEndDate = tEndDate

end subroutine new_daterange_fm_datetime_sub


!--------------------------------------------------------------------------

!> \brief Return the number of days in the given year.
!!
!! This function simply returns the number of days given the current year.
!function day_of_year(iJulianDay) result(iDOY)

!  integer (kind=T_INT), intent(in) :: iJulianDay

  ! [ LOCALS ]
!  integer (kind=T_INT) :: iFirstDay, iCurrDay, iDOY
!  integer (kind=T_INT) :: iYear, iMonth, iDay

  ! first get the value for the current year
!  call gregorian_date(iJulianDay, iYear, iMonth, iDay)

  ! now calculate the Julian day for the first of the year
!  iFirstDay = julian_day ( iYear, 1, 1 )

  ! return the current day of the year
!  iDOY = iJulianDay - iFirstDay + 1

!  return

!end function day_of_year

!--------------------------------------------------------------------------
!!****f* types/solstice
! NAME
!   solstice - Returns 0 normally, or a value >0 during solstice or equinox.
!
! SYNOPSIS
!    Returns the following:
!      0: non-solstice and non-equinox day
!      1: Vernal equinox
!      2: Summer Solstice
!      3: Autumnal equinox
!      4: Winter solstice
!
! INPUTS
!   iJD     Julian day value
!
! OUTPUTS
!   iSol    Code as described above
!
! SOURCE

!function solstice (iJD)  result (iSol)

  ! [ ARGUMENTS ]
!  integer (kind=T_INT), intent(in) :: iJD

  ! [ LOCALS ]
!  integer (kind=T_INT) iMonth, iDay, iYear


!  ! [ RETURN VALUE ]
!  integer (kind=T_INT) :: iSol

!  call gregorian_date(iJD, iYear, iMonth, iDay)

!  if(iMonth==3 .and. iDay == 20) then
!    iSol = 1
!  elseif(iMonth==6 .and. iDay == 21) then
!    iSol = 2
!  elseif(iMonth==9 .and. iDay == 22) then
!    iSol = 3
!  elseif(iMonth==12 .and. iDay == 21) then
!   iSol = 4
!  else
!    iSol = 0
!  endif
!
!  return
!
!end function solstice

!------------------------------------------------------------------------------

function get_julian_day_float_fn(this)                   result(rJulianDay)

  class(T_DATETIME) :: this
  real (kind=T_DBL) :: rJulianDay

  rJulianDay = real(this%iJulianDay, kind=T_DBL) + real(this%rFractionOfDay,kind=T_DBL)

end function get_julian_day_float_fn

!------------------------------------------------------------------------------

function get_days_in_month_fn(this)  result(iDaysInMonth)

  class(T_DATETIME) :: this
  integer (kind=T_INT) :: iDaysInMonth

  ! [ LOCALS ]
  integer (kind=T_INT), dimension(12), parameter :: iNumberOfDaysInMonth = &
    [31,28,31,30,31,30,31,31,30,31,30,31]

  iDaysInMonth = iNumberOfDaysInMonth(this%iMonth)

  if (this%lIsLeapYear) then
    iDaysInMonth = max(iDaysInMonth, 29)
  endif

end function get_days_in_month_fn

!------------------------------------------------------------------------------

function get_days_in_year_fn(this)  result(iDaysInYear)

  class(T_DATETIME) :: this
  integer (kind=T_INT) :: iDaysInYear

  if (this%lIsLeapYear) then
    iDaysInYear = 366
  else
    iDaysInYear = 365
  endif

end function get_days_in_year_fn

!------------------------------------------------------------------------------

subroutine date_plus_day_sub(this)

  class(T_DATETIME) :: this

  this%iJulianDay = this%iJulianDay + 1
  call this%calcGregorianDate()
  call this%isLeapYear()

end subroutine date_plus_day_sub

!------------------------------------------------------------------------------

subroutine date_minus_day_sub(this)

  class(T_DATETIME) :: this

  this%iJulianDay = this%iJulianDay - 1
  call this%calcGregorianDate()
  call this%isLeapYear()

end subroutine date_minus_day_sub

!------------------------------------------------------------------------------

subroutine is_leap_year(this)

  class(T_DATETIME) :: this

  this%lIsLeapYear = ( mod(this%iYear, 4) == 0 .and. mod(this%iYear, 100) /= 0 ) .or. &
                 ( mod(this%iYear, 400) == 0 .and. this%iYear /= 0 )

end subroutine is_leap_year


end module datetime
