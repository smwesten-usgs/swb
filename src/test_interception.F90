program test_interception

  use iso_c_binding, only : c_float, c_int
  use interception_linear
  use interception_exponential

  implicit none

  real (kind=c_float), parameter :: fA = 0.05
  real (kind=c_float), parameter :: fB = 1.0

  real (kind=c_float), parameter :: fC = .35
  real (kind=c_float), parameter :: fD = 0.2

  real (kind=c_float) :: fRainfall(10) = &
    [0., 0.5, 1., 2., 3., 4., 5., 6., 7., 8.]

  real (kind=c_float) :: fFog(10) = &
    [0., .25, .5, .75, 1.0, 1.25, 1.5, 1.75, 2., 2.5]

  integer (kind=c_int) :: iIndex, iIndex2    
  
  type (T_INTERCEPTION_LINEAR) :: intcp1
  type (T_INTERCEPTION_EXPONENTIAL) :: intcp2
  type (T_INTERCEPTION_EXPONENTIAL) :: intcp3	

  call intcp1%initialize( fConstant_1=fA, fConstant_2=fB)
  call intcp2%initialize( fConstant_1=fC, fConstant_2=fD)
  call intcp3%initialize( fConstant_1=fC, fConstant_2=2.*fD)

  write(*, fmt="(8x,10(a,8x))") "Index","Rainfall","Fog","Intcp1","Intcp2","Intcp3"

  do iIndex=1, ubound(fRainfall,1)

  	do iIndex2=1, ubound(fFog,1)

  	  print *, iIndex, fRainfall(iIndex), fFog(iIndex2), intcp1%compute(fRainfall(iIndex), fFog(iIndex2)), &
  	                       intcp2%compute(fRainfall(iIndex), fFog(iIndex2)), &
  	                       intcp3%compute(fRainfall(iIndex), fFog(iIndex2))

  	enddo                       

  enddo	                       

end program test_interception  