!> @file
!>  Contains a single module, \ref graph, which
!>  creates plots through calls to the DISLIN library


!>  Creates plots through calls to the DISLIN library.
module graph

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use dislin
  use types

  implicit none
  save

  contains

  subroutine makegraph(pGraph, pGrd, iVarNum)

    type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
    type(T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
    integer (c_int), intent(in) :: iVarNum
    real, dimension(pGrd%iNX,pGrd%iNY) :: ZMAT
    integer (c_int) :: iCol,iRow
    character (len=256) :: sBuf

#ifdef GRAPHICS_SUPPORT

    ! [ LOCALS ]

    real :: XA,XE,XOR,XSTEP
    real :: YA,YE,YOR,YSTEP
    real :: ZA,ZE,ZOR,ZSTEP
    real :: rAspectRatio
    integer :: iX, iY, iNXW, iNYW, iNXP, iNYP
    integer , parameter :: iSZ = 1024    ! base graph size

    real (c_float) :: rMin, rMean, rMax
    integer :: iNumGridCells
    character (len=256) :: sPaperSize
    real :: rFG_Color
    integer :: iBG_Color
    integer (c_int), dimension(pGrd%iNX,pGrd%iNY) :: iMask
    character (len=256) :: sDislin

    ! is the DISLIN environment variable defined? if so, it is OK
    ! to make a call to BMPFNT
    call get_environment_variable("DISLIN", sDislin)

    ! establish number of cells in model grid
!    iNumGridCells = pGrd%iNY * pGrd%iNX
    iNumGridCells = count(pGrd%iMask == iACTIVE_CELL)

    XA = REAL(pGrd%rX0)
    XE = REAL(pGrd%rX1)
    XOR = XA
    XSTEP = (XE - XA) / 5.

    YA = REAL(pGrd%rY0)
    YE = REAL(pGrd%rY1)
    YOR = YA
    YSTEP = (YE - YA) / 5.

    ZA = REAL(pGraph(iVarNum)%rZA(pGraph(iVarNum)%iTimeFrame))
    ZE = REAL(pGraph(iVarNum)%rZE(pGraph(iVarNum)%iTimeFrame))
    ZOR = REAL(pGraph(iVarNum)%rZOR(pGraph(iVarNum)%iTimeFrame))
    ZSTEP = REAL(pGraph(iVarNum)%rZSTEP(pGraph(iVarNum)%iTimeFrame))

    iX = INT(pGrd%iNX)
    iY = INT(pGrd%iNY)

    select case(pGrd%iDataType)

      case(DATATYPE_INT)
        do iCol=1,iX
          do iRow=1,iY
            ZMAT(iCol,iRow) = REAL(pGrd%iData(iCol,(iY-iRow+1)))
            iMask(iCol, iRow) = pGrd%iMask(iCol,(iY-iRow+1))
          end do
        end do

      case(DATATYPE_REAL)
        do iCol=1,iX
          do iRow=1,iY
            ZMAT(iCol,iRow) = REAL(pGrd%rData(iCol,(iY-iRow+1)))
            iMask(iCol, iRow) = pGrd%iMask(iCol,(iY-iRow+1))
          end do
        end do

      case(DATATYPE_CELL_GRID)

        call Assert(lFALSE,"Unsupported grid type (T_GRID_CELL) was used in call", &
          TRIM(__FILE__), __LINE__)

    end select

    ! if no data and ZA == ZE, make up a maximum and calc ZSTEP
    ! accordingly
!    if(approx_equal(real(ZA,c_float), real(ZE,c_float) ) ) then
     if( int(ZA) == int(ZE) ) then
      ZE = ZA * 1.1 + .1
      ZSTEP = (ZE - ZA) / 10.
    endif

    rMin = minval(ZMAT, iMask == iACTIVE_CELL)
    rMax = maxval(ZMAT, iMask == iACTIVE_CELL)
    rMean = sum(ZMAT, iMask == iACTIVE_CELL) / count(iMask == iACTIVE_CELL)

    write(sBuf,"(a)") &
       "min: "//adjustl(trim(asCharacter(rMin,sFmt="F9.2"))) &
       //"  mean: "//adjustl(trim(asCharacter(rMean,sFmt="F9.2"))) &
       //"  max: "//adjustl(trim(asCharacter(rMax,sFmt="F9.2")))

    ! METAFL defines the metafile format.
    ! 'PS'     defines a coloured PostScript file.
    ! 'EPS'     defines an Encapsulated PostScript file. The format is nearly the same as for 'PS'.
    ! 'PDF'     defines a PDF file.
    ! 'WMF'     defines a Windows metafile.
    ! 'PNG'     defines a PNG file.
    ! 'BMP'     defines a Windows Bitmap format.

      CALL METAFL(TRIM(pGraph(iVarNum)%cCDEV))

    !'USAL'     US paper size A, landscape,  2790 *  2160 points.
    !'USAP' US paper size A, portrait, 2160 *  2790 points
!      call SETPAG(TRIM(pGraph(iVarNum)%cPAPER_SIZE))

    rAspectRatio = REAL(iX) / REAL(iY)

    ! set paper size for Postscript-based output options
    if(rAspectRatio>1.) then
      sPaperSize = "USAL"
      iNXP = 2790
      iNYP = 2160
    else
      sPaperSize = "USAP"
      iNXP = 2160
      iNYP = 2790
    endif

!      iNXP = INT(rAspectRatio * REAL(iSZ,c_float))
    iNXW = iNXP

!      iNYP = INT((1./rAspectRatio) * REAL(iSZ,c_float) * 1.15)
      iNYW = iNYP


!      write(UNIT=LU_LOG,FMT=*) "iX: ",iX,"    iY: ",iY
!      write(UNIT=LU_LOG,FMT=*) "iNXP: ",iNXP, "   iNYP: ", iNYP
!      write(UNIT=LU_LOG,FMT=*) " "

!      call PAGE(iNXP,iNYP)

     ! WINSIZ: This routine defines the size of windows and the
     ! resolution of DISLIN image formats such as TIFF, PNG, BMP,
     ! PPM and IMAGE. By default, the window size is
     ! set to 2/3 of the screen size, and the resolution of
     ! image formats is 853 x 603 pixels.

    ! Allow scaling up to full page size
    CALL SCLMOD('FULL')

    ! A output image filename can be set with SETFIL
    call SETFIL(TRIM(pGraph(iVarNum)%cSETFIL))

    ! Set options specific to the desired output type
    ! ==> LEVEL 0 <==

    select case(TRIM(pGraph(iVarNum)%cCDEV))

      case("PNG")

        ! The routine PNGMOD enables transparency for PNG files.
!        call PNGMOD('ON','TRANSPARENCY')
        call PAGE(iNXP,iNYP)
        call WINSIZ(iNXW/2,iNYW/2)

      case("PDF")

        call SETPAG(TRIM(sPaperSize))

      case("GIF")

        ! The routine GIFMOD enables transparency for GIF files.
        call GIFMOD('ON','TRANSPARENCY')
        call WINSIZ(iNXW,iNYW)

      case("WMF")

      case("TIFF")

        call WINSIZ(iNXW,iNYW)

      case("BMP")

        call WINSIZ(iNXW,iNYW)

      case("PS")

        call SETPAG(TRIM(sPaperSize))

      case("EPS")

        call SETPAG(TRIM(sPaperSize))

      case("SVG")

        call SETPAG(TRIM(sPaperSize))

      case default

        call assert(lFALSE, "Unknown device specified for output plots." &
          //" Device: "//trim(pGraph(iVarNum)%cCDEV))

    end select


    ! WINSIZ: This routine defines the size of windows and the
    ! resolution of DISLIN image formats such as TIFF, PNG, BMP,
    ! PPM and IMAGE. By default, the window size is
    ! set to 2/3 of the screen size, and the resolution of
    ! image formats is 853 x 603 pixels.

    !------------------------------------------------------------
    ! ** INITIALIZE PLOT - MOVE TO LEVEL 1
    !------------------------------------------------------------

    ! DISINI: initializes DISLIN by setting default parameters
    ! and creating a plotfile.
    CALL DISINI()

    ! ERRMOD: The printing of warnings and the output of the
    ! protocol in DISFIN can be disabled with the routine ERRMOD.
    if(pGraph(iVarNum)%lEnableDislinMessages) then
      call ERRMOD('ALL','ON')
    else
      call ERRMOD('ALL','OFF')
    endif

    ! SETVLT: selects a colour table.
    ! 'SPEC'     defines 256 colours arranged in a rainbow where 0 means
    ! black and 255 means white. This colour table uses more
    ! violet colours than 'RAIN'.
    if (TRIM(pGraph(iVarNum)%cCOLOR_TABLE) /= "CUSTOM") then
      CALL SETVLT(TRIM(pGraph(iVarNum)%cCOLOR_TABLE))
    else
      call makeColorTable()
    endif

    rFG_Color = 1.
    iBG_Color = 0

    ! LABELS: The routine LABELS defines contour labels.
    CALL LABELS('FLOAT','Z')

    ! LABDIG: Set the number of digits following the decimal place.
    ! select an appropriate number of digits
    if(ZE >= 100.) then
      CALL LABDIG (1, 'Z')
    elseif(ZE < 1.) then
      CALL LABDIG (3, 'Z')
    else
      CALL LABDIG (2, 'Z')
    endif

    ! SETGRF removes a part of an axis or a complete axis from an axis system.
    ! The call is:     CALL SETGRF (C1, C2, C3, C4)     level 1, 2, 3
    ! or:    void setgrf (char *c1, char *c2, char *c3, char *c4);
    ! The parameters can have the values
    ! 'NONE', 'LINE', 'TICKS', 'LABELS' and 'NAME'.
    call SETGRF('LINES','LINES','LINES','LINES')

    ! Set options specific to the desired output type
    ! ==> LEVEL 1 <==
    if(TRIM(pGraph(iVarNum)%cCDEV)=="PNG") then
      if(len_trim(sDislin) > 0) then
        CALL BMPFNT(TRIM(pGraph(iVarNum)%cFONT_NAME))
      else
        CALL DUPLX()
      endif
      ! set the foreground color; SETRGB(1., 1., 1.) should set
      ! background color to "WHITE"
      call SETRGB(rFG_Color, rFG_Color, rFG_Color)
      ! set the background color
      CALL PAGFLL(iBG_Color)

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="PDF") then
      CALL PSFONT(TRIM(pGraph(iVarNum)%cFONT_NAME))

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="GIF") then
      CALL HWFONT()
!           call COLOR('YELLOW')

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="WMF") then
      CALL WINFNT(TRIM(pGraph(iVarNum)%cFONT_NAME))
!           call COLOR('YELLOW')

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="TIFF") then
      CALL BMPFNT(TRIM(pGraph(iVarNum)%cFONT_NAME))
!           call COLOR('YELLOW')

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="BMP") then
        if(len_trim(sDislin) > 0) then
          CALL BMPFNT(TRIM(pGraph(iVarNum)%cFONT_NAME))
        else
          CALL DUPLX()
        endif
!           call COLOR('YELLOW')

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="PS") then
      CALL PSFONT(TRIM(pGraph(iVarNum)%cFONT_NAME))

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="EPS") then
      CALL PSFONT(TRIM(pGraph(iVarNum)%cFONT_NAME))

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="SVG") then
      CALL PSFONT(TRIM(pGraph(iVarNum)%cFONT_NAME))

    endif

    ! COLOR defines the colours used for plotting text and lines.


    ! PAGERA: plots a border around the page.
!      CALL PAGERA()

    ! TITLIN: This subroutine defines up to four lines of
    ! text used for axis system titles. The text can be
    ! plotted with TITLE after a call to GRAF.

    ! populate second line of title
    CALL TITLIN(TRIM(pGraph(iVarNum)%cTITLE),2)
    ! populate fourth line of title
    CALL TITLIN(sBuf,4)

    CALL NAME(TRIM(pGraph(iVarNum)%cZ_AXIS_TITLE),'Z')

    ! AUTRES: With a call to AUTRES, the size of coloured rectangles
    ! will be automatically calculated by GRAF3 or CRVMAT.
    CALL AUTRES(iX,iY)

    ! CENTER:  center the axis system on the page
    call CENTER()

    ! The routine GRAF3 plots a 3-D axis system where the
    ! Z-axis is plotted as a colour bar.
    CALL GRAF3(XA, &
               XE, &
               XOR, &
               XSTEP, &
               YA, &
               YE, &
               YOR, &
               YSTEP, &
               ZA, &
               ZE, &
               ZOR, &
               ZSTEP)

    ! CRVMAT: plots a coloured surface according to a matrix
    CALL CRVMAT(ZMAT,iX,iY,1,1)

    ! HEIGHT: defines the character height.
    CALL HEIGHT(50)

    ! TITLE: places the title text onto the page
    CALL TITLE()

    CALL DISFIN()
    return

#endif

  end subroutine makegraph

!----------------------------------------------------------------------------------

  subroutine make_shaded_contour(pGrd, sOutputFilename, sTitleTxt, sAxisTxt, &
      rMinZVal, rMaxZVal)

    type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
    character (len=*) :: sOutputFilename
    character (len=*) :: sTitleTxt
    character (len=*) :: sAxisTxt
    real (c_float), optional :: rMinZVal
    real (c_float), optional :: rMaxZVal

#ifdef GRAPHICS_SUPPORT

    ! [ LOCALS ]
    real, dimension(pGrd%iNX,pGrd%iNY) :: ZMAT
    integer (c_int) :: iCol,iRow
    character (len=256) :: sBuf = ""
    character (len=256) :: sSummaryTxt = ""
    real (c_float) :: rH_V_AspectRatio
    integer (c_int) :: iPixVRes = 1000.
    integer (c_int) :: iPixHRes
    integer (c_int), dimension(pGrd%iNX,pGrd%iNY) :: iMask

    integer (c_int) :: iPtVRes
    integer (c_int) :: iPtHRes
    integer (c_int) :: iPtVOrigin
    integer (c_int) :: iPtHOrigin
    integer (c_int) :: iPtVAxLen
    integer (c_int) :: iPtHAxLen

    character (len=256) :: sDislin

    ! EXPLANATION OF DISLIN VARIABLES
    ! XA, XE        :: lower and upper limits of the X-axis.
    ! XOR, XSTP     :: first X-axis label and the step between labels.
    ! YA, YE        :: lower and upper limits of the Y-axis.
    ! YOR, YSTP     :: first Y-axis label and the step between labels.
    ! ZA, ZE        :: lower and upper limits of the Z-axis.
    ! ZOR, ZSTP     :: first Z-axis label and the step between labels.

    real :: XA,XE,XOR,XSTEP
    real :: YA,YE,YOR,YSTEP
    real :: ZA,ZE,ZOR,ZSTEP

    integer :: iX, iY, iNXW, iNYW
    integer (c_int) :: iNumGridCells

    call get_environment_variable("DISLIN", sDislin)

    ! establish number of cells in model grid
!    iNumGridCells = pGrd%iNY * pGrd%iNX
    iNumGridCells = count(pGrd%iMask == iACTIVE_CELL)

    XA = REAL(pGrd%rX0)
    XE = REAL(pGrd%rX1)
    XOR = XA
    XSTEP = (XE - XA) / 5.

    YA = REAL(pGrd%rY0)
    YE = REAL(pGrd%rY1)
    YOR = YA
    YSTEP = (YE - YA) / 5.

    iX = INT(pGrd%iNX)
    iY = INT(pGrd%iNY)

    ! determine page size in PIXEL dimensions
    rH_V_AspectRatio = REAL(pGrd%iNX) / REAL(pGrd%iNY) ! + 0.2  ! add extra for colorbar
    iPixHRes = INT(REAL(iPixVRes) * rH_V_AspectRatio)

    ! determine page size in POINT dimensions; for some reason DISLIN seems to like
    ! larger values to work with, therefore we multiply by 4
    iPtHRes = iPixHRes * 4
    iPtVRes = iPixVRes * 4

    iPtVOrigin = INT(REAL(iPtVRes) * 0.97)
    iPtHOrigin = INT(REAL(iPtHRes) * 0.03)

    iPtVAxLen = INT(REAL(iPtVRes) * 0.82)
    iPtHAxLen = INT(REAL(iPtHRes) * 0.82)

    select case(pGrd%iDataType)

      case(DATATYPE_INT)

        if (present(rMinZVal) .and. present(rMaxZVal)) then

          ZA = rMinZVal
          ZE = rMaxZVal

        elseif ( present(rMinZVal) ) then

          ZA = rMinZVal
          ZE = maxval(pGrd%iData, mask=pGrd%iMask == iACTIVE_CELL)

        elseif ( present(rMaxZVal) ) then

          ZA = minval(pGrd%iData, mask=pGrd%iMask == iACTIVE_CELL)
          ZE = rMaxZVal

        else

          if(minval(pGrd%iData) <= 0 .and. maxval(pGrd%iData) <= 0) then
            ZA = maxval(pGrd%iData, mask=pGrd%iMask == iACTIVE_CELL)
            ZE = minval(pGrd%iData, mask=pGrd%iMask == iACTIVE_CELL)
          else
            ZA = minval(pGrd%iData, mask=pGrd%iMask == iACTIVE_CELL)
            ZE = maxval(pGrd%iData, mask=pGrd%iMask == iACTIVE_CELL)
          endif

        endif

        ZOR = ZA
        ZSTEP = (ZE - ZA) / 10.

        do iRow=1,iY
          do iCol=1,iX
            ZMAT(iCol,iRow) = REAL(pGrd%iData(iCol,(iY-iRow+1)),c_float)
            iMask(iCol, iRow) = pGrd%iMask(iCol,(iY-iRow+1))
          end do
        end do

      case(DATATYPE_REAL)

        if (present(rMinZVal) .and. present(rMaxZVal)) then

          ZA = rMinZVal
          ZE = rMaxZVal

        elseif ( present(rMinZVal) ) then

          ZA = rMinZVal
          ZE = maxval(pGrd%rData, mask=pGrd%iMask == iACTIVE_CELL)

        elseif ( present(rMaxZVal) ) then

          ZA = minval(pGrd%rData, mask=pGrd%iMask == iACTIVE_CELL)
          ZE = rMaxZVal

        else

          if(minval(pGrd%rData) <= 0. .and. maxval(pGrd%rData) <= 0.) then
            ZA = maxval(pGrd%rData, mask=pGrd%iMask == iACTIVE_CELL)
            ZE = minval(pGrd%rData, mask=pGrd%iMask == iACTIVE_CELL)
          else
            ZA = minval(pGrd%rData, mask=pGrd%iMask == iACTIVE_CELL)
            ZE = maxval(pGrd%rData, mask=pGrd%iMask == iACTIVE_CELL)
          endif

        endif

        ZOR = ZA
        ZSTEP = (ZE - ZA) / 10.

        ! flip array up-down
        do iRow=1,iY
          do iCol=1,iX
            ZMAT(iCol,iRow) = REAL(pGrd%rData(iCol,(iY-iRow+1)))
            iMask(iCol, iRow) = pGrd%iMask(iCol,(iY-iRow+1))
          end do
        end do

      case(DATATYPE_CELL_GRID)

        call Assert(lFALSE,"Unsupported grid type (T_GRID_CELL) was used in call", &
          TRIM(__FILE__), __LINE__)

      end select

      ! if no data and ZA == ZE, make up a maximum and calc ZSTEP
      ! accordingly
!      if(approx_equal(ZA, ZE) ) then
      if( int(ZA) == int(ZE) ) then
        ZE = ZA * 1.1 + .1
        ZSTEP = (ZE - ZA) / 10.
      endif

      write(sSummaryTxt,"(a,f9.2,a,f9.2,a,f9.2)") &
        "  min: ",minval(ZMAT,iMask == iACTIVE_CELL), &
        "  mean: ",sum(ZMAT, iMask == iACTIVE_CELL)/iNumGridCells, &
        "  max: ",maxval(ZMAT, iMask == iACTIVE_CELL)

      ! METAFL defines the metafile format
      CALL METAFL("PNG")

      ! The routine PNGMOD enables transparency for PNG files.
!       CALL PNGMOD('ON','TRANSPARENCY')

      ! PAGE determines the size of the page.
      call PAGE(iPtHRes,iPtVRes)
!       call SETPAG('da4p')

      ! Allow scaling up to full page size
      CALL SCLMOD('FULL')

      ! The routine FILMOD determines if a new plot file name is
      ! created for existing files.
      CALL FILMOD('DELETE')

      ! A output image filename can be set with SETFIL
      call SETFIL(TRIM(sOutputFilename))

      ! DISINI initializes DISLIN by setting default parameters and creating a plotfile.
      CALL DISINI()

      ! The printing of warnings and the output of the protocol in DISFIN
      ! can be disabled with the routine ERRMOD.
      call ERRMOD('ALL','ON')

      ! This routine defines the size of windows and the resolution of
      ! DISLIN image formats such as TIFF, PNG, BMP, PPM and IMAGE.
      ! By default, the window size is set to 2/3 of the screen size,
      ! and the resolution of image formats is 853 x 603 pixels.
      call WINSIZ(iPixHRes,iPixVRes)

!      call axspos (iPtHOrigin, iPtVOrigin)
!      call ax3len (iPtHAxLen, iPtVAxLen, iPtVAxLen)

      ! COLOR defines the colours used for plotting text and lines.
      call COLOR('YELLOW')

      ! SETVLT selects a colour table.
      ! 'SPEC'  defines 256 colours arranged in a rainbow where 0 means
      ! black and 255 means white. This colour table uses more
      ! violet colours than 'RAIN'.
      CALL SETVLT('SPEC')

!     CALL HELVES()

      if(len_trim(sDislin) > 0) then
        call BMPFNT("helve")
      else
        CALL DUPLX()
      endif
!      CALL SHDCHA()

      ! The routine LABELS defines contour labels.
      CALL LABELS('FLOAT','Z')

      if(ZE >= 100.) then
        CALL LABDIG (1, 'Z')
      elseif(ZE < 1.) then
        CALL LABDIG (3, 'Z')
      else
        CALL LABDIG (2, 'Z')
      endif

      ! SETGRF removes a part of an axis or a complete axis from an axis system.
      ! The call is:  CALL SETGRF (C1, C2, C3, C4)  level 1, 2, 3
      ! or: void setgrf (char *c1, char *c2, char *c3, char *c4);
      ! The parameters can have the values
      ! 'NONE', 'LINE', 'TICKS', 'LABELS' and 'NAME'.
      call SETGRF('LINES','LINES','LINES','LINES')

      ! With a call to AUTRES, the size of coloured rectangles
      ! will be automatically calculated by GRAF3 or CRVMAT.
      CALL AUTRES(iX,iY)

      CALL PAGERA()

      CALL NAME(TRIM(sAxisTxt),'Z')

      ! HEIGHT defines the character height.
      CALL HEIGHT(50)

      CALL TITLIN(TRIM(sTitleTxt),1)
      CALL TITLIN(TRIM(sSummaryTxt),3)

    ! The routine GRAF3 plots a 3-D axis system where the Z-axis
    ! is plotted as a colour bar.

    ! The call is:     CALL GRAF3 (XA, XE, XOR, XSTP, YA, YE, YOR, YSTP, ZA, ZE, ZOR, ZSTP)     level 1

    ! XA, XE     are the lower and upper limits of the X-axis.
    ! XOR, XSTP  are the first X-axis label and the step between labels.
    ! YA, YE     are the lower and upper limits of the Y-axis.
    ! YOR, YSTP  are the first Y-axis label and the step between labels.
    ! ZA, ZE     are the lower and upper limits of the Z-axis.
    ! ZOR, ZSTP  are the first Z-axis label and the step between labels.

    CALL CENTER()

    CALL GRAF3(XA, &
               XE, &
               XOR, &
               XSTEP, &
               YA, &
               YE, &
               YOR, &
               YSTEP, &
               ZA, &
               ZE, &
               ZOR, &
               ZSTEP)

    ! CRVMAT plots a coloured surface according to a matrix
    CALL CRVMAT(ZMAT,iX,iY,1,1)

    CALL TITLE()

    CALL DISFIN()

#endif

  end subroutine make_shaded_contour

!----------------------------------------------------------------------

  subroutine makeColorTable()

    integer :: iIndex
    real :: XR, XG, XB, XH, XS, XV
    real :: rStartHue, rEndHue, rDeltaHue

    rStartHue = 75.
    rEndHue = 330.
    rDeltaHue = rEndHue - rStartHue

    XS = 0.9  ! saturation = 0.9 on a scale from 0. to 1.
    XV = 0.9  ! value (brightness) = 0.9 on a scale from 0. to 1.

#ifdef GRAPHICS_SUPPORT

    do iIndex=0,255

      XH = rStartHue + real(iIndex) / 255. * rDeltaHue

  		CALL HSVRGB (XH, XS, XV, XR, XG, XB)
   	  CALL SETIND (iIndex, XR, XG, XB)

		enddo

 	  CALL SETIND (0, 0., 0., 0.)  !set first color in set to BLACK

#endif

  end subroutine makeColorTable

end module graph
