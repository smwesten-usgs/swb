!> @file
!> @brief  Contains a single module, \ref graph, which
!>  creates plots through calls to the DISLIN library


!> @brief  Creates plots through calls to the DISLIN library.
module graph

#ifdef GRAPHICS_SUPPORT

  use dislin
  use types
  implicit none
  save

  contains

  subroutine makegraph(pGraph,pGrd, iVarNum)

    type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
    type(T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
    integer (kind=T_INT), intent(in) :: iVarNum
    real, dimension(pGrd%iNX,pGrd%iNY) :: ZMAT
    integer (kind=T_INT) :: iCol,iRow
    character (len=256) :: sBuf

    ! [ LOCALS ]

    real :: XA,XE,XOR,XSTEP
    real :: YA,YE,YOR,YSTEP
    real :: ZA,ZE,ZOR,ZSTEP
    real :: rAspectRatio
    integer :: iX, iY, iNXW, iNYW, iNXP, iNYP
    integer , parameter :: iSZ = 1024    ! base graph size

    integer :: iNumGridCells
    character (len=256) :: sPaperSize
    real :: rFG_Color
    integer :: iBG_Color
    character (len=256) :: sDislin

    ! is the DISLIN environment variable defined? if so, it is OK
    ! to make a call to BMPFNT
    call get_environment_variable("DISLIN", sDislin)

    ! establish number of cells in model grid
    iNumGridCells = pGrd%iNY * pGrd%iNX

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

      case(T_INT_GRID)
        do iCol=1,iX
          do iRow=1,iY
            ZMAT(iCol,iRow) = REAL(pGrd%iData(iCol,(iY-iRow+1)))
          end do
        end do

      case(T_SGL_GRID)
        do iCol=1,iX
          do iRow=1,iY
            ZMAT(iCol,iRow) = REAL(pGrd%rData(iCol,(iY-iRow+1)))
          end do
        end do

      case(T_CELL_GRID)

        call Assert(lFALSE,"Unsupported grid type (T_GRID_CELL) was used in call", &
          TRIM(__FILE__), __LINE__)

    end select

    ! if no data and ZA == ZE, make up a maximum and calc ZSTEP
    ! accordingly
!    if(approx_equal(real(ZA,kind=T_SGL), real(ZE,kind=T_SGL) ) ) then
     if( int(ZA) == int(ZE) ) then
      ZE = ZA * 1.1 + .1
      ZSTEP = (ZE - ZA) / 10.
    endif

    write(sBuf,"(a7,f9.2,a9,f9.2,a7,f9.2)") &
       "  min: ",minval(ZMAT), &
       "   mean: ",sum(ZMAT)/iNumGridCells, &
       "  max: ",maxval(ZMAT)

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

!      iNXP = INT(rAspectRatio * REAL(iSZ,kind=T_SGL))
    iNXW = iNXP

!      iNYP = INT((1./rAspectRatio) * REAL(iSZ,kind=T_SGL) * 1.15)
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

    ! A output image filename can be set with SETFIL
    call SETFIL(TRIM(pGraph(iVarNum)%cSETFIL))

    ! Set options specific to the desired output type
    ! ==> LEVEL 0 <==
    if(TRIM(pGraph(iVarNum)%cCDEV)=="PNG") then
      ! The routine PNGMOD enables transparency for PNG files.
!       call PNGMOD('ON','TRANSPARENCY')
      call PAGE(iNXP,iNYP)
      call WINSIZ(iNXW/2,iNYW/2)

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="PDF") then
      call SETPAG(TRIM(sPaperSize))

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="GIF") then
      ! The routine GIFMOD enables transparency for GIF files.
      call GIFMOD('ON','TRANSPARENCY')
      call WINSIZ(iNXW,iNYW)

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="WMF") then

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="TIFF") then
      call WINSIZ(iNXW,iNYW)

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="BMP") then
      call WINSIZ(iNXW,iNYW)

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="PS") then
      call SETPAG(TRIM(sPaperSize))

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="EPS") then
      call SETPAG(TRIM(sPaperSize))

    elseif(TRIM(pGraph(iVarNum)%cCDEV)=="SVG") then
      call SETPAG(TRIM(sPaperSize))

    endif

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
    CALL SETVLT(TRIM(pGraph(iVarNum)%cCOLOR_TABLE))

    if(TRIM(pGraph(iVarNum)%cCOLOR_TABLE)=="RRAIN" &
      .or. TRIM(pGraph(iVarNum)%cCOLOR_TABLE)=="RGREY" &
      .or. TRIM(pGraph(iVarNum)%cCOLOR_TABLE)=="RSPEC") then
      rFG_Color = 1.
      iBG_Color = 255
    else
      rFG_Color = 1.
      iBG_Color = 0
    end if

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
      call SETRGB(rFG_Color, rFG_Color, rFG_Color)
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
    CALL CRVMAT(ZMAT,iX,iY,2,2)

    ! HEIGHT: defines the character height.
    CALL HEIGHT(50)

    ! TITLE: places the title text onto the page
    CALL TITLE()

    CALL DISFIN()
    return

  end subroutine makegraph

!----------------------------------------------------------------------------------

  subroutine make_shaded_contour(pGrd, sOutputFilename, sTitleTxt, sAxisTxt)

    type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
    character (len=*) :: sOutputFilename
    character (len=*) :: sTitleTxt
    character (len=*) :: sAxisTxt

    ! [ LOCALS ]
    real, dimension(pGrd%iNX,pGrd%iNY) :: ZMAT
    integer (kind=T_INT) :: iCol,iRow
    character (len=256) :: sBuf = ""
    character (len=256) :: sSummaryTxt = ""
    real (kind=T_SGL) :: rH_V_AspectRatio
    integer (kind=T_INT) :: iPixVRes = 600.
    integer (kind=T_INT) :: iPixHRes

    integer (kind=T_INT) :: iPtVRes
    integer (kind=T_INT) :: iPtHRes
    integer (kind=T_INT) :: iPtVOrigin
    integer (kind=T_INT) :: iPtHOrigin
    integer (kind=T_INT) :: iPtVAxLen
    integer (kind=T_INT) :: iPtHAxLen

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
    integer (kind=T_INT) :: iNumGridCells

    call get_environment_variable("DISLIN", sDislin)

    ! establish number of cells in model grid
    iNumGridCells = pGrd%iNY * pGrd%iNX

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

      case(T_INT_GRID)

        if(minval(pGrd%iData) <= 0 .and. maxval(pGrd%iData) <= 0) then
          ZA = maxval(pGrd%iData)
          ZE = minval(pGrd%iData)
        else
          ZA = minval(pGrd%iData)
          ZE = maxval(pGrd%iData)
        endif

        ZOR = ZA
        ZSTEP = (ZE - ZA) / 10.

        do iRow=1,iY
          do iCol=1,iX
            ZMAT(iCol,iRow) = REAL(pGrd%iData(iCol,(iY-iRow+1)),kind=T_SGL_DISLIN)
          end do
        end do

      case(T_SGL_GRID)

        if(minval(pGrd%rData) <= 0. .and. maxval(pGrd%rData) <= 0.) then
          ZA = maxval(pGrd%rData)
          ZE = minval(pGrd%rData)
        else
          ZA = minval(pGrd%rData)
          ZE = maxval(pGrd%rData)
        endif

        ZOR = ZA
        ZSTEP = (ZE - ZA) / 10.

        ! flip array up-down
        do iRow=1,iY
          do iCol=1,iX
            ZMAT(iCol,iRow) = REAL(pGrd%rData(iCol,(iY-iRow+1)))
          end do
        end do

      case(T_CELL_GRID)

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
        "  min: ",minval(ZMAT), &
        "  mean: ",sum(ZMAT)/iNumGridCells, &
        "  max: ",maxval(ZMAT)

      ! METAFL defines the metafile format
      CALL METAFL("PNG")

      ! The routine PNGMOD enables transparency for PNG files.
!       CALL PNGMOD('ON','TRANSPARENCY')

      ! A output image filename can be set with SETFIL
      call SETFIL(TRIM(sOutputFilename))

      ! The routine FILMOD determines if a new plot file name is
      ! created for existing files.
      CALL FILMOD('DELETE')

      ! PAGE determines the size of the page.
      call PAGE(iPtHRes,iPtVRes)
!       call SETPAG('da4p')

      ! Allow scaling up to full page size
      CALL SCLMOD('FULL')

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
    return

  end subroutine make_shaded_contour

#endif

end module graph
