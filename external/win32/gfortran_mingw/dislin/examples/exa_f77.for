      PROGRAM EXAMPL
      PARAMETER (NR1=24,NR2=17)

      CHARACTER CR0*58,CR1(NR1)*58,CR2(NR2)*58,CDEV*4,C*1

      DATA CR0/
     *'**********************************************************'/

      DATA (CR1(I),I=1,16)/
     *'                      DISLIN-Examples                     ',
     *'                                                          ',
     *'Fortran 77 Demo of the Data Plotting Library DISLIN.      ',
     *'Author: H. Michels,  MPI fuer Sonnensystemforschung       ',
     *'                                                          ',
     *'Device:      defines the metafile.                        ',
     *'   = CONS    Screen (full window).                        ', 
     *'   = XWIN    Screen (small window).                       ', 
     *'   = GL      Screen (OpenGL window).                      ', 
     *'   = GKSL    A GKSLIN metafile will be created.           ',
     *'   = CGM     A CGM metafile will be created.              ',
     *'   = PS      A PostScript file will created.              ',
     *'   = EPS     An EPS file will created.                    ',
     *'   = PDF     A PDF file will created.                     ',
     *'   = WMF     A Windows metafil will be created.           ',
     *'   = HPGL    A HPGL file will be created.                 '/

      DATA (CR1(I),I=17,NR1)/
     *'   = SVG     A SVG file will be created.                  ',
     *'   = JAVA    A Java applet file will be created.          ',
     *'   = GIF     A GIF file will be created.                  ', 
     *'   = TIFF    A TIFF file will be created.                 ', 
     *'   = PNG     A PNG file will be created.                  ', 
     *'   = PPM     A PPM file will be created.                  ', 
     *'   = BMP     A BMP file will be created.                  ', 
     *'   = IMAG    An Image file will be created.               '/

      DATA CR2/
     *'                      DISLIN-Examples                     ',
     *'                                                          ',
     *' 0: Program end                  15: World Coastlines     ',
     *' 1: Demo of Curve                16: Elliptical Proj.     ',
     *' 2: Polar Plots                  17: Azimuthal Proj.      ',
     *' 3: Symbols                      18: Conical Proj.        ',
     *' 4: Logarithmic Axes             19: Bar Graphs           ',
     *' 5: Interpolation Methods        20: Pie Charts           ',
     *' 6: Line Styles                  21: 3-D Bar/Pie Graphs   ',
     *' 7: Legend                       22: Contours             ',
     *' 8: Shading Patterns (AREAF)     23: Shaded Contours      ',
     *' 9: Vectors                      24: Instruction Alphabet ',
     *'10: Shading Patterns (PIEGRF)    25: 3-D Colour Plot      ',
     *'11: Surface Plot                 26: Shaded World         ',
     *'12: Surface Plot                 27: Colour Tables        ',
     *'13: Parametric Function Plot                              ',
     *'14: Map Plot                     30: Menue end            '/

10    WRITE(6,'(/4X,3A)')            '/****',CR0,   '****/'
      WRITE(6,'(4X,3A,23(/4X,3A))') ('/**  ',CR1(I),'  **/',I=1,NR1)
      WRITE(6,'(4X,3A)')             '/****',CR0,   '****/'

      WRITE(6,'(/4X,A)') '_Device: '
      READ(5,'(A)') CDEV
      IF(CDEV.EQ.' ') GOTO 100

20    WRITE(6,'(/4X,3A)')            '/****',CR0,   '****/'
      WRITE(6,'(4X,3A,16(/4X,3A))') ('/**  ',CR2(I),'  **/',I=1,NR2)
      WRITE(6,'(4X,3A)')             '/****',CR0,   '****/'

      WRITE(6,'(/4X,A)') '_Option: '
      READ(5,*) IOPT

      CALL METAFL(CDEV)
      CALL SETPAG('DA4L')

      IF(IOPT.EQ.1) THEN
        CALL EXA_1
      ELSE IF(IOPT.EQ.2) THEN
        CALL EXA_2
      ELSE IF(IOPT.EQ.3) THEN
        CALL EXA_3
      ELSE IF(IOPT.EQ.4) THEN
        CALL EXA_4
      ELSE IF(IOPT.EQ.5) THEN
        CALL EXA_5
      ELSE IF(IOPT.EQ.6) THEN
        CALL EXA_6
      ELSE IF(IOPT.EQ.7) THEN
        CALL EXA_7
      ELSE IF(IOPT.EQ.8) THEN
        CALL EXA_8
      ELSE IF(IOPT.EQ.9) THEN
        CALL EXA_9
      ELSE IF(IOPT.EQ.10) THEN
        CALL EXA_10
      ELSE IF(IOPT.EQ.11) THEN
        CALL EXA_11
      ELSE IF(IOPT.EQ.12) THEN
        CALL EX12_1
      ELSE IF(IOPT.EQ.13) THEN
        CALL EX12_2
      ELSE IF(IOPT.EQ.14) THEN
        CALL EXA_12
      ELSE IF(IOPT.EQ.15) THEN
        CALL EX13_1
      ELSE IF(IOPT.EQ.16) THEN
        CALL EX13_2
      ELSE IF(IOPT.EQ.17) THEN
        CALL EX13_3
      ELSE IF(IOPT.EQ.18) THEN
        CALL EX13_4
      ELSE IF(IOPT.EQ.19) THEN
        CALL EX10_1
      ELSE IF(IOPT.EQ.20) THEN
        CALL EX10_2
      ELSE IF(IOPT.EQ.21) THEN
        CALL EX10_3
      ELSE IF(IOPT.EQ.22) THEN
        CALL EX14_1
      ELSE IF(IOPT.EQ.23) THEN
        CALL EX14_2
      ELSE IF(IOPT.EQ.24) THEN
        CALL EX6_1
      ELSE IF(IOPT.EQ.25) THEN
        CALL EX11_1
      ELSE IF(IOPT.EQ.26) THEN
        CALL EXA_13
      ELSE IF(IOPT.EQ.27) THEN
        CALL EXA_14
      ELSE IF(IOPT.EQ.30) THEN
        GOTO 10
      ELSE IF(IOPT.EQ.0) THEN
        GOTO 100
      END IF

      WRITE(6,'(4X,A)') 'Press RETURN to continue ...'
      READ(5,'(A)') C
      IF((C.EQ.'Q').OR.(C.EQ.'q')) STOP
      GOTO 20

100   CONTINUE
      END

      SUBROUTINE EXA_1
      PARAMETER (N=100)
      COMMON /WDATA/ XRAY(300),YRAY(300),Y1RAY(100),Y2RAY(100),
     *               ZMAT(50,50)

      PI=3.1415926
      FPI=PI/180.
      STEP=360./(N-1)

      DO I=1,N
        XRAY(I)=(I-1)*STEP
        X=XRAY(I)*FPI
        Y1RAY(I)=SIN(X)
        Y2RAY(I)=COS(X)
      END DO

      CALL DISINI
      CALL PAGERA
      CALL HWFONT
      CALL AXSPOS(450,1800)
      CALL AXSLEN(2200,1200)

      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')

      CALL LABDIG(-1,'X')
      CALL TICKS(10,'XY')

      CALL TITLIN('Demonstration of CURVE',1)
      CALL TITLIN('SIN(X), COS(X)',3)

      CALL GRAF(0.,360.,0.,90.,-1.,1.,-1.,0.5)
      CALL TITLE

      CALL COLOR('RED')
      CALL CURVE(XRAY,Y1RAY,N)
      CALL COLOR('GREEN')
      CALL CURVE(XRAY,Y2RAY,N)

      CALL COLOR('FORE')
      CALL DASH
      CALL XAXGIT

      CALL DISFIN
      END

      SUBROUTINE EXA_2
      PARAMETER (N=300,M=10)
      COMMON /WDATA/ XRAY(300),YRAY(300),Y1RAY(100),Y2RAY(100),
     *               ZMAT(50,50)
      REAL X2(M),Y2(M)

      XPI=3.1415927
      STEP=360./(N-1)
      DO I=1,N
        A=(I-1)*STEP
        A=A*XPI/180
        YRAY(I)=A
        XRAY(I)=SIN(5*A)
      END DO

      DO I=1,M
        X2(I)=I
        Y2(I)=I
      END DO
 
      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT

      CALL TITLIN ('Polar Plots', 2)
      CALL TICKS(3,'Y')
      CALL AXENDS('NOENDS','X')
      CALL LABDIG(-1,'Y')
      CALL AXSLEN(1000,1000)
      CALL AXSORG(1050,900)

      CALL POLAR(1.,0., 0.2, 0., 30.)
      CALL CURVE(XRAY,YRAY,N)
      CALL HTITLE(50)
      CALL TITLE
      CALL ENDGRF

      CALL LABDIG(-1,'X')
      CALL AXSORG(1050,2250)
      CALL LABTYP('VERT','Y')
      CALL BARWTH (5.)
      
      CALL POLAR(10.,0.,2.,0.,30.)
      CALL BARWTH(-5.)
      CALL POLCRV('FBARS')
      CALL CURVE(X2,Y2,M)
      CALL DISFIN
      END

      SUBROUTINE EXA_3
      CHARACTER CTIT*20,CSTR*2
      CTIT='Symbols'

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL COLOR('YELLOW')
      CALL HWFONT
      CALL PAGHDR('H. Michels  (',')',2,0)

      CALL HEIGHT(60)

      NL=NLMESS(CTIT)
      CALL MESSAG(CTIT,(2100-NL)/2,200)

      CALL HEIGHT(50)
      CALL HSYMBL(120)

      NY=150

      DO I=0,23
        IF(MOD(I,4).EQ.0) THEN
          NY=NY+400
          NXP=550
        ELSE
          NXP=NXP+350
        END IF

        IF(I.LT.10) THEN
          WRITE(CSTR,'(I1)') I
        ELSE
          WRITE(CSTR,'(I2)') I
        END IF

        NL=NLMESS(CSTR)/2
        CALL MESSAG(CSTR,NXP-NL,NY+150)
        CALL SYMBOL(I,NXP,NY)
      END DO

      CALL DISFIN
      END

      SUBROUTINE EXA_4
      CHARACTER CTIT*60,CLAB(3)*5
      DATA CLAB/'LOG','FLOAT','ELOG '/

      CTIT='Logarithmic Scaling'

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT

      CALL AXSLEN(1400,500)
      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')
      CALL AXSSCL('LOG','XY')

      CALL TITLIN(CTIT,2)

      DO I=1,3
        NYA=2650-(I-1)*800
        CALL LABDIG(-1,'XY')
        IF(I.EQ.2)THEN
          CALL LABDIG(1,'Y')
          CALL NAME(' ','X')
        END IF

        CALL AXSPOS(500,NYA)
        CALL COLOR('YELLOW')
        CALL MESSAG('Labels: '//CLAB(I),600,NYA-400)
        CALL COLOR('FORE')
        CALL LABELS(CLAB(I),'XY')
        CALL GRAF(0.,3.,0.,1.,-1.,2.,-1.,1.)

        IF(I.EQ.3) THEN
          CALL HEIGHT(50)
          CALL TITLE
        END IF

        CALL ENDGRF
      END DO

      CALL DISFIN
      END

      SUBROUTINE EXA_5

      DIMENSION X(16), Y(16)
      CHARACTER CPOL(6)*8,CTIT*60

      DATA X/0.,1.,3.,4.5,6.,8.,9.,11.,12.,12.5,13.,15.,16.,
     *         17.,19.,20./
     *     Y/2.,4.,4.5,3.,1.,7.,2.,3.,5.,2.,2.5,2.,4.,6.,5.5,4./
     *     CPOL/'SPLINE','STEM','BARS','STAIRS','STEP','LINEAR'/
     *     NYA/2700/

      CTIT='Interpolation Methods'

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL HWFONT
      CALL PAGERA
      CALL INCMRK(1)
      CALL HSYMBL(25)
      CALL TITLIN(CTIT,1)
      CALL AXSLEN(1500,350)
      CALL SETGRF('LINE','LINE','LINE','LINE')

      DO I=1,6
        CALL AXSPOS(350,NYA-(I-1)*350)
        CALL POLCRV(CPOL(I))
        CALL MARKER(0)

        CALL GRAF(0.,20.,0.,5.,0.,10.,0.,5.)
        NX=NXPOSN(1.)
        NY=NYPOSN(8.)
        CALL COLOR('YELLOW')
        CALL MESSAG(CPOL(I),NX,NY)
        CALL CURVE(X,Y,16)
        CALL COLOR('FORE')

        IF(I.EQ.6) THEN
          CALL HEIGHT(50)
          CALL TITLE
        END IF
        CALL ENDGRF
      END DO

      CALL DISFIN
      END

      SUBROUTINE EXA_6
      DIMENSION X(2),Y(2)
      CHARACTER CTYP(8)*6
      DATA X/3.,9./CTYP/'SOLID','DOT','DASH','CHNDSH',
     *                        'CHNDOT','DASHM','DOTL','DASHL'/

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL CENTER
      CALL CHNCRV('BOTH')
      CALL HWFONT

      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')

      CALL TITLIN('Demonstration of CURVE',1)
      CALL TITLIN('Line Styles',3)

      CALL GRAF(0.,10.,0.,2.,0.,10.,0.,2.)
      CALL TITLE

      DO I=1,8
        Y(1)=9.5-I
        Y(2)=9.5-I
        NY=NYPOSN(Y(1))
        NX=NXPOSN(1.0)
        CALL MESSAG(CTYP(I),NX,NY-20)
        CALL CURVE(X,Y,2)
      END DO

      CALL DISFIN
      END

      SUBROUTINE EXA_7
      PARAMETER(N=100)
      COMMON /WDATA/ XRAY(300),YRAY(300),Y1RAY(100),Y2RAY(100),
     *               ZMAT(50,50)
      CHARACTER CBUF*14

      FPI=3.1415926/180.
      STEP=360./(N-1)
      DO I=1,N
        XRAY(I)=(I-1)*STEP
        X=XRAY(I)*FPI
        Y1RAY(I)=SIN(X)
        Y2RAY(I)=COS(X)
      END DO

      CALL DISINI
      CALL HWFONT
      CALL PAGERA

      CALL AXSPOS(450,1800)
      CALL AXSLEN(2200,1200)

      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')
      CALL LABDIG(-1,'X')
      CALL TICKS(10,'XY')

      CALL TITLIN('Demonstration of CURVE',1)
      CALL TITLIN('Legend',3)

      CALL GRAF(0.,360.,0.,90.,-1.,1.,-1.,0.5)
      CALL TITLE
      CALL XAXGIT

      CALL CHNCRV('BOTH')
      CALL CURVE(XRAY,Y1RAY,N)
      CALL CURVE(XRAY,Y2RAY,N)

C     Statements for the legend
      CALL LEGINI(CBUF,2,7)
      NX=NXPOSN(190.)
      NY=NYPOSN(0.75)
      CALL LEGPOS(NX,NY)
      CALL LEGLIN(CBUF,'sin (x)',1)
      CALL LEGLIN(CBUF,'cos (x)',2)
      CALL LEGTIT('Legend')
      CALL LEGEND(CBUF,3)

      CALL DISFIN
      END

      SUBROUTINE EXA_8
      DIMENSION IXP(4),IYP(4),IX(4),IY(4)
      CHARACTER CTIT*60,CSTR*2
      DATA IX/0,300,300,0/IY/0,0,400,400/

      CTIT='Shading Patterns (AREAF)'

      CALL DISINI
      CALL SETVLT('SMALL')
      CALL PAGERA
      CALL HWFONT

      CALL HEIGHT(50)
      NL=NLMESS(CTIT)
      NX=(2970-NL)/2
      CALL MESSAG(CTIT,NX,200)

      NX0=335
      NY0=350

      DO I=1,3
        NY=NY0+(I-1)*600
        DO J=1,6
          ICLR=(I-1)*6+J-1
          ICLR=MOD(ICLR,8)
          IF(ICLR.EQ.0) ICLR=8
          CALL SETCLR(ICLR)
          NX=NX0+(J-1)*400
          II=(I-1)*6+J-1
          CALL SHDPAT(II)
          WRITE(CSTR,'(I2)') II

          DO K=1,4
            IXP(K)=IX(K)+NX
            IYP(K)=IY(K)+NY
          END DO
          CALL AREAF(IXP,IYP,4)

          NL=NLMESS(CSTR)
          NX=NX+(300-NL)/2
          CALL MESSAG(CSTR,NX,NY+460)
        END DO
      END DO

      CALL DISFIN
      END

      SUBROUTINE EXA_9
      DIMENSION IVEC(20)
      CHARACTER CTIT*60,CNUM*4
      DATA IVEC/0,1111,1311,1421,1531,1701,1911,3111,3311,3421,
     *           3531,3703,4221,4302,4413,4522,4701,5312,5502,5703/

      CTIT='Vectors'

      CALL DISINI
      CALL COLOR('CYAN')
      CALL PAGERA
      CALL HWFONT

      CALL HEIGHT(60)
      NL=NLMESS(CTIT)
      NX=(2970-NL)/2
      CALL MESSAG(CTIT,NX,200)

      CALL HEIGHT(50)
      NX=300
      NY=400

      DO I=1,20
        IF(I.EQ.11) THEN
          NX=NX+2970/2
          NY=400
        END IF

        WRITE(CNUM,'(I4)') IVEC(I)
        NL=NLMESS(CNUM)
        CALL MESSAG(CNUM,NX-NL,NY-25 )

        CALL VECTOR(NX+100,NY,NX+1000,NY,IVEC(I))
        NY=NY+160
      END DO

      CALL DISFIN
      END

      SUBROUTINE EXA_10
      DIMENSION XRAY(18)
      CHARACTER CTIT*60,CBUF*36,CSTR*2
      DATA XRAY/18*1./

      CTIT='Shading Patterns (PIEGRF)'

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT

      CALL AXSPOS(250,2700)
      CALL AXSLEN(1600,2200)
      CALL TITLIN(CTIT,3)
      CALL HEIGHT(50)

      CALL LEGINI(CBUF,18,2)

      DO I=1,18
        WRITE(CSTR,'(I2)') I-1
        CALL LEGLIN(CBUF,CSTR,I)
      END DO

      CALL CHNPIE('BOTH')
      CALL LABELS('NONE','PIE')
      CALL PIEGRF(CBUF,1,XRAY,18)
      CALL TITLE

      CALL DISFIN
      END

      SUBROUTINE EXA_11
      PARAMETER (N=50)
      COMMON /WDATA/ XRAY(300),YRAY(300),Y1RAY(100),Y2RAY(100),
     *               ZMAT(50,50)
      CHARACTER CTIT1*60,CTIT2*60

      FPI=3.14159/180.

      STEP = 360./(N-1)
      DO I=1,N
        X=(I-1)*STEP
        DO J=1,N
          Y=(J-1)*STEP
          ZMAT(I,J)=2*SIN(X*FPI)*SIN(Y*FPI)
        END DO
      END DO
 
      CTIT1='Surface Plot (SURMAT)'
      CTIT2='F(X,Y) = 2*SIN(X)*SIN(Y)'

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT
      CALL AXSPOS(200,2600)
      CALL AXSLEN(1800,1800)

      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')
      CALL NAME('Z-axis','Z')

      CALL TITLIN(CTIT1,2)
      CALL TITLIN(CTIT2,4)

      CALL VIEW3D(-5.,-5.,4.,'ABS')
      CALL GRAF3D(0.,360.,0.,90.,0.,360.,0.,90.,-3.,3.,-3.,1.)
      CALL HEIGHT(50)
      CALL TITLE

      CALL COLOR('GREEN')
      CALL SURMAT(ZMAT,50,50,1,1)

      CALL DISFIN
      END

      SUBROUTINE EX12_1
      DIMENSION IXP(4),IYP(4)
      PARAMETER (N=50)
      COMMON /WDATA/ XRAY(300),YRAY(300),Y1RAY(100),Y2RAY(100),
     *               ZMAT(50,50)
      DATA IXP/200,1999,1999,200/ IYP/2600,2600,801,801/

      FPI=3.14159/180.

      STEP = 360./(N-1)
      DO I=1,N
        X=(I-1)*STEP
        DO J=1,N
          Y=(J-1)*STEP
          ZMAT(I,J)=2*SIN(X*FPI)*SIN(Y*FPI)
        END DO
      END DO
 
      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT

      CALL AXSPOS(200,2600)
      CALL AXSLEN(1800,1800)
      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')
      CALL NAME('Z-axis','Z')
      CALL TITLIN('Surface Plot (SURMAT)',2)
      CALL TITLIN('F(X,Y) = 2*SIN(X)*SIN(Y)',4)

      CALL GRAF3D(0.,360.,0.,90.,0.,360.,0.,90.,-3.,3.,-3.,1.)
      CALL HEIGHT(50)
      CALL TITLE
      CALL SHLSUR

      CALL COLOR('GREEN')
      CALL SURMAT(ZMAT,50,50,1,1)

      CALL COLOR('FORE')
C     Grid in the XY plane
      CALL GRFINI(-1.,-1.,-1.,1.,-1.,-1.,1.,1.,-1.)
      CALL NOGRAF
      CALL GRAF(0.,360.,0.,90.,0.,360.,0.,90.)
      CALL DASHL
      CALL GRID(1,1)
      CALL GRFFIN

C     Grid in the YZ plane
      CALL GRFINI(-1.,-1.,-1.,-1.,1.,-1.,-1.,1.,1.)
      CALL GRAF(0.,360.,0.,90.,-3.,3.,-3.,1.)
      CALL GRID(1,1)
      CALL GRFFIN

C     Shading in the XZ plane
      CALL GRFINI(-1.,1.,-1.,1.,1.,-1.,1.,1.,1.)
      CALL SHDPAT(7)
      CALL SOLID
      CALL AREAF(IXP,IYP,4)
      CALL GRFFIN

      CALL DISFIN
      END

      SUBROUTINE EX12_2
      CHARACTER CTIT1*60,CTIT2*60
      EXTERNAL ZFUN

      CTIT1='Surface Plot of the Parametric Function'
      CTIT2='[COS(t)*(3+COS(u)),SIN(t)*(3+COS(u)), SIN(u)]'
      PI=3.14159

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL HWFONT
      CALL PAGERA
      CALL AXSPOS(200,2400)
      CALL AXSLEN(1800,1800)
      CALL INTAX

      CALL TITLIN(CTIT1,2)
      CALL TITLIN(CTIT2,4)

      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')
      CALL NAME('Z-axis','Z')

      CALL VKYTIT(-300)
      CALL ZSCALE(-1.,1.)
      CALL GRAF3D(-4.,4.,-4.,1.,-4.,4.,-4.,1.,-3.,3.,-3.,1.)

      CALL HEIGHT(40)
      CALL TITLE

      CALL SURMSH('ON')
      STEP=2*PI/30.
      CALL SURFCP(ZFUN,0.,2*PI,STEP,0.,2*PI,STEP)
      CALL DISFIN
      END

      FUNCTION ZFUN(X,Y,IOPT)

      IF(IOPT.EQ.1) THEN
        ZFUN=COS(X)*(3+COS(Y))
      ELSE IF(IOPT.EQ.2) THEN
        ZFUN=SIN(X)*(3+COS(Y))
      ELSE
        ZFUN=SIN(Y)
      END IF
      END

      SUBROUTINE EXA_12
      DIMENSION XC(9),YC(9)
      CHARACTER CSTR(9)*12

      DATA XC/-22.,18.,37.5,0.,2.5,12.5,23.5,-3.75,14.25/
     *     YC/64.,59.6,56.,51.5,48.5,42.,38.,40.3,50.1/
     *   CSTR/'Reykjavik','Stockholm','Moskau','London','Paris',
     *        'Rom','Athen','Madrid','Prag'/

      CALL DISINI
      CALL PAGERA
      CALL HWFONT

      CALL AXSPOS(500,1850)
      CALL AXSLEN(2200,1400)

      CALL LABDIG(-1,'xy')
      CALL TICKS(1,'xy')
      CALL NAME('Longitude','x')
      CALL NAME('Latitude','y')

      CALL TITLIN('Map Plot',3)
      CALL INCMRK(-1)

      CALL LABELS('MAP','xy')
      CALL PROJCT('LAMBERT')
      CALL FRAME(3)
      CALL GRAFMP(-40.,60.,-40.,20.,35.,70.,40.,10.)

      CALL COLOR('GREEN')
      CALL WORLD
      CALL COLOR('FORE')
      CALL CURVMP(XC,YC,9)

      DO I=1,9
        CALL POS2PT(XC(I),YC(I),XP,YP)
        NXP=XP+30
        NYP=YP
        CALL MESSAG(CSTR(I),NXP,NYP)
      END DO

      CALL GRIDMP(1,1)
      CALL HEIGHT(50)
      CALL TITLE
      CALL DISFIN
      END

      SUBROUTINE EX13_1
      CALL SETPAG('DA4L')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT

      CALL FRAME(3)
      CALL AXSPOS(400,1850)
      CALL AXSLEN(2400,1400)

      CALL NAME('Longitude','X')
      CALL NAME('Latitude','Y')
      CALL TITLIN('World Coastlines and Lakes',3)

      CALL LABELS('MAP','XY')
      CALL GRAFMP(-180.,180.,-180.,90.,-90.,90.,-90.,30.)

      CALL GRIDMP(1,1)
      CALL COLOR('GREEN')
      CALL WORLD
      CALL COLOR('FORE')

      CALL HEIGHT(50)
      CALL TITLE
      CALL DISFIN
      END

      SUBROUTINE EX13_2
      CHARACTER CPROJ(3)*6,CTIT*60
      DATA CPROJ/'Sanson','Winkel','Hammer'/

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL HWFONT
      CALL PAGERA

      CALL HEIGHT(40)
      CALL AXSLEN(1600,750)

      NYA=3850
      DO I=1,3
        NYA=NYA-950
        CALL AXSPOS(250,NYA)

        CALL PROJCT(CPROJ(I))
        CALL NOCLIP
        CALL GRAFMP(-180.,180.,-180.,30.,-90.,90.,-90.,15.)

        WRITE(CTIT,'(2A)') 'Elliptical Projection of ',CPROJ(I)
        CALL TITLIN(CTIT,4)
        CALL TITLE

        CALL COLOR('GREEN')
        CALL WORLD
        CALL COLOR('FORE')

        CALL GRIDMP(1,1)
        CALL ENDGRF
      END DO

      CALL DISFIN
      END

      SUBROUTINE EX13_3
      DIMENSION NXA(4),NYA(4),XPOL(4),YPOL(4)
      CHARACTER CTIT*60
      DATA NXA/200,1150,200,1150/NYA/1600,1600,2700,2700/
      DATA XPOL/0.,0.,0.,0./YPOL/0.,45.,90.,-45./

      CTIT='Azimuthal Lambert Projections'

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL HWFONT
      CALL PAGERA

      CALL HEIGHT(50)
      NL=NLMESS(CTIT)
      NX=(2250-NL)/2.
      CALL MESSAG(CTIT,NX,300)

      CALL AXSLEN(900,900)
      CALL PROJCT('LAMBERT')

      DO I=1,4
        CALL AXSPOS(NXA(I),NYA(I))
        CALL MAPPOL(XPOL(I),YPOL(I))
        CALL GRAFMP(-180.,180.,-180.,30.,-90.,90.,-90.,30.)

        CALL COLOR('GREEN')
        CALL WORLD
        CALL COLOR('FORE')
        CALL GRIDMP(1,1)
        CALL ENDGRF
      END DO

      CALL DISFIN
      END

      SUBROUTINE EX13_4
      PARAMETER (N = 32)
      DIMENSION INRAY(N),IPRAY(N),ICRAY(N)

      DO I=1,32
        INRAY(I)=I
        IPRAY(I)=0
        ICRAY(I)=1
      END DO

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT
      CALL SETVLT('SMALL')

      CALL INTAX
      CALL TICKS(1,'XY')
      CALL FRAME(3)
      CALL AXSLEN(1600,2200)
      CALL AXSPOS(400,2700)

      CALL NAME('Longitude','X')
      CALL NAME('Latitude','Y')
      CALL TITLIN('Conformal Conic Projection',3)

      CALL LABELS('MAP','XY')
      CALL PROJCT('CONF')
      CALL GRAFMP(-10.,30.,-10.,5.,35.,70.,35.,5.)

      CALL GRIDMP(1,1)
      CALL SHDEUR(INRAY,IPRAY,ICRAY,N)

      CALL HEIGHT(50)
      CALL TITLE
      CALL DISFIN
      END

      SUBROUTINE EX14_1
      PARAMETER (N=50)
      COMMON /WDATA/ XRAY(300),YRAY(300),Y1RAY(100),Y2RAY(100),
     *               ZMAT(50,50)

      FPI=3.14159/180.
      STEP=360./(N-1)

      DO I=1,N
        XRAY(I)=(I-1.)*STEP
        YRAY(I)=(I-1.)*STEP
      END DO

      DO I=1,N
        DO J=1,N
          ZMAT(I,J)=2*SIN(XRAY(I)*FPI)*SIN(YRAY(J)*FPI)
        END DO
      END DO

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL COMPLX
      CALL PAGERA

      CALL TITLIN('Contour Plot',1)
      CALL TITLIN('F(X,Y) = 2 * SIN(X) * SIN(Y)',3)

      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')

      CALL INTAX
      CALL AXSPOS(450,2670)
      CALL GRAF(0.,360.,0.,90.,0.,360.,0.,90.)

      CALL HEIGHT(30)
      DO I=1,9
        ZLEV=-2.+(I-1)*0.5
        CALL SETCLR(I*25)
        IF(I.EQ.5) THEN
          CALL LABELS('NONE','CONTUR')
        ELSE
          CALL LABELS('FLOAT','CONTUR')
        END IF
        CALL CONTUR(XRAY,N,YRAY,N,ZMAT,ZLEV)
      END DO

      CALL HEIGHT(50)
      CALL COLOR('FORE')
      CALL TITLE

      CALL DISFIN
      END

      SUBROUTINE EX14_2
      PARAMETER (N=50)
      COMMON /WDATA/ XRAY(300),YRAY(300),Y1RAY(100),Y2RAY(100),
     *               ZMAT(50,50)
      DIMENSION ZLEV(12)

      STEP=1.6/(N-1)
      DO I=1,N
        X=0.0+(I-1)*STEP
        XRAY(I)=X
        DO J=1,N
          Y=0.0+(J-1)*STEP
          YRAY(J)=Y
          ZMAT(I,J)=(X*X-1.)**2 + (Y*Y-1.)**2
        END DO
      END DO

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT

      CALL MIXALF
      CALL TITLIN('Shaded Contour Plot',1)
      CALL TITLIN('F(X,Y) = (X[2$ - 1)[2$ + (Y[2$ - 1)[2$',3)
      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')

      CALL SHDMOD('POLY','CONTUR')
      CALL AXSPOS(450,2670)
      CALL GRAF(0.0,1.6,0.0,0.2,0.0,1.6,0.0,0.2)

      DO I=1,12
        ZLEV(13-I)=0.1+(I-1)*0.1
      END DO

      CALL CONSHD(XRAY,N,YRAY,N,ZMAT,ZLEV,12)

      CALL HEIGHT(50)
      CALL TITLE
      CALL DISFIN
      END  

      SUBROUTINE EX6_1
      CHARACTER CT0*100,CT1*100,CT2*100,CT3*100,CTIT*100

      CTIT='Instruction Alphabet'
      CT1='{M2}C{M4}(x) ='//
     *    ' {M3P}v{M4}e{E}-t{R}t{E}x-1{R}dt{GDH0.4F-1}0{U1.4M3F3}l'

      CT2='lim{GDHC}x{M3CD1.1}a{C}l{RM}'//
     *    '  (1 + {PUH} 1 {RVGD0.5H} x {R}){U1.2H}x{R} = e'

      CT0='Character{H0.5} height{RZ-30}  incli{Z30}nation {ZW0.5} '//
     *    ' ratio {WK} fixed width'

      CT3='Underscoring{L}    {P}twice{J}    vectors {PA8V}'
      NY=1100

      CALL SETPAG('DA4L')
      CALL DISINI
      CALL PAGERA
      CALL COMPLX
      CALL COLOR('CYAN')

      CALL HEIGHT(50)
      NL=NLMESS(CTIT)
      CALL MESSAG(CTIT,(2970-NL)/2,250)

      CALL HEIGHT(36)
      CALL MESSAG('1.)',300,450)
      CALL MESSAG(CT0,500,450)

      CALL HEIGHT(50)
      CALL SMXALF('INST','{','}',1)
      CALL MESSAG(CT0,500,550)

      CALL RESET('SMXA')
      CALL HEIGHT(36)
      CALL MESSAG('2.)',300,750)
      CALL MESSAG(CT3,500,750)

      CALL HEIGHT(50)
      CALL SMXALF('INST','{','}',1)
      CALL MESSAG(CT3,500,850)

      CALL RESET('SMXALF')
      CALL HEIGHT(36)
      CALL MESSAG('3.)',300,NY)
      CALL MESSAG(CT1,500,NY)

      CALL SMXALF('INST','{','}',1)
      CALL HEIGHT(80)
      CALL MESSAG(CT1,900,NY+150)

      CALL HEIGHT(36)
      CALL RESET('SMXA')
      CALL MESSAG('4.)',300,NY+450)
      CALL MESSAG(CT2,500,NY+450)

      CALL HEIGHT(80)
      CALL SMXALF('INST','{','}',1)
      CALL MESSAG(CT2,900,NY+600)
      CALL DISFIN
      END

      SUBROUTINE EX10_1
      DIMENSION X(9),Y(9),Y1(9),Y2(9),Y3(9)
      CHARACTER CTIT*60,CBUF*24

      DATA  X/1.,2.,3.,4.,5.,6.,7.,8.,9./ Y/9*0./
     *     Y1/1.,1.5,2.5,1.3,2.0,1.2,0.7,1.4,1.1/
     *     Y2/2.,2.7,3.5,2.1,3.2,1.9,2.0,2.3,1.8/
     *     Y3/4.,3.5,4.5,3.7,4.,2.9,3.0,3.2,2.6/

      NYA=2700
      CTIT='Bar Graphs (BARS)'

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT
      CALL TICKS(1,'X')
      CALL INTAX
      CALL AXSLEN(1600,700)
      CALL TITLIN(CTIT,3)

      CALL LEGINI(CBUF,3,8)
      CALL LEGLIN(CBUF,'FIRST',1)
      CALL LEGLIN(CBUF,'SECOND',2)
      CALL LEGLIN(CBUF,'THIRD',3)
      CALL LEGTIT(' ')
      CALL SHDPAT(5)
      DO I=1,3
        IF(I.GT.1) CALL LABELS('NONE','X')
        CALL AXSPOS(300,NYA-(I-1)*800)

        CALL GRAF(0.,10.,0.,1.,0.,5.,0.,1.)

        IF(I.EQ.1) THEN
          CALL BARGRP(3,0.15)
          CALL COLOR('RED')
          CALL BARS(X,Y,Y1,9)
          CALL COLOR('GREEN')
          CALL BARS(X,Y,Y2,9)
          CALL COLOR('BLUE')
          CALL BARS(X,Y,Y3,9)
          CALL COLOR('FORE')
          CALL RESET('BARGRP')
        ELSE IF(I.EQ.2) THEN
          CALL HEIGHT(30)
          CALL LABELS('DELTA','BARS')
          CALL LABPOS('CENTER','BARS')
          CALL COLOR('RED')
          CALL BARS(X,Y,Y1,9)
          CALL COLOR('GREEN')
          CALL BARS(X,Y1,Y2,9)
          CALL COLOR('BLUE')
          CALL BARS(X,Y2,Y3,9)
          CALL COLOR('FORE')
          CALL RESET('HEIGHT')
        ELSE IF(I.EQ.3) THEN
          CALL LABELS('SECOND','BARS')
          CALL LABPOS('OUTSIDE','BARS')
          CALL COLOR('RED')
          CALL BARS(X,Y,Y1,9)
          CALL COLOR('FORE')
        END IF

        IF(I.NE.3) CALL LEGEND(CBUF,7)

        IF(I.EQ.3) THEN
          CALL HEIGHT(50)
          CALL TITLE
        END IF

        CALL ENDGRF
      END DO

      CALL DISFIN
      END

      SUBROUTINE EX10_2
      DIMENSION XRAY(5)
      CHARACTER CTIT*60,CBUF*40
      DATA XRAY/1.,2.5,2.,2.7,1.8/

      CTIT='Pie Charts (PIEGRF)'
      NYA=2800

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT
      CALL AXSLEN(1600,1000)
      CALL TITLIN(CTIT,2)
      CALL CHNPIE('BOTH')

      CALL LEGINI(CBUF,5,8)
      CALL LEGLIN(CBUF,'FIRST',1)
      CALL LEGLIN(CBUF,'SECOND',2)
      CALL LEGLIN(CBUF,'THIRD',3)
      CALL LEGLIN(CBUF,'FOURTH',4)
      CALL LEGLIN(CBUF,'FIFTH',5)

C     INDIVIDUELLE SCHRAFFUREN AUSWAEHLEN
      CALL PATCYC(1,7)
      CALL PATCYC(2,4)
      CALL PATCYC(3,13)
      CALL PATCYC(4,3)
      CALL PATCYC(5,5)

      DO I=1,2
        CALL AXSPOS(250,NYA-(I-1)*1200)
        IF(I.EQ.2) THEN
          CALL LABELS('DATA','PIE')
          CALL LABPOS('EXTERNAL','PIE')
        END IF

        CALL PIEGRF(CBUF,1,XRAY,5)

        IF(I.EQ.2) THEN
          CALL HEIGHT(50)
          CALL TITLE
        END IF
        CALL ENDGRF
      END DO
      CALL DISFIN
      END

      SUBROUTINE EX10_3
      CHARACTER CBUF*80
      REAL XRAY(5),Y1RAY(5),Y2RAY(5)
      INTEGER IC1RAY(5),IC2RAY(5)
      DATA XRAY/2.,4.,6.,8.,10./,Y1RAY/0.,0.,0.,0.,0./,
     *     Y2RAY/3.2,1.5,2.0,1.0,3.0/
      DATA IC1RAY/50,150,100,200,175/,
     *     IC2RAY/50,150,100,200,175/

      CALL SETPAG('DA4P')
      CALL DISINI
      CALL PAGERA
      CALL HWFONT

      CALL TITLIN('3-D Bar Graph / 3-D Pie Chart', 2)
      CALL HTITLE(40)

      CALL SHDPAT(16)
      CALL AXSLEN(1500,1000)
      CALL AXSPOS(300,1400)

      CALL BARWTH(0.5)
      CALL BARTYP('3DVERT')
      CALL LABELS('SECOND','BARS')
      CALL LABPOS('OUTSIDE','BARS')
      CALL LABCLR(255,'BARS')
      CALL GRAF(0.,12.,0.,2.,0.,5.,0.,1.)
      CALL TITLE
      CALL COLOR('RED')
      CALL BARS(XRAY,Y1RAY,Y2RAY,5)
      CALL ENDGRF

      CALL SHDPAT(16)
      CALL LABELS('DATA','PIE')
      CALL LABCLR(255,'PIE')
      CALL CHNPIE('NONE')
      CALL PIECLR(IC1RAY,IC2RAY,5)
      CALL PIETYP('3D')
      CALL AXSPOS(300,2700)
      CALL PIEGRF(CBUF,0,Y2RAY,5)       
      CALL DISFIN
      END

      SUBROUTINE EX11_1
      PARAMETER (N=100)
      DIMENSION ZMAT(N,N)
      CHARACTER CDEV*4
     
      FPI=3.1415927/180.
      STEP=360./(N-1)
      DO I=1,N
        X=(I-1.)*STEP
        DO J=1,N
          Y=(J-1.)*STEP
          ZMAT(I,J)=2*SIN(X*FPI)*SIN(Y*FPI)
        END DO
      END DO
      
      CALL DISINI
      CALL PAGERA
      CALL GETMFL(CDEV)
      CALL HWFONT

      CALL TITLIN('3-D Colour Plot of the Function',2)
      CALL TITLIN('F(X,Y) = 2 * SIN(X) * SIN(Y)',4)

      CALL NAME('X-axis','X')
      CALL NAME('Y-axis','Y')
      CALL NAME('Z-axis','Z')

      CALL INTAX
      CALL AUTRES(N,N)
      CALL AXSPOS(300,1850)
      CALL AX3LEN(2200,1400,1400)

      CALL GRAF3(0.,360.,0.,90.,0.,360.,0.,90.,-2.,2.,-2.,1.)
      CALL CRVMAT(ZMAT,N,N,1,1)

      CALL HEIGHT(50)
      IF(CDEV.EQ.'POST') CALL PSFONT('Palatino-BoldItalic')
      CALL TITLE
      CALL MPAEPL(3)
      CALL DISFIN
      END  

      SUBROUTINE EXA_13
      PARAMETER (N=7)
      CHARACTER CLR(N)*4,CONT(N)*4
      DATA CLR/'FORE','RED ','GREE','ORAN','YELL','CYAN','BLUE'/
     *    CONT/'ANTA','AFRI','EURA','NORT','SOUT','AUST','LAKE'/

      CALL DISINI
      CALL HWFONT
      CALL SETVLT('SMALL')
      CALL PAGERA

      CALL PROJCT('HAMMER')
      CALL NOCLIP
      CALL GRAFMP(-180.,180.,-180.,30.,-90.,90.,-90.,30.)

      CALL SHDPAT(16)
      DO I=1,N
        CALL COLOR(CLR(I))
        CALL SHDMAP(CONT(I))
      END DO

      CALL COLOR('FORE')
      CALL GRIDMP(1,1)
        
      CALL DISFIN
      END

      SUBROUTINE EXA_14
      CHARACTER CVLT*4
      DATA NB,NH,NCOL/100,80,0/

      WRITE(6,'(A,$)') ' Give colour table (RAIN, SPEC, GREY): '
      READ(5,'(A)') CVLT

      CALL DISINI
      CALL PAGERA
      CALL SETVLT(CVLT)

      CALL HEIGHT(30)
      DO I=1,13
        NY=I*150
        NX=-50
        DO J=1,20
          NX=NX+145
          IF(NCOL.LE.255) THEN
            CALL POINT(NX,NY,NB,NH,NCOL)
            XCOL=NCOL
            NL=NLNUMB(XCOL,-1)
            CALL SETCLR(255)
            CALL NUMBER(XCOL,-1,NX-NL/2,NY+50)
            NCOL=NCOL+1
          END IF
        END DO
      END DO
      CALL DISFIN
      END
