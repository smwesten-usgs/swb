      MODULE MYGLOB
        IMPLICIT NONE
        INTEGER, PARAMETER :: NPROJ=14
        INTEGER :: ID_LIS,ID_DRAW
        CHARACTER (LEN=512) :: CLIS
      END MODULE MYGLOB

      PROGRAM MAPPLT
      USE DISLIN
      USE MYGLOB
      IMPLICIT NONE

      INTERFACE
        SUBROUTINE MYPLOT (ID)
          IMPLICIT NONE
          INTEGER, INTENT (IN) :: ID
        END SUBROUTINE MYPLOT
      END INTERFACE
 
      INTEGER :: I,N,IP,IP1,IP2,ID_BUT

      CHARACTER (LEN=40), DIMENSION (NPROJ) :: &
       CL1 = (/'Cylindrical Equidistant', &
               'Mercator               ', &
               'Cylindrical Equal-Area ', &
               'Hammer (Elliptical)    ', &
               'Aitoff (Elliptical)    ', &
               'Winkel (Elliptical)    ', &
               'Sanson (Elliptical)    ', &
               'Conical Equidistant    ', &
               'Conical Equal-Area     ', &
               'Conical Conformal      ', &
               'Azimuthal Equidistant  ', & 
               'Azimuthal Equal-Area   ', &
               'Azimuthal Stereographic', &
               'Azimuthal Orthgraphic  '/)

      CLIS=CL1(1)
      DO I=2,NPROJ
        N=TRMLEN(CLIS)
        CLIS(N+1:N+1)='|'
        CLIS(N+2:)=CL1(I)
      END DO

      CALL SWGTIT('DISLIN Map Plot')
      CALL WGINI('HORI',IP)
      CALL SWGWTH(-15)
      CALL WGBAS(IP,'VERT',IP1)
      CALL SWGWTH(-50)
      CALL WGBAS(IP,'VERT',IP2)

      CALL SWGDRW(2100./2970.)
      CALL WGLAB(IP1, 'Projection:',I)
      CALL WGLIS(IP1,CLIS,1,ID_LIS)

      CALL WGPBUT(IP1,'Plot',ID_BUT)
      CALL SWGCBK(ID_BUT,MYPLOT)

      CALL WGQUIT(IP1,I)
      CALL WGLAB(IP2,'DISLIN Draw Widget:',I)
      CALL WGDRAW(IP2,ID_DRAW)
      CALL WGFIN()
      STOP
      END PROGRAM MAPPLT

      SUBROUTINE MYPLOT (ID)
      USE DISLIN
      USE MYGLOB
      IMPLICIT NONE
      INTEGER, INTENT (IN) :: ID
      CHARACTER (LEN=80) :: CTITLE

      CHARACTER (LEN=4), DIMENSION (NPROJ) :: &
        CL2 = (/'CYLI', 'MERC', 'EQUA', 'HAMM', 'AITO', 'WINK', &
               'SANS', 'CONI', 'ALBE', 'CONF', 'AZIM', 'LAMB', &
               'STER', 'ORTH'/)

      REAL :: XA,XE,XOR,XSTP,YA,YE,YOR,YSTP
      INTEGER :: ISEL,N

      N=ID
      XA=-180.
      XE=180.
      XOR=-180.
      XSTP=90.

      YA=-90.
      YE=90.
      YOR=-90.
      YSTP=30.

      CALL GWGLIS(ID_LIS,ISEL)  
      CALL SETXID(ID_DRAW,'WIDGET')
      CALL METAFL('XWIN')
      CALL DISINI()
      CALL ERASE()
      CALL HWFONT()

      IF((ISEL.GE.4).AND.(ISEL.LE.7)) THEN 
        CALL NOCLIP()
      ELSE IF(ISEL.EQ.2) THEN
        YA=-85.
        YE=85.
        YOR=-60.
      ELSE IF((ISEL.GE.8).AND.(ISEL.LE.10)) THEN
        YA=0.
        YE=90.
        YOR=0.
      END IF

      CALL LABDIG(-1,'XY')
      CALL NAME('Longitude','X')
      CALL NAME('Latitude','Y')

      CALL PROJCT(CL2(ISEL))
      CALL HTITLE(50)
      CALL ITMSTR(CLIS,ISEL,CTITLE)
      N=TRMLEN(CTITLE)
      CALL TITLIN(CTITLE(1:N)//' Projection',3)

      CALL GRAFMP(XA,XE,XOR,XSTP,YA,YE,YOR,YSTP)
      CALL TITLE()
      CALL GRIDMP(1,1)
      CALL COLOR('GREEN')
      CALL WORLD()
      CALL ERRMOD('PROTOCOL','OFF')
      CALL DISFIN()
      RETURN
      END SUBROUTINE MYPLOT
