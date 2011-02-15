/***************************************************************/
/**                         E X A _ C                         **/
/**                                                           **/
/** This is the C examples program for the data plotting      **/
/** library DISLIN.                                           **/
/**                                                           **/
/** Version: 10.0 / Windows                                   **/
/***************************************************************/

#include <stdio.h>
#include <math.h>                  
#include <string.h>
#include "dislin.h"

void     widget (void);
void     mysub (int id);
void     exa (char *cdev, int iopt);
void     exa_1  (void);
void     exa_2  (void);
void     exa_3  (void);
void     exa_4  (void);
void     exa_5  (void);
void     exa_6  (void);
void     exa_7  (void);
void     exa_8  (void);
void     exa_9  (void);
void     exa_10 (void);
void     exa_11 (void);
void     exa12_1 (void);
void     exa_12 (void);
void     exa12_2 (void);
void     exa_13 (void);
void     exa_14 (void);
void     ex6_1  (void);
void     ex10_1 (void);
void     ex10_2 (void);
void     ex10_3 (void);
void     ex11_1 (void);
void     ex13_1 (void);
void     ex13_2 (void);
void     ex13_3 (void);
void     ex13_4 (void);
void     ex14_1 (void);
void     ex14_2 (void);
  
float zfun(float x, float y, int iopt);
                      
static float xray[300], yray[300], y1ray[300], y2ray[300], zmat[50][50];
static char  cl1[450], cl2[450];
static int id_lis1, id_lis2, id_pbut;

int main()
{                                                
  widget ();
  return 0;
}

void widget ()
{ int ip, ip_l, ip_r, idev = 1, iopt = 1;
  char chelp[200];
  	
  strcpy (chelp, "C Demo program for the graphics library DISLIN.|");
  strcat (chelp, "Author: H. Michels, Max-Planck-Institut fuer Aeronomie,|");
  strcat (chelp, "           D-37191 Katlenburg-Lindau, Germany");

  strcpy (cl1, "CONS   (X Window Screen)|");
  strcat (cl1, "XWIN   (Small Window)|");
  strcat (cl1, "GL       (OpenGL Window)|");
  strcat (cl1, "GKSL   (GKSLIN Metafile)|");
  strcat (cl1, "CGM    (CGM Metafile)|");
  strcat (cl1, "PS       (PostScript File)|");
  strcat (cl1, "EPS     (EPS File)|");
  strcat (cl1, "PDF     (PDF File)|");
  strcat (cl1, "WMF    (Windows Metafile)|");
  strcat (cl1, "HPGL   (HPGL Plotfile)|");
  strcat (cl1, "SVG     (SVG File)|");
  strcat (cl1, "JAVA   (Java Applet File)|");
  strcat (cl1, "GIF     (GIF File)|");
  strcat (cl1, "TIFF   (TIFF File)|");
  strcat (cl1, "PNG    (PNG File)|");
  strcat (cl1, "PPM    (PPM File)|");
  strcat (cl1, "BMP    (BMP File)|");
  strcat (cl1, "IMAG   (Image File)");

  strcpy (cl2, "Demo of CURVE|Polar Plots|Symbols|Logarithmic Axes|");
  strcat (cl2, "Interpolation Methods|Line Styles|Legend|");
  strcat (cl2, "Shading Patterns (AREAF)|Vectors|");
  strcat (cl2, "Shading Patterns (PIEGRF)|Surface Plot|");
  strcat (cl2, "Surface Plot|Parametric Function Plot|");
  strcat (cl2, "Map Plot|World Coastlines|");
  strcat (cl2, "Elliptical Proj.|Azimuthal Proj.|Conical Proj.|");
  strcat (cl2, "Bar Graphs|Pie Charts|3-D Bar Graph / Pie Chart|");
  strcat (cl2, "Contours|Shaded Contours|");
  strcat (cl2, "Instruction Alphabet|3-D Colour Plot|Shaded World|");
  strcat (cl2, "Colour Tables");

  swgtit ("DISLIN Examples");
  swghlp (chelp);
  swgwth (30);
  swgtyp ("SCROLL", "LIST");
  swgpop ("NOOK");
  
  ip = wgini ("hori");
  ip_l = wgbas (ip, "vert");
  ip_r = wgbas (ip, "vert");

  wglab (ip_l, "Example:");
  id_lis1 = wglis (ip_l, cl2, iopt);
  
  wglab (ip_r, "Device:");
  id_lis2 = wglis (ip_r, cl1, idev);
  
  id_pbut = wgpbut (ip_r, "Plot");
  swgcbk (id_pbut, mysub);

  wgquit (ip_l);
  wgfin ();
}  

void mysub (int id)
{ int iopt, idev;
  char cdev[5], *cstr;

  if (getlev () != 0) return;
  if (id != id_pbut) return;
  iopt = gwglis (id_lis1);
  idev = gwglis (id_lis2);

  cstr = itmstr (cl1, idev);
  strncpy (cdev, cstr, 4);
  cdev[4] = '\0';
  
  exa (cdev, iopt);
  return;
}
 
void exa (char *cdev, int iopt)
{
  metafl(cdev);
  setpag("da4l");
  
  switch (iopt)
  { case  1: exa_1();
             break;
    case  2: exa_2();
             break;
    case  3: exa_3();
             break;
    case  4: exa_4();
             break;
    case  5: exa_5();
             break;
    case  6: exa_6();
             break;
    case  7: exa_7();
             break;
    case  8: exa_8();
             break;
    case  9: exa_9();
             break;
    case 10: exa_10();
             break;
    case 11: exa_11();
             break;
    case 12: exa12_1();
             break;
    case 13: exa12_2();
             break;
    case 14: exa_12();
             break;
    case 15: ex13_1();
             break;
    case 16: ex13_2();
             break;
    case 17: ex13_3();
             break;
    case 18: ex13_4();
             break;
    case 19: ex10_1();
             break;
    case 20: ex10_2();
             break;
    case 21: ex10_3();
             break;
    case 22: ex14_1();
             break;
    case 23: ex14_2();
             break;
    case 24: ex6_1();
             break;
    case 25: ex11_1();
             break;
    case 26: exa_13();
             break;
    case 27: exa_14();
             break;
  }
}

/* >>>>>>>>>> EXA_1  <<<<<<<<<< */
void exa_1()
{ int n = 100, i;
  double fpi = 3.1415926/180., step, x;

  step = 360. / (n-1);

  for (i = 0; i < n; i++)
  { xray[i] = (float) (i * step);
    x = xray[i] * fpi;
    y1ray[i] = (float) sin(x);
    y2ray[i] = (float) cos(x);
  }

  disini();
  pagera();
  hwfont();
  axspos(450,1800);
  axslen(2200,1200);

  name("X-axis","x");
  name("Y-axis","y");

  labdig(-1,"x");
  ticks(10,"xy");

  titlin("Demonstration of CURVE",1);
  titlin("SIN(X), COS(X)",3);

  graf(0.f, 360.f, 0.f, 90.f, -1.f, 1.f, -1.f, 0.5f);
  title();

  color("red");
  curve(xray,y1ray,n);
  color("green");
  curve(xray,y2ray,n);

  color("fore");
  dash();
  xaxgit();
             
  disfin();
}

/* >>>>>>>>>> EXA_2  <<<<<<<<<< */
void exa_2()
{ int n = 300, m = 10, i;
  double f = 3.1415926/180., step, a;
  float x2[10], y2[10];
  
  step = 360. / (n-1);

  for (i = 0; i < n; i++)
  { a = i * step * f;
    yray[i] = (float) a;
    xray[i] = (float) sin(5 * a);
  }

  for (i = 0; i < m; i++)
  { x2[i] = i + 1;
    y2[i] = i + 1;
  }

  setpag ("da4p");
  disini ();
  pagera ();
  hwfont ();
  axspos (450,1800);

  titlin ("Polar Plots", 2);
  ticks  (3, "Y");
  axends ("NOENDS", "X");
  labdig (-1, "Y");
  axslen (1000, 1000);
  axsorg (1050, 900);

  grafp  (1.f ,0.f, 0.2f, 0.f, 30.f);
  curve  (xray, yray, n);
  htitle (50);
  title  ();
  endgrf ();

  labdig (-1, "X");
  axsorg (1050, 2250);
  labtyp ("VERT", "Y");
  grafp  (10.f, 0.f, 2.f, 0.f, 30.f);
  barwth (-5.f);
  polcrv ("FBARS");
  curve  (x2, y2, m);
             
  disfin();
}

/* >>>>>>>>>> EXA_3  <<<<<<<<<< */
void exa_3()
{ int nl, ny, i, nxp = 0;
  static char ctit[] = "Symbols", cstr[3];

  setpag("da4p");
  disini();
  pagera();
  color("yellow");
  hwfont();

  height(60);

  nl = nlmess(ctit);
  messag(ctit,(2100-nl)/2,200);

  height(50);
  hsymbl(120);

  ny = 150;

  for (i = 0; i < 24; i++)
  { if ((i % 4) == 0) 
    { ny  += 400;
      nxp  = 550;
    }
    else
    { nxp += 350;
    }

    sprintf(cstr,"%d",i); 
    nl = nlmess(cstr)/2;
    messag(cstr,nxp-nl,ny+150);
    symbol(i,nxp,ny);
  }

  disfin();
}

/* >>>>>>>>>> EXA_4  <<<<<<<<<< */
void exa_4()
{ int nya, i;
  static char    ctit[] =  "Logarithmic Scaling", cstr[60],
                *clab[3] = {"LOG", "FLOAT", "ELOG"};

  setpag("da4p");
  disini();
  pagera();
  hwfont();

  axslen(1400,500);
  name("X-axis","x");
  name("Y-axis","y");
  axsscl("log","xy");

  titlin(ctit,2);

  for (i = 1; i <= 3; i++)
  { nya = 2650 - (i-1) * 800;
    labdig(-1,"xy");
    if (i == 2)
    { labdig(1,"y");
      name(" ","x");
    }

    axspos(500,nya);
    color("yellow");
    strcpy(cstr, "Labels: ");
    strcat(cstr,clab[i-1]);
    messag(cstr,600,nya-400);
    color("fore");
    labels(clab[i-1],"xy");
    graf(0.f, 3.f, 0.f, 1.f, -1.f, 2.f, -1.f, 1.f);

    if (i == 3)
    { height(50);
      title();
    }

    endgrf();
  }
  disfin();
}

/* >>>>>>>>>> EXA_5  <<<<<<<<<< */
void exa_5()
{ int nya = 2700, i, nx, ny;
  static float
        x[] = { 0.0f,  1.0f,  3.0f,  4.5f,  6.0f,  8.0f,  9.0f, 11.0f, 12.0f,
               12.5f, 13.0f, 15.0f, 16.0f, 17.0f, 19.0f, 20.0f},
        y[] = { 2.0f,  4.0f,  4.5f,  3.0f,  1.0f,  7.0f,  2.0f,  3.0f,  5.0f,
                2.0f,  2.5f,  2.0f,  4.0f,  6.0f,  5.5f,  4.0f};
  static char 
       *cpol[6] = {"SPLINE", "STEM", "BARS", "STAIRS", "STEP", "LINEAR"},
       *ctit    = "Interpolation Methods";

  setpag("da4p");
  disini();
  hwfont();
  pagera();
  incmrk(1);
  hsymbl(25);
  titlin(ctit,1);
  axslen(1500,350);
  setgrf("line","line","line","line");

  for (i=0; i<6; i++)
  { axspos(350,nya-i*350);
    polcrv(cpol[i]);
    marker(0);

    graf(0.f, 20.f, 0.f, 5.f, 0.f, 10.f, 0.f, 5.f);
    nx=nxposn(1.f);
    ny=nyposn(8.f);
    color("yellow");
    messag(cpol[i],nx,ny);
    curve(x,y,16);
    color("fore");

    if (i == 5)
    { height(50);
      title();
    }
    endgrf();
  }

  disfin();
}

/* >>>>>>>>>> EXA_6  <<<<<<<<<< */
void exa_6()
{ int i, ny, nx;
  static float    x[2] = {3.f, 9.f}, y[2];
  static char *ctyp[8] = {"SOLID",  "DOT",   "DASH", "CHNDSH",
                          "CHNDOT", "DASHM", "DOTL", "DASHL"};

  setpag("da4p");
  disini();
  setvlt("small");
  pagera();
  center();
  chncrv("both");
  hwfont();

  name("X-axis","x");
  name("Y-axis","y");

  titlin("Demonstration of Curve",1);
  titlin("Line Styles",3);

  graf(0.f, 10.f, 0.f, 2.f, 0.f, 10.f, 0.f, 2.f);
  title();

  for (i = 1; i <= 8; i++)
  { y[0] = 9.5f - i;
    y[1] = 9.5f - i;
    ny = nyposn(y[0]);
    nx = nxposn(1.0f);
    messag(ctyp[i-1],nx,ny-20);
    curve(x,y,2);
  }

  disfin();
}

/* >>>>>>>>>> EXA_7  <<<<<<<<<< */
void exa_7()
{ int n = 100, i, nx, ny;
  double fpi = 3.1415926/180., step, x;
  char   cbuf[20];

  step = 360./(n-1);
  for (i=0; i<n; i++)
  { xray[i] = (float) (i * step);
    x=xray[i] * fpi;
    y1ray[i] = (float) sin(x);
    y2ray[i] = (float) cos(x);
  }

  disini();
  hwfont();
  pagera();

  axspos(450,1800);
  axslen(2200,1200);

  name("X-axis","x");
  name("Y-axis","y");
  labdig(-1,"x");
  ticks(10,"xy");

  titlin("Demonstration of Curve",1);
  titlin("Legend",3);

  graf(0.f, 360.f, 0.f, 90.f, -1.f, 1.f, -1.f, 0.5f);
  title();
  xaxgit();

  chncrv("both");
  curve(xray,y1ray,n);
  curve(xray,y2ray,n);

  legini(cbuf,2,7);
  nx = nxposn(190.f);
  ny = nyposn(0.75f);
  legpos(nx,ny);
  leglin(cbuf,"sin (x)",1);
  leglin(cbuf,"cos (x)",2);
  legtit("Legend");
  legend(cbuf,3);

  disfin();
}

/* >>>>>>>>>> EXA_8  <<<<<<<<<< */
void exa_8()
{ int   ixp[4], iyp[4], nl, nx, nx0 = 335, ny0 = 350, ny, i, j, ii, k, iclr;
  static int  ix[4] = {0, 300, 300, 0},
              iy[4] = {0, 0, 400, 400};
  static char *ctit = "Shading Patterns (AREAF)", cstr[3];

  disini();
  setvlt("small");
  pagera();
  hwfont();

  height(50);
  nl = nlmess(ctit);
  nx = (2970-nl)/2;
  messag(ctit,nx,200);

  iclr = 0;
  for (i = 0; i < 3; i++)
  { ny = ny0+i*600;
    for (j = 0; j < 6; j++)
    { nx = nx0+j*400;
      ii = i*6+j;
      shdpat((long) ii);
      sprintf(cstr,"%d",ii);

      iclr = iclr % 8;
      iclr++;
      setclr(iclr);

      for (k=0; k<4; k++)
      { ixp[k] = ix[k] + nx;
        iyp[k] = iy[k] + ny;
      }
      areaf(ixp,iyp,4);

      nl  = nlmess(cstr);
      nx += (300-nl)/2;
      messag(cstr,nx,ny+460);
    }
  }

  disfin();
}

/* >>>>>>>>>> EXA_9  <<<<<<<<<< */
void exa_9()
{ int nl, nx, ny, i;
  static int ivec[20]   = {   0,1111,1311,1421,1531,1701,1911,3111,3311,3421,
                           3531,3703,4221,4302,4413,4522,4701,5312,5502,5703};
  static char *ctit = "Vectors", cnum[5];

  disini();
  color("cyan");
  pagera();
  hwfont();

  height(60);
  nl = nlmess(ctit);
  nx = (2970-nl)/2;
  messag(ctit,nx,200);

  height(50);
  nx = 300;
  ny = 400;

  for (i = 0; i < 20; i++)
  { if (i == 10) 
    { nx += 2970/2;
      ny  = 400;
    }

    sprintf(cnum,"%d",ivec[i]);
    nl = nlmess(cnum);
    messag(cnum,nx-nl,ny-25 );

    vector(nx+100,ny,nx+1000,ny,ivec[i]);
    ny += 160;
  }

  disfin();
}

/* >>>>>>>>>> EXA_10  <<<<<<<<<< */
void exa_10()
{ int i;
  char cbuf[40], cstr[4];

  for (i = 0; i < 18; i++)
    xray[i] = 1.f;

  setpag("da4p");
  disini();
  setvlt("small");
  pagera();
  hwfont();

  axspos(250,2700);
  axslen(1600,2200);
  titlin("Shading Patterns (PIEGRF)",3);
  height(50);

  legini(cbuf,18,2);

  for (i = 0; i < 18; i++)
  { sprintf(cstr,"%d",i);
    leglin(cbuf,cstr,i+1);
  }

  chnpie("both");
  labels("none","pie");
  piegrf(cbuf,1,xray,18);
  title();

  disfin();
}

/* >>>>>>>>>> EXA_11 <<<<<<<<<< */
void exa_11()
{ int n = 50 ,i, j;
  double fpi=3.1415927/180., step, x, y;

  step = 360./(n-1);
  for (i = 0; i < n; i++)
  { x = i*step;
    for (j = 0; j < n; j++)
    { y = j*step;
      zmat[i][j] = (float) (2*sin(x*fpi)*sin(y*fpi));
    }
  }

  setpag("da4p");
  disini();
  pagera();
  hwfont();
  axspos(200,2600);
  axslen(1800,1800);

  name("X-axis","x");
  name("Y-axis","y");
  name("Z-axis","z");

  titlin("Surface Plot (SURMAT)",2);
  titlin("F(X,Y) = 2*SIN(X)*SIN(Y)",4);

  view3d(-5.f, -5.f, 4.f,"abs");
  graf3d(0.f,360.f,0.f,90.f,0.f,360.f,0.f,90.f,-3.f,3.f,-3.f,1.f);
  height(50);
  title();

  color("green");
  surmat((float *) zmat,50,50,1,1);
  disfin();
}

/* >>>>>>>>>> EXA12_1 <<<<<<<<<< */
void exa12_1()
{ int n = 50 ,i, j;
  static int ixp[4] = {200,1999,1999,200},
             iyp[4] = {2600,2600,801,801};

  double fpi = 3.1415927/180., step, x, y;

  step = 360./(n-1);
  for (i = 0; i < n; i++)
  { x = i*step;
    for (j = 0; j < n; j++)
    { y = j*step;
      zmat[i][j] = (float) (2*sin(x*fpi)*sin(y*fpi));
    }
  }

  setpag("da4p");
  disini();
  pagera();
  hwfont();

  axspos(200,2600);
  axslen(1800,1800);

  name("X-axis","x");
  name("Y-axis","y");
  name("Z-axis","z");

  titlin("Surface Plot (SURMAT)",2);
  titlin("F(X,Y) = 2*SIN(X)*SIN(Y)",4);

  graf3d(0.f,360.f,0.f,90.f,0.f,360.f,0.f,90.f,-3.f,3.f,-3.f,1.f);
  height(50);
  title();
  shlsur();
  color("green");
  surmat((float *) zmat,n,n,1,1);

  color("fore");
  grfini(-1.f,-1.f,-1.f,1.f,-1.f,-1.f,1.f,1.f,-1.f);
  nograf();
  graf(0.f,360.f,0.f,90.f,0.f,360.f,0.f,90.f);
  dashl();
  grid(1,1);
  grffin();

  grfini(-1.f,-1.f,-1.f,-1.f,1.f,-1.f,-1.f,1.f,1.f);
  graf(0.f,360.f,0.f,90.f,-3.f,3.f,-3.f,1.f);
  grid(1,1);
  grffin();

  grfini(-1.f,1.f,-1.f,1.f,1.f,-1.f,1.f,1.f,1.f);
  shdpat(7L);
  solid();
  areaf(ixp,iyp,4);
  grffin();

  disfin();
}

/* >>>>>>>>>> EXA12_2 <<<<<<<<<< */
void exa12_2()

{ float p = 3.14159f, step;
  
   setpag("da4p");
   disini();
   pagera();
   hwfont();
   axspos (200,2400);
   axslen(1800,1800);
   intax();

   titlin("Surface Plot of the Parametric Function",2);
   titlin("[COS(t)+(3+COS(u)), SIN(t)*(3+COS(u)), SIN(u)]",4);

   name("X-axis", "x");
   name("Y-axis", "y");
   name("Z-axis", "z");

   vkytit(-300);
   zscale(-1.f,1.f);
   graf3d (-4.f,4.f,-4.f,1.f,-4.f,4.f,-4.f,1.f,
	       -3.f,3.f,-3.f,1.f);
   height(40);
   title();

   surmsh("on");
   step = (float) (2 * 3.14159/30.);
   surfcp (zfun, 0.f,2*p,step,0.f,2*p,step);
   disfin();
}

float zfun(float x, float y, int iopt)
{ double v;
  if (iopt == 1)
    v = cos((double) x) * (3.+cos((double) y));
  else if (iopt == 2)
    v = sin((double) x) * (3.+cos((double) y));
  else
    v = sin ((double) y);

  return (float) v;
}

/* >>>>>>>>>> EXA_12 <<<<<<<<<< */
void exa_12()
{ int i, nxp, nyp;
  float  xp, yp;
  static float   xc[9] = {-22.f,18.f,37.5f,0.f,2.5f,12.5f,23.5f,-3.75f,14.25f},
                 yc[9] = {64.f,59.6f,56.f,51.5f,48.5f,42.f,38.f,40.3f,50.1f};
  static char *cstr[9] = {"REYKJAVIK", "STOCKHOLM", "MOSKAU", "LONDON",
                          "PARIS", "ROM", "ATHEN", "MADRID", "PRAG"};

  disini();
  pagera();
  hwfont();

  axspos(500,1850);
  axslen(2200,1400);

  labdig(-1,"xy");
  ticks(1,"xy");
  name("Longitude","x");
  name("Latitude","y");

  titlin("Map Plot",3);
  incmrk(-1);

  labels("map","xy");
  projct("lambert");
  frame(3);
  grafmp(-40.f, 60.f, -40.f, 20.f, 35.f, 70.f, 40.f, 10.f);

  color ("green");
  world();
  color ("fore");
  curvmp(xc,yc,9);

  for (i = 0; i < 9; i++)
  { pos2pt(xc[i],yc[i],&xp,&yp);
    nxp = (int) (xp + 30);
    nyp = (int) yp;
    messag(cstr[i],nxp,nyp);
  }

  gridmp(1,1);
  height(50);
  title();
  disfin();
}

/* >>>>>>>>>> EX13_1 <<<<<<<<<< */
void ex13_1()
{ setpag("da4l");
  disini();
  pagera();
  hwfont();

  frame(3);
  axspos(400,1850);
  axslen(2400,1400);

  name("Longitude","x");
  name("Latitude","y");
  titlin("World Coastlines and Lakes",3);

  labels("map","xy");
  grafmp(-180.f, 180.f, -180.f, 90.f, -90.f, 90.f, -90.f, 30.f);

  gridmp(1,1);
  color("green");
  world();
  color("fore");

  height(50);
  title();
  disfin();
}

/* >>>>>>>>>> EX13_2 <<<<<<<<<< */
void ex13_2()
{ int nya, i;
  static char *cproj[3] = {"Sanson","Winkel","Hammer"};
  char   ctit[61];

  setpag("da4p");
  disini();
  hwfont();
  pagera();

  height(40);
  axslen(1600,750);

  nya=3850;
  for (i=0; i<3; i++)
  { nya=nya-950;
    axspos(250,nya);

    projct(cproj[i]);
    noclip();
    grafmp(-180.f, 180.f, -180.f, 30.f, -90.f, 90.f, -90.f, 15.f);

    sprintf(ctit,"%s%s","Elliptical Projection of ",cproj[i]);
    titlin(ctit,4);
    title();
    color("green");
    world();
    color("fore");

    gridmp(1,1);
    endgrf();
  }

  disfin();
}

/* >>>>>>>>>> EX13_3 <<<<<<<<<< */
void ex13_3()
{ int    nl, nx, i;
  static int   nxa[4]  = {200,1150,200,1150},
               nya[4]  = {1600,1600,2700,2700};
  static float xpol[4] = {0.f,0.f,0.f,0.f},
               ypol[4] = {0.f,45.f,90.f,-45.f};
  static char  *ctit   = "Azimuthal Lambert Projections";

  setpag("da4p");
  disini();
  hwfont();
  pagera();

  height(50);
  nl = nlmess(ctit);
  nx = (2250-nl)/2;
  messag(ctit,nx,300);

  axslen(900,900);
  projct("lambert");

  for(i = 0; i < 4; i++)
  { axspos(nxa[i],nya[i]);
    mappol(xpol[i],ypol[i]);
    grafmp(-180.f,180.f,-180.f,30.f,-90.f,90.f,-90.f,30.f);

    color("green");
    world();
    color("fore");
    gridmp(1,1);
    endgrf();
  }

  disfin();
}

/* >>>>>>>>>> EX13_4 <<<<<<<<<< */
void ex13_4()
{ int n = 32, inray[32], icray[32], i;
  long ipray[32];

  for (i=0; i<32; i++)
  { inray[i] = i+1;
    ipray[i] = 0;
    icray[i] = 1;
  }

  setpag("da4p");
  disini();
  setvlt("small");
  pagera();
  hwfont();

  intax();
  ticks(1,"xy");
  frame(3);
  axslen(1600,2200);
  axspos(400,2700);

  name("Longitude","x");
  name("Latitude","y");
  titlin("Conformal Conic Projection",3);

  labels("map","xy");
  projct("conf");
  grafmp(-10.f, 30.f, -10.f, 5.f, 35.f, 70.f, 35.f, 5.f);

  gridmp(1,1);
  shdeur(inray,ipray,icray,n);

  height(50);
  title();
  disfin();
}

/* >>>>>>>>>> EX14_1 <<<<<<<<<< */
void ex14_1()
{ int n = 50, i, j;
  double   fpi = 3.14159/180., step, x, y;
  float  zlev;

  step = 360./(n-1);

  for (i = 0; i < n; i++)
  { xray[i] = (float) (i*step);
    yray[i] = (float) (i*step);
  }

  for (i = 0; i < n; i++)
  { for (j = 0; j < n; j++)
    { x = xray[i]*fpi;
      y = yray[j]*fpi;    
      zmat[i][j] = (float) (2*sin(x)*sin(y));
    }
  }

  setpag("da4p");
  disini();
  complx();
  pagera();

  titlin("Contour Plot",1);
  titlin("F(X,Y) = 2 * SIN(X) * SIN(Y)",3);

  name("X-axis","x");
  name("Y-axis","y");

  intax();
  axspos(450,2670);
  graf(0.f, 360.f, 0.f, 90.f, 0.f, 360.f, 0.f, 90.f);

  height(30);
  for (i = 0; i < 9; i++)
  { zlev = (float) (-2.+i*0.5);
    setclr((i+1)*25);
    if (i == 4)
      labels("none","contur"); 
    else
      labels("float","contur");

    contur(xray,n,yray,n,(float *) zmat,zlev);
  }

  height(50);
  color("fore");
  title();
                                                                  
  disfin();
}

/* >>>>>>>>>> EX14_2 <<<<<<<<<< */
void ex14_2()
{ int    n = 50, i, j;
  float zlev[12];
  double step, x, y;

  step = 1.6/(n-1);
  for (i = 0; i < n; i++)
  { x = 0.0+i*step;
    xray[i] = (float) x;
    for (j = 0; j < n; j++)
    { y = 0.0+j*step;
      yray[j] = (float) y;
      zmat[i][j] = (float) ((x*x-1.)*(x*x-1.) + (y*y-1.)*(y*y-1.));
    }
  }

  setpag("da4p");
  disini();
  pagera();
  hwfont();

  mixalf();
  titlin("Shaded Contour Plot",1);
  titlin("F(X,Y) = (X[2$ - 1)[2$ + (Y[2$ - 1)[2$",3);
  name("X-axis","x");
  name("Y-axis","y");

  shdmod("poly", "contur");
  axspos(450,2670);
  graf(0.0f,1.6f,0.0f,0.2f,0.0f,1.6f,0.0f,0.2f);

  for (i = 1; i <= 12; i++)
    zlev[12-i] = (float) (0.1+(i-1)*0.1);

  conshd(xray, n, yray, n, (float *) zmat,zlev,12);

  height(50);
  title();
  disfin();
}

/* >>>>>>>>>> EX6_1  <<<<<<<<<< */
void ex6_1()
{ int ny = 1100, nl;
  static char *ctit = "Instruction Alphabet",
  *ct1 = 
   "{m2}C{m4}(x) = {m3p}v{m4}e{e}-t{r}t{e}x-1{r}dt{gdh0.4f-1}0{u1.4m3f3}l",
  *ct2 =
   "lim{gdhc}x{m3cd1.1}a{c}l{rm} (1 + {puh} 1 {rvgd0.5h} x {r}){u1.2h}x{r} = e",
  *ct0 = 
   "Character{h0.5} height{rz-30}  incli{z30}nation {zw0.5} ratio {wk} fixed width",
  *ct3 = "Underscoring{l}    {p}twice{j}    vectors {pa8v}";

  setpag("da4l");
  disini();
  pagera();
  hwfont();
  color("cyan");

  height(50);
  nl=nlmess(ctit);
  messag(ctit,(2970-nl)/2,250);

  height(36);
  messag("1.)",300,450);
  messag(ct0,500,450);

  height(50);
  smxalf("inst","{","}",1);
  messag(ct0,500,550);

  reset("smxa");
  height(36);
  messag("2.)",300,750);
  messag(ct3,500,750);

  height(50);
  smxalf("inst","{","}",1);
  messag(ct3,500,850);

  reset("smxalf");
  height(36);
  messag("3.)",300,ny);
  messag(ct1,500,ny);

  smxalf("inst","{","}",1);
  height(80);
  messag(ct1,900,ny+150);

  height(36);
  reset("smxa");
  messag("4.)",300,ny+450);
  messag(ct2,500,ny+450);

  height(80);
  smxalf("inst","{","}",1);
  messag(ct2,900,ny+600);
  disfin();
}

/* >>>>>>>>>> EX10_1 <<<<<<<<<< */
void ex10_1()
{ int nya = 2700, i;
  static char   *ctit = "Bar Graphs (BARS)", cbuf[25];

  static float x[9]  = {1.f,2.f,3.f,4.f,5.f,6.f,7.f,8.f,9.f},
               y[9]  = {0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f,0.f},
               y1[9] = {1.f,1.5f,2.5f,1.3f,2.0f,1.2f,0.7f,1.4f,1.1f},
               y2[9] = {2.f,2.7f,3.5f,2.1f,3.2f,1.9f,2.0f,2.3f,1.8f},
               y3[9] = {4.f,3.5f,4.5f,3.7f,4.0f,2.9f,3.0f,3.2f,2.6f};

  setpag("da4p");
  disini();
  pagera();
  hwfont();
  ticks(1,"x");
  intax();;
  axslen(1600,700);
  titlin(ctit,3);

  legini(cbuf,3,8);
  leglin(cbuf,"FIRST",1);
  leglin(cbuf,"SECOND",2);
  leglin(cbuf,"THIRD",3);
  legtit(" ");
  shdpat(5L);
  for (i = 1; i <= 3; i++)
  { if (i >  1) labels("none","x");
    axspos(300,nya-(i-1)*800);

    graf(0.f,10.f,0.f,1.f,0.f,5.f,0.f,1.f);

    if (i == 1)
    { bargrp(3,0.15f);
      color("red");
      bars(x,y,y1,9);
      color("green");
      bars(x,y,y2,9);
      color("blue");
      bars(x,y,y3,9);
      color("fore");
      reset("bargrp");
    }
    else if (i == 2)
    { height(30);
      labels("delta","bars");
      labpos("center","bars");
      color("red");
      bars(x,y,y1,9);
      color("green");
      bars(x,y1,y2,9);
      color("blue");
      bars(x,y2,y3,9);
      color("fore");
      reset("height"); 
    }
    else if (i == 3)
    { labels("second","bars");
      labpos("outside","bars");
      color("red");
      bars(x,y,y1,9);
      color("fore");
    }

    if (i != 3) legend(cbuf,7);

    if (i == 3)
    { height(50);
      title();
    }

    endgrf();
  }

  disfin();
}

/* >>>>>>>>>> EX10_2 <<<<<<<<<< */
void ex10_2()
{ int nya = 2800, i;
  static char    *ctit = "Pie Charts (PIEGRF)", cbuf[41];
  static float xdray[5] = {1.f,2.5f,2.f,2.7f,1.8f};

  setpag("da4p");
  disini();
  pagera();
  hwfont();
  axslen(1600,1000);
  titlin(ctit,2);
  chnpie("both");

  legini(cbuf,5,8);
  leglin(cbuf,"FIRST",1);
  leglin(cbuf,"SECOND",2);
  leglin(cbuf,"THIRD",3);
  leglin(cbuf,"FOURTH",4);
  leglin(cbuf,"FIFTH",5);

  patcyc(1,7L);
  patcyc(2,4L);
  patcyc(3,13L);
  patcyc(4,3L);
  patcyc(5,5L);

  for (i = 0; i < 2; i++)
  { axspos(250,nya-i*1200);
    if (i == 1)
    { labels("data","pie");
      labpos("external","pie");
    }

    piegrf(cbuf,1,xdray,5);

    if (i == 1)
    { height(50);
      title();
    }
    endgrf();
  }
  disfin();
}

/* >>>>>>>>>> EX10_3 <<<<<<<<<< */
void ex10_3()
{ char cbuf[80];
  float x[5]  = {2.,4.,6.,8.,10.},
        y1[5] = {0.,0.,0.,0.,0.},
        y2[5] = {3.2,1.5,2.0,1.0,3.0};

  int ic1ray[5]  = {50,150,100,200,175},
      ic2ray[5]  = {50,150,100,200,175};

  setpag("da4p");
  disini();
  pagera();
  hwfont();

  titlin("3-D Bar Graph / 3-D Pie Chart", 2);
  htitle(40);

  shdpat(16);
  axslen(1500,1000);
  axspos(300,1400);

  barwth(0.5);
  bartyp("3dvert");
  labels("second","bars");
  labpos("outside","bars");
  graf(0.,12.,0.,2.,0.,5.,0.,1.);
  title();
  color("red");
  bars(x,y1,y2,5);
  endgrf();

  shdpat(16);
  labels("data","pie");
  chnpie("none");
  pieclr(ic1ray,ic2ray,5);
  pietyp("3d");
  axspos(300,2700);
  piegrf(cbuf,0,y2,5);       
  disfin();
}

/* >>>>>>>>>> EX11_1 <<<<<<<<<< */
void ex11_1()
{ int n = 50, i, j;
  double   fpi = 3.1415927/180., step, x, y;
  char   *cdev;

  step = 360./(n-1);
  for (i = 0; i < n; i++)
  { x = i*step;
    for (j = 0; j < n; j++)
    { y = j*step;
      zmat[i][j] = (float) (2*sin(x*fpi)*sin(y*fpi));
    }
  }

  disini();
  pagera();
  cdev = getmfl();
  hwfont();

  titlin("3-D Colour Plot of the Function",2);
  titlin("F(X,Y) = 2 * SIN(X) * SIN(Y)",4);

  name("X-axis","x");
  name("Y-axis","y");
  name("Z-axis","z");

  intax();
  autres(n,n);
  axspos(300,1850);
  ax3len(2200,1400,1400);

  graf3(0.f,360.f,0.f,90.f,0.f,360.f,0.f,90.f,-2.f,2.f,-2.f,1.f);
  crvmat((float *) zmat,n,n,1,1);

  height(50);
  if (strcmp(cdev,"POST") == 0)
        psfont("palatino-bolditalic");
  title();
  disfin();
}

/* >>>>>>>>>> EXA_13 <<<<<<<<<< */
void exa_13()
{ int n = 7, i;
  static char *clr[7]  = {"fore","red","yello","cyan","oran","green","blue"},
              *cont[7] = {"anta","afri","sout" ,"aust","nort","eura", "lake"};

  disini();
  hwfont();
  setvlt("small");
  pagera();

  projct("hammer");
  noclip();
  grafmp(-180.f,180.f,-180.f,30.f,-90.f,90.f,-90.f,30.f);

  shdpat(16L);
  for (i = 0; i < n; i++)
  { color(clr[i]); 
    shdmap(cont[i]);                               
    sendbf();
  }

  color("fore");
  gridmp(1,1);

  disfin();
}

/* >>>>>>>>>> EXA_14 <<<<<<<<<< */
void exa_14()
{ int ncol = 0, ny, i, j, nx, nb = 100, nh = 80, nl, ip, id_lis, ilis;
  float xcol;
  char   *cvlt;
  static char clis[] =
      {"SMALL|RAINBOW|SPECTRUM|TEMPERATURE|GREY|RRAIN|RGREY|VGA"};

  swgpop ("NOQUIT");
  swgpop ("NOHELP");

  ip = wgini ("vert");
  ilis = 2;
  wglab (ip, "Colour Table:");
  id_lis = wglis (ip, clis, ilis);
  wgok (ip);
  wgfin ();

  ilis = gwglis (id_lis);
  cvlt = itmstr (clis, ilis);

  disini();
  setvlt(cvlt);
  pagera();

  height(30);
  for (i = 1; i <= 13; i++)
  { ny = i*150;
    nx = -50;
    for (j = 1; j <= 20; j++)
    { nx += 145;
      if (ncol <= 255)
      { point(nx,ny,nb,nh,ncol);
        xcol = (float) ncol;
        nl=nlnumb(xcol,-1);
        color ("fore");  
        number(xcol,-1,nx-nl/2,ny+50);
        ncol++;
      }
    }
  }                   
  disfin();
}

