#include <stdio.h>
#include <string.h>
#include "dislin.h"

#define NPROJ 14

static int id_lis, id_draw, id_but, id_lab1, id_lab2, id_quit;

static char clis[512];
static char *cl1[NPROJ] = {"Cylindrical Equidistant",
                           "Mercator",
                           "Cylindrical Equal-Area",
                           "Hammer (Elliptical)",
                           "Aitoff (Elliptical)",
                           "Winkel (Elliptical)",
                           "Sanson (Elliptical)",
                           "Conical Equidistant",
                           "Conical Equal-Area",
                           "Conical Conformal",
                           "Azimuthal Equidistant",
                           "Azimuthal Equal-Area",
                           "Azimuthal Stereographic",
                           "Azimuthal Orthgraphic"};

static char *cl2[NPROJ] = {"CYLI", "MERC", "EQUA", "HAMM", "AITO", "WINK",
                           "SANS", "CONI", "ALBE", "CONF", "AZIM", "LAMB",
                           "STER", "ORTH"};

void myplot (int id);

int main()
{ int i, ip, ip1, ip2;

  strcpy (clis, cl1[0]);
  for (i = 1; i < NPROJ; i++)
  { strcat (clis, "|");
    strcat (clis, cl1[i]);
  }

  swgtit ("DISLIN Map Plot");
  ip = wgini ("hori");
  swgwth (-15);
  ip1 = wgbas (ip, "vert");
  swgwth (-50);
  ip2 = wgbas (ip, "vert");

  swgdrw ((float) (2100./2970.));
  id_lab1 = wglab (ip1, "Projection:");
  id_lis  = wglis (ip1, clis, 1);

  id_but  = wgpbut (ip1, "Plot");
  swgcbk (id_but, myplot); 

  id_quit = wgquit (ip1);
  id_lab2 = wglab (ip2, "DISLIN Draw Widget:");
  id_draw = wgdraw (ip2);
  wgfin ();
  return 0;
}  

void myplot (int id)
{ char ctitle[80];

  int isel;
  float xa, xe, xor, xstp, ya, ye, yor, ystp;

  if (id != id_but) return;    /* Dummy statement */

  xa = -180.;
  xe = 180.;
  xor = -180.;
  xstp = 60.;

  ya =  -90.; 
  ye =  90.;
  yor =  -90.;
  ystp = 30.;

  isel = gwglis (id_lis);
  setxid (id_draw, "widget");
  metafl ("xwin");
  disini ();
  erase();
  hwfont ();

  if (isel >=4 && isel <= 7) 
    noclip ();
  else if (isel == 2)
  { ya = -85;
    ye = 85;
    yor = -60;
  }
  else if (isel >= 8 && isel <= 10)
  { ya = 0;
    ye = 90;
    yor = 0;
  }

  labdig (-1, "xy");
  name ("Longitude", "x");
  name ("Latitude", "y");

  projct (cl2[isel-1]);
  htitle (50);
  strcpy (ctitle, cl1[isel-1]);
  strcat (ctitle, " Projection");

  titlin (ctitle, 3);
  grafmp (xa, xe, xor, xstp, ya, ye, yor, ystp);
  title ();
  gridmp (1,1);
  color ("green");
  world();
  errmod ("protocol", "off");
  disfin ();
}
