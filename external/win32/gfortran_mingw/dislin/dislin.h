/****************************************************************/
/**                        DISLIN.H                            **/
/**                                                            **/
/** INCLUDE file for DISLIN C routines.                        **/
/**                                                            **/
/** Date     : 08.08.2010                                      **/
/** Functions: 727                                             **/
/** Version  : 10.0                                            **/
/****************************************************************/

#ifdef __cplusplus
  extern "C" {
#endif

void  abs3pt (float x, float y, float z, float *xp, float *yp);
void  addlab (const char *cstr, float v, int itic, const char *cax);
void  angle  (int ngrad);
void  arcell (int nx, int ny, int na, int nb, float a, float b, float t);
void  areaf  (const int *nxray, const int *nyray, int n);
void  autres (int ixdim, int iydim);
void  ax2grf (void);
void  ax3len (int nxl, int nyl, int nzl);
void  axclrs (int nclr, const char *copt, const char *cax);
void  axends (const char *cstr, const char *cax);
void  axgit  (void);
void  axis3d (float x3, float y3, float z3);
void  axsbgd (int nclr);
void  axslen (int nxl, int nyl);
void  axsorg (int nxa, int nya);
void  axspos (int nxa, int nya);
void  axsscl (const char *cscl, const char *cax);
void  axstyp (const char *copt);
void  barbor (int iclr);
void  barclr (int ic1, int ic2, int ic3);
void  bargrp (int ngrp, float gap);
void  barmod (const char *cmod, const char *copt);
void  baropt (float xf, float a);
void  barpos (const char *copt);
void  bars   (float *xray, float *y1ray, float *y2ray, int n);
void  bars3d (const float *xray, const float *yray, const float *z1ray, 
              const float *z2ray, const float *xwray, const float *ywray,
              const int *icray, int n);
void  bartyp (const char *ctyp);
void  barwth (float factor);
void  basalf (const char *calph);
void  basdat (int id, int im, int iy); 
void  bezier (const float *xray, const float *yray, int nray, 
              float *x, float *y, int n);
void  bfcclr (int ic);
void  bfcmsh (int ic);
short bitsi2 (int nbits, short mher, int iher, short mhin, int ihin);
int   bitsi4 (int nbits, int mher,int iher, int mhin, int ihin);
void  bmpfnt (const char *copt);
void  bmpmod (int n, const char *cval, const char *copt);
void  box2d  (void);
void  box3d  (void);
void  bufmod (const char *cmod, const char *ckey);
void  center (void);
void  cgmbgd (float xr, float xg, float xb);
void  cgmpic (const char *cstr);
void  cgmver (int nclr);
void  chaang (float angle);
void  chacod (const char *copt);
void  chaspc (float xspc);
void  chawth (float xwth);
void  chnatt (void);
void  chncrv (const char *copt);
void  chndot (void);
void  chndsh (void);
void  chnbar (const char *copt);
void  chnpie (const char *copt);
void  circ3p (float x1, float y1, float x2, float y2, float x3, float y3,
              float *xm, float *ym, float *r);
void  circle (int nx, int ny, int nr);
void  circsp (int nspc);
void  clip3d (const char *ctyp);
int   closfl (int nu); 
void  clpbor (const char *copt);
void  clpmod (const char *copt);
void  clpwin (int nx, int ny, int nw, int nh);
void  clrcyc (int index, int iclr);
void  clrmod (const char *cmode);
void  clswin (int id);
void  color  (const char *col);
void  colran (int nca, int nce);
void  colray (const float *zray, int *nray, int n);
void  complx (void);
void  conclr (const int *nray, int n);
void  concrv (const float *xray, const float *yray, int n, float zlev);
void  cone3d (float xm, float ym, float zm, float r, 
              float h1, float h2, int nsk1, int nsk2); 
void  confll (const float *xray, const float *yray, const float *zray, int n, 
              const int *i1ray, const int *i2ray, const int *i3ray, int ntri, 
              const float *zlev, int nlev);
void  congap (float xfac);
void  conlab (const char *clab);
void  conmat (const float *zmat, int n, int m, float zlev);
void  conmod (float xfac, float xquot);
void  conn3d (float x, float y, float z);
void  connpt (float x, float y);
void  conpts (const float *xray, int n, const float *yray, int m, 
              const float *zmat, float zlev, float *xpts, float *ypts, 
              int maxpts, int *nray, int maxray, int *nlins);
void  conshd (const float *xray, int n, const float *yray, int m,
              const float *zmat, const float *zlev, int nlev);
void  conshd3d (const float *xray, int n, const float *yray, int m, 
              const float *zmat, const float *zlev, int nlev);
void  contri (const float *xray, const float *yray, const float *zray, int n, 
              const int *i1ray, const int *i2ray, const int *i3ray, 
              int ntri, float zlev);
void  contur (const float *xray, int n, const float *yray, int m,
              const float *zmat, float zlev);
void  cross  (void);
void  crvmat (const float *zmat, int n, int m, int ixpts, int iypts);
void  crvqdr (const float *xray, const float *yray, const float *zray, int n); 
void  crvt3d (const float *xray, const float *yray, const float *zray, 
              const float *rray, const int *icray, int n);
void  crvtri (const float *xray, const float *yray, const float *zray, int n, 
              const int *i1ray, const int *i2ray, const int *i3ray, int ntri);
int   csrkey (void);
void  csrmod (const char *cmod, const char *ckey);
int   csrpos (int *ix, int *iy);
void  csrpt1 (int *ix, int *iy);
void  csrpts (int *ix, int *iy, int nmax, int *n, int *iret);
void  csrmov (int *ix, int *iy, int nmax, int *n, int *iret);
void  csrrec (int *ix1, int *iy1, int *ix2, int *iy2);
void  csrtyp (const char *copt);
void  csruni (const char *copt);
void  curv3d (const float *xray, const float *yray, const float *zray, int n);
void  curve  (const float *xray, const float *yray, int n);
void  curve3 (const float *xray, const float *yray, const float *zray, int n);
void  curvmp (const float *xray, const float *yray, int n);
void  curvx3 (const float *xray, float y, const float *zray, int n);
void  curvy3 (float x, const float *yray, const float *zray, int n);
void  cyli3d (float xm, float ym, float zm, float r, float h, 
              int nsk1, int nsk2); 
void  dash   (void);
void  dashl  (void);
void  dashm  (void);
void  dbffin (void);
int   dbfini (void);
void  dbfmod (const char *copt);
void  delglb (void);
void  digits (int ndig, const char *cax);
void  disalf (void);
void  disenv (const char *cenv);
void  disfin (void);
void  disini (void);
void  disk3d (float xm, float ym, float zm, float r1, float r2, 
              int nsk1, int nsk2); 
void  doevnt (void);
void  dot    (void);
void  dotl   (void);
void  duplx  (void);
int   dwgbut (const char *cstr, int ival);
int   dwgerr (void);
char *dwgfil (const char *clab, const char *cstr, const char *cmask);
int   dwglis (const char *clab, const char *clis, int ilis);
void  dwgmsg (const char *cstr);
char *dwgtxt (const char *clab, const char *cstr);
void  ellips (int nx, int ny, int na, int nb);
void  endgrf (void);
void  erase  (void);
void  errbar (const float *x, const float *y, 
              const float *err1, const float *err2, int n);
void  errdev (const char *cdev);
void  errfil (const char *cfil);
void  errmod (const char *cstr, const char *copt);
void  eushft (const char *cnat, const char *cshf);
void  expzlb (const char *cstr);
void  fbars  (const float *xray, const float *y1ray, const float *y2ray, 
              const float *y3ray, const float *y4ray, int n);
int   fcha   (float x, int ndig, char *cstr);
void  field  (const float *xray, const float *yray,
              const float *uray, const float *vray, int n, int ivec);
void  field3d (const float *x1ray, const float *y1ray, const float *z1ray,
               const float *x2ray, const float *y2ray, const float *z2ray,
              int n, int ivec);
void  filbox (int nx, int ny, int nw, int nh);
void  filclr (const char *copt);
void  filmod (const char *cmod);
void  filopt (const char *copt, const char *ckey);
void  fixspc (float xfac);
void  flab3d (void);
int   flen   (float x, int ndig);
void  frame  (int nfrm);
void  frmclr (int nclr);
void  frmess (int nfrm);
void  gapcrv (float xgap);
void  gaxpar (float a1, float a2, char *copt, char *cax,
              float *a, float *b, float *org, float *stp, int *ndig);
char *getalf (void);
int   getang (void);
int   getbpp (void);
void  getclp (int *nx, int *ny, int *nw, int *nh);
int   getclr (void);
void  getdig (int *nxdig, int *nydig, int *nzdig);
char *getdsp (void);
char *getfil (void);
void  getgrf (float *a, float *e, float *org, float *step, const char *cax);
int   gethgt (void);
int   gethnm (void);
void  getind (int index, float *xr, float *xg, float *xb);
void  getlab (char *cx, char *cy, char *cz);
void  getlen (int *nxl, int *nyl, int *nzl);
int   getlev (void);
int   getlin (void);
int   getlit (float xp, float yp, float zp,
              float xn, float yn, float zn);
void  getmat (const float *xray, const float *yray, const float *zray, int n,
              float *zmat, int nx, int ny, float zval, int *imat, float *wmat);
char *getmfl (void);
char *getmix (const char *copt);
void  getor  (int *nx0, int *ny0);
void  getpag (int *nxpag, int *nypag);
long  getpat (void);
int   getplv (void);
void  getpos (int *nxa, int *nya);
void  getran (int *nca, int *nce);
void  getres (int *npb, int *nph);
void  getrgb (float *xr, float *xg, float *xb);
void  getscl (int *nxscl, int *nyscl, int *nzscl);
void  getscr (int *nwidth, int *nheight);
char *getshf (const char *copt);
void  getsp1 (int *nxdis, int *nydis, int *nzdis);
void  getsp2 (int *nxdis, int *nydis, int *nzdis);
void  getsym (int *nxsym, int *nysym);
void  gettcl (int *nmaj, int *nmin);
void  gettic (int *nxtic, int *nytic, int *nztic);
int   gettyp (void);
void *getuni (void);
float getver (void);
void  getvk  (int *nv, int *nvfx, int *nvfy);
char *getvlt (void);
int   getwid (void);
void  getwin (int *ix, int *iy, int *nwidth, int *nheight);
int   getxid (const char *copt);
void  gifmod (const char *cmod, const char *ckey);
int   gmxalf (const char *copt, char *ca, char *cb);
void  gothic (void);
void  grace  (int ngrace);
void  graf   (float xa, float xe, float xorg, float xstp,
              float ya, float ye, float yorg, float ystp);
void  graf3  (float xa, float xe, float xorg, float xstp,
              float ya, float ye, float yorg, float ystp,
              float za, float ze, float zorg, float zstp);
void  graf3d (float xa, float xe, float xorg, float xstp,
              float ya, float ye, float yorg, float ystp,
              float za, float ze, float zorg, float zstp);
void  grafmp (float xa, float xe, float xorg, float xstp,
              float ya, float ye, float yorg, float ystp);
void  grafp  (float xe, float xorg, float xstp, float yorg, float ystp);
void  grdpol (int ixgrid, int iygrid);
void  grffin (void);
void  grfini (float x1, float y1, float z1,
              float x2, float y2, float z2, 
              float x3, float y3, float z3);
void  grid   (int ixgrid, int iygrid);
void  grid3d (int ixgrid, int iygrid, const char *copt);
void  gridmp (int ixgrid, int iygrid);
int   gwgatt (int id, const char *copt);
int   gwgbox (int id);
int   gwgbut (int id);
void  gwgfil (int id, char *cfile);
float gwgflt (int id);
int   gwgint (int id);
int   gwglis (int id);
float gwgscl (int id);
float gwgtbf (int id, int i, int j);
int   gwgtbi (int id, int i, int j);
void  gwgtbl (int id, float *xray, int n, int idx, char *copt);
void  gwgtbs (int id, int i, int j, char *s);
void  gwgtxt (int id, char *ctext);
int   gwgxid (int id);
void  height (int nhchar);
void  helve  (void);
void  helves (void);
void  histog (const float *xray, int n, float *x, float *y, int *m);
void  hname  (int nhchar);
void  hpgmod (const char *cmod, const char *ckey);
void  hsvrgb (float  xh, float  xs, float  xv, 
              float *xr, float *xg, float *xb);
void  hsym3d (float x);
void  hsymbl (int nhsym);
void  htitle (int nhtit);
void  hwfont (void);
void  hwmode (const char *copt, const char *ckey);
void  hworig (int nx, int ny);
void  hwpage (int nw, int nh);
void  hwscal (float xfac);
void  imgbox (int nx, int ny, int nw, int nh);
void  imgclp (int nx, int ny, int nw, int nh);
void  imgfin (void);
void  imgfmt (const char *copt);
void  imgini (void);
void  imgmod (const char *copt);
void  imgsiz (int nw, int nh);
void  imgtpr (int n);
void  inccrv (int ncrv);
int   incdat (int id, int im, int iy);
void  incfil (const char *cfil);
void  incmrk (int nmrk);
int   indrgb (float xr, float xg, float xb);
void  intax  (void);
int   intcha (int nx, char *cstr);
int   intlen (int nx);
int   intrgb (float xr, float xg, float xb);
int   intutf (int *iray, int n, char *cstr, int nmax);
void  isopts (const float *xray, int nx, const float *yray, int ny, 
              const float *zray, int nz, const float *wmat, float wlev,
              float *xtri, float *ytri, float *ztri, int nmax, int *ntri);
void  itmcat (char *clis, const char *cstr);
int   itmcnt (const char *clis);
char *itmstr (const char *clis, int nlis);
void  labclr (int iclr, const char *copt);
void  labdig (int ndig, const char *cax);
void  labdis (int ndis, const char *cax);
void  labels (const char *clab, const char *cax);
void  labjus (const char *copt, const char *cax);
void  labl3d (const char *copt);
void  labmod (const char *ckey, const char *cval, const char *cax);
void  labpos (const char *cpos, const char *cax);
void  labtyp (const char *ctyp, const char *cax);
void  legclr (void);
void  legend (const char *cbuf, int ncor);
void  legini (char *cbuf, int nlin, int nmaxln);
void  leglin (char *cbuf, const char *cstr, int ilin);
void  legopt (float x1, float x2, float x3);
void  legpat (int ityp, int ithk, int isym, int iclr, long ipat, int ilin);
void  legpos (int nx, int ny);
void  legtit (const char *cstr);
void  legval (float x, char *copt);
void  lfttit (void);
void  licmod (const char *cmod, const char *ckey);
void  licpts (const float *xv, const float *yv, int nx, int ny, 
              const int *itmat, int *iwmat, float *wmat); 
void  light  (const char *copt);
void  lincyc (int index, int ityp);
void  line   (int nx, int ny, int nu, int nv);
void  linesp (float xfac);
void  lintyp (int ntyp);
void  linwid (int i);
void  litmod (int id, const char *copt);
void  litop3 (int id, float xr, float xg, float xb, const char *copt);
void  litopt (int id, float xval, const char *copt);
void  litpos (int id, float x, float y, float z, const char *copt);
void  lncap  (const char *copt);
void  lnjoin (const char *copt);
void  lnmlt  (float x);
void  logtic (const char *cmod);
void  mapbas (const char *cmod);
void  mapfil (const char *cfil, const char *copt);
void  mapimg (const char *cfil, float x1, float x2, float x3, float x4,
              float x5, float x6);
void  maplab (const char *copt, const char *ckey);
void  maplev (const char *cmod);
void  mapmod (const char *cmod);
void  mappol (float xpol, float ypol);
void  mapopt (const char *copt, const char *ckey);
void  mapref (float ylower, float yupper);
void  mapsph (float xrad);
void  marker (int nsym);
void  matop3 (float xr, float xg, float xb, const char *copt);
void  matopt (float xval, const char *copt);
void  mdfmat (int nx, int ny, float weight);
void  messag (const char *cstr, int nx, int ny);
void  metafl (const char *cstr);
void  mixalf (void);
void  mixleg (void);
void  mpaepl (int i);
void  mplang (float x);
void  mplclr (int nbg, int nfg);
void  mplpos (int nx, int ny);
void  mplsiz (int nsize);
void  mpslogo (int nx, int ny, int nsize, const char *copt);
void  mrkclr (int nclr);
void  msgbox (const char *cstr);
void  mshclr (int ic);
void  mshcrv (int n);
void  mylab  (const char *cstr, int itick, const char *cax);
void  myline (const int *nray, int n);
void  mypat  (int iang, int itype, int idens, int icross);
void  mysymb (const float *xray, const float *yray, int n, int isym, 
              int iflag);
void  myvlt  (const float *xr, const float *xg, const float *xb, int n);
void  namdis (int ndis, const char *cax);
void  name   (const char *cstr, const char *cax);
void  namjus (const char *copt, const char *cax);
void  neglog (float eps);
void  newmix (void);
void  newpag (void);
int   nlmess (const char *cstr);
int   nlnumb (float x, int ndig);
void  noarln (void);
void  nobar  (void);
void  nobgd  (void);
void  nochek (void);
void  noclip (void);
void  nofill (void);
void  nograf (void);
void  nohide (void);
void  noline (const char *cax);
void  number (float x, int ndig, int nx, int ny);
void  numfmt (const char *copt);
void  numode (const char *cdec, const char *cgrp, 
              const char *cpos, const char *cfix);
int   nwkday (int id, int im, int iy);
int   nxlegn (const char *cbuf);
int   nxpixl (int ix, int iy);
int   nxposn (float x);
int   nylegn (const char *cbuf);
int   nypixl (int ix, int iy);
int   nyposn (float y);
int   nzposn (float z);
int   openfl (const char *cfil, int nu, int irw); 
void  opnwin (int id);
void  origin (int nx0, int ny0);
void  page   (int nw, int nh);
void  pagera (void);
void  pagfll (int nclr);
void  paghdr (const char *cstr1, const char *cstr2, int iopt, int idir);
void  pagmod (const char *cmod);
void  pagorg (const char *cpos);
void  pagwin (int nw, int nh);
void  patcyc (int index, long ipat);
int   pdfbuf (char *cbuf, int nmax);
void  pdfmod (const char *cmod, const char *ckey);
void  pdfmrk (const char *cstr, const char *copt);
void  penwid (float x);
void  pie    (int nxm, int nym, int nr, float a, float b);
void  piebor (int iclr);
void  piecbk (void (*callbck) (int iseg, float xdat, float xper, int *nrad,
              int *noff, float *angle, int *nvx, int *nvy, int *idrw, 
              int *iann));
void  pieclr (const int *ic1, const int *ic2, int n);
void  pieexp (void);
void  piegrf (const char *cbuf, int nlin, const float *xray, int nseg);
void  pielab (const char *clab, const char *cpos);
void  pieopt (float xf, float a);
void  pierot (float angle);
void  pietyp (const char *ctyp);
void  pieval (float x, const char *copt);
void  pievec (int ivec, const char *copt);
void  pike3d (float x1, float y1, float z1, 
              float x2, float y2, float z2, float r, int nsk1, int nsk2); 
void  plat3d (float xm, float ym, float zm, float xl, const char *copt); 
void  pngmod (const char *cmod, const char *ckey);
void  point  (int nx, int ny, int nb, int nh, int ncol);
void  polar  (float xe, float xorg, float xstp, float yorg, float ystp);
int   polclp (const float *xray, const float *yray, int n, 
              float *xout, float *yout, int nmax, float xv, 
              const char *cedge);
void  polcrv (const char *cpol);
void  polmod (const char *cpos, const char *cdir);
void  pos2pt (float x, float y, float *xp, float *yp);
void  pos3pt (float x, float y, float z, float *xp, float *yp, float *zp);
int   posifl (int nu, int nbyte); 
void  projct (const char *cproj);
void  psfont (const char *cfont);
void  psmode (const char *cmod);
void  pyra3d (float xm, float ym, float zm, float xl, 
              float h1, float h2, int n); 
void  qplbar (const float *yray, int n);
void  qplclr (const float *zmat, int n, int m);
void  qplcrv (const float *xray, const float *yray, int n, const char *copt);
void  qplcon (const float *zmat, int n, int m, int nlv);
void  qplot  (const float *xray, const float *yray, int n);
void  qplpie (const float *xray, int n);
void  qplsca (const float *xray, const float *yray, int n);
void  qplscl (float a, float e, float org, float stp, const char *copt);
void  qplsur (const float *zmat, int n, int m);
void  quad3d (float xm, float ym, float zm, float xl, float yl, float zl);
int   rbfpng (char *cbuf, int nmax);
void  rbmp   (const char *cfil);
int   readfl (int nu, unsigned char *cbuf, int nbyte); 
void  reawgt (void);
void  recfll (int nx, int ny, int nw, int nh, int ncol);
void  rectan (int nx, int ny, int nw, int nh);
void  rel3pt (float x, float y, float z, float *xp, float *yp);
void  resatt (void);
void  reset  (const char *cname);
void  revscr (void);
void  rgbhsv (float xr, float xg, float xb, float *xh, float *xs, float *xv);
void  rgif   (const char *cfil);
void  rgtlab (void);
void  rimage (const char *cfil);
void  rlarc  (float xm, float ym, float xa, float xb,
              float a,  float b,  float t);
void  rlarea (const float *xray, const float *yray, int n);
void  rlcirc (float xm, float ym, float r);
void  rlconn (float x, float y);
void  rlell  (float xm, float ym, float a, float b);
void  rline  (float x, float y, float u, float v);
void  rlmess (const char *cstr, float x, float y);
void  rlnumb (float x, int ndig, float xp, float yp);
void  rlpie  (float xm, float ym, float r, float a, float b);
void  rlpoin (float x, float y, int nb, int nh, int ncol);
void  rlrec  (float x, float y, float xw, float xh);
void  rlrnd  (float x, float y, float xb, float xh, int irnd);
void  rlsec  (float xm, float ym, float r1, float r2,
              float a,  float b,  int ncol);
void  rlstrt (float x, float y);
void  rlsymb (int nsym, float x, float y);
void  rlvec  (float x1, float y1, float x2, float y2, int ivec);
void  rlwind (float xk, float x, float y, int nwidth, float a);
void  rndrec (int nx, int ny, int nb, int nh, int irnd);
void  rot3d  (float xa, float ya, float za); 
void  rpixel (int ix, int iy, int *iclr);
void  rpixls (unsigned char *iray, int ix, int iy, int nw, int nh);
void  rpng   (const char *cfil);
void  rppm   (const char *cfil);
void  rpxrow (unsigned char *iray, int ix, int iy, int n);
void  rtiff  (const char *cfil);
void  rvynam (void);
void  scale  (const char *cscl, const char *cax);
void  sclfac (float xfac);
void  sclmod (const char *cmode);
void  scrmod (const char *cmode);
void  sector (int nx, int ny, int nr1, int nr2, float a, float b, int ncol);
void  selwin (int id);
void  sendbf (void);
void  sendmb (void);
void  sendok (void);
void  serif  (void);
void  setbas (float xfac);
void  setcbk (void (*callbck) (float *x, float *y), const char *copt);
void  setclr (int ncol);
void  setcsr (const char *copt);
void  setexp (float fexp);
void  setfce (const char *copt);
void  setfil (const char *cfil);
void  setgrf (const char *c1, const char *c2, const char *c3, const char *c4);
void  setind (int index, float xr, float xg, float xb);
void  setmix (const char *cstr, const char *cmix);
void  setpag (const char *cpag);
void  setres (int npb, int nph);
void  setrgb (float xr, float xg, float xb);
void  setscl (const float *xray, int n, const char *cax);
void  setvlt (const char *cvlt);
void  setxid (int id, const char *copt);
void  shdafr (const int *inray, const long *ipray, const int *icray, int n);
void  shdasi (const int *inray, const long *ipray, const int *icray, int n);
void  shdaus (const int *inray, const long *ipray, const int *icray, int n);
void  shdcha (void);
void  shdcrv (const float *x1ray, const float *y1ray, int n1,
              const float *x2ray, const float *y2ray, int n2);
void  shdeur (const int *inray, const long *ipray, const int *icray, int n);
void  shdmap (const char *cmap);
void  shdmod (const char *copt, const char *ctyp);
void  shdnor (const int *inray, const long *ipray, const int *icray, int n);
void  shdpat (long ipat);
void  shdsou (const int *inray, const long *ipray, const int *icray, int n);
void  shdusa (const int *inray, const long *ipray, const int *icray, int n);
void  shield (const char *carea, const char *cmode);
void  shlcir (int nx, int ny, int nr);
void  shldel (int id);
void  shlell (int nx, int ny, int na, int nb, float t);
int   shlind (void);
void  shlpie (int nx, int ny, int nr, float a, float b);
void  shlpol (const int *nxray, const int *nyray, int n);
void  shlrct (int nx, int ny, int nw, int nh, float t);
void  shlrec (int nx, int ny, int nw, int nh);
void  shlres (int n);
void  shlsur (void);
void  shlvis (int id, const char *cmode);
void  simplx (void);
int   skipfl (int nu, int nbyte); 
void  smxalf (const char *calph, const char *c1, const char *c2, int n);
void  solid  (void);
void  sortr1 (float *xray, int n, const char *copt);
void  sortr2 (float *xray, float *yray, int n, const char *copt);
void  sphe3d (float xm, float ym, float zm, float r, int n, int m);
void  spline (const float *xray, const float *yray,  int n,
              float *xsray, float *ysray, int *nspl);
void  splmod (int ngrad, int npts);
void  stmmod (const char *cmod, const char *ckey);
void  stmopt (int n, const char *copt);
void  stmpts (const float *xmat, const float *ymat, int nx, int ny, 
              const float *xp, const float *yp, float x0, float y0, 
              float *xray, float *yray, int nmax, int *nray);
void  stmpts3d (const float *xv, const float *yv, const float *zv,
	      int nx, int ny, int nz, const float *xp, const float *yp, 
	      const float *zp, float x0, float y0, float z0, 
              float *xray, float *yray, float *zray, int nmax, int *nray);
void  stmtri (const float *xv, const float *yv, const float *xp, 
              const float *yp, int n, const int *i1ray, const int *i2ray, 
              const int *i3ray, int ntri, const float *xs, 
              const float *ys, int nray);
void  stmval (float x, const char *copt);
void  stream (const float *xmat, const float *ymat, int nx, int ny, 
              const float *xp, const float *yp, 
              const float *xs, const float *ys, int n);
void  stream3d (const float *xv, const float *yv, const float *zv,
	      int nx, int ny, int nz, const float *xp, const float *yp, 
              const float *zp, const float *xs, const float *ys, 
              const float *zs, int n);
void  strt3d (float x, float y, float z);
void  strtpt (float x, float y);
void  surclr (int ictop, int icbot);
void  surfce (const float *xray, int n, const float *yray, int m, 
              const float *zmat);
void  surfcp (float (*zfun)(float x, float y, int i), 
                         float a1, float a2, float astp,
                         float b1, float b2, float bstp);
void  surfun (float (*zfun)(float x, float y), int ixpts, float xdel, 
                     int iypts, float ydel);
void  suriso (const float *xray, int nx, const float *yray, int ny, 
              const float *zray, int nz, const float *wmat, float wlev);
void  surmat (const float *zmat, int nx, int ny, int ixpts, int iypts);
void  surmsh (const char *copt);
void  suropt (const char *copt);
void  surshd (const float *xray, int n, const float *yray, int m, 
              const float *zmat);
void  sursze (float xmin, float xmax, float ymin, float ymax);
void  surtri (const float *xray, const float *yray, const float *zray, int n, 
              const int *i1ray, const int *i2ray, const int *i3ray, int ntri);
void  survis (const char *cvis);
void  swapi2 (short *iray, int n);
void  swapi4 (int *iray, int n);
void  swgatt (int id, const char *cval, const char *copt);
void  swgbox (int ip, int ival);
void  swgbut (int ip, int ival);
void  swgcb  (int id, void (*callbck) (int i, int *ir), int *iray);
void  swgcb2 (int id, void (*callbck) (int id, int irow, int icol));
void  swgcbk (int id, void (*callbck) (int i));
void  swgclr (float xr, float xg, float xb, const char *copt);
void  swgdrw (float x);
void  swgfil (int ip, const char *cval);
void  swgflt (int ip, float xv, int ndig);
void  swgfnt (const char *cfnt, int n);
void  swgfoc (int id);
void  swghlp (const char *cstr);
void  swgint (int ip, int iv);
void  swgiop (int n, const char *copt);
void  swgjus (const char *ctyp, const char *cwidg);
void  swglis (int ip, int ival);
void  swgmix (const char *c, const char *cstr);
void  swgmrg (int ival, const char *cstr);
void  swgoff (int nx, int ny);
void  swgopt (const char *cval, const char *copt);
void  swgpop (const char *copt);
void  swgpos (int nx, int ny);
void  swgray (float *xray, int nray, char *copt);
void  swgscl (int ip, float xval);
void  swgsiz (int nx, int ny);
void  swgspc (float xw, float xh);
void  swgstp (float step);
void  swgtbf (int id, float xval, int ndig, int irow, int icol, char *copt);
void  swgtbi (int id, int ival, int irow, int icol, char *copt);
void  swgtbl (int id, float *xray, int nray, int ndig, int idx, char *copt);
void  swgtbs (int id, char *cstr, int irow, int icol, char *copt);
void  swgtit (const char *ctit);
void  swgtxt (int ip, const char *cval);
void  swgtyp (const char *ctyp, const char *cwidg);
void  swgval (int ip, float xval);
void  swgwin (int nx, int ny, int nw, int nh);
void  swgwth (int nchar);
void  symb3d (int n, float xm, float ym, float zm); 
void  symbol (int nsym, int nx, int ny);
void  symfil (const char *cdev, const char *cstat);
void  symrot (float angle);
int   tellfl (int nu); 
void  texmod (const char *copt);
void  texopt (const char *copt, const char *ctyp);
void  texval (float x, const char *copt);
void  thkc3d (float x);
void  thkcrv (int nthk);
void  thrfin (void);
void  thrini (int n);
void  ticks  (int itick, const char *cax);
void  ticlen (int nmaj, int nmin);
void  ticmod (const char *copt, const char *cax);
void  ticpos (const char *cpos, const char *cax);
void  tifmod (int n, const char *cval, const char *copt);
void  tiforg (int nx, int ny);
void  tifwin (int nx, int ny, int nw, int nh);
void  timopt (void);
void  titjus (const char *copt);
void  title  (void);
void  titlin (const char *cstr, int n);
void  titpos (const char *copt);
void  torus3d (float xm, float ym, float zm, float r1, float r2, float h,
              float a1, float a2, int n, int m);
void  tprfin (void);
void  tprini (void);
void  tprmod (char *cmod, char *ckey);
void  tprval (float x);
void  tr3res (void);
void  tr3rot (float a, float b, float c);
void  tr3scl (float x, float y, float z);
void  tr3shf (float x, float y, float z);
void  trfco1 (float *xray, int n, const char *cfrom, const char *cto); 
void  trfco2 (float *xray, float *yray, int n, 
              const char *cfrom, const char *cto); 
void  trfco3 (float *xray, float *yray, float *zray, int n, 
              const char *cfrom, const char *cto);
void  trfdat (int ndays, int *id, int *im, int *iy); 
void  trfmat (const float *zmat, int nx, int ny, 
              float *zmat2, int nx2, int ny2);
void  trfrel (float *xray, float *yray, int n);
void  trfres (void);
void  trfrot (float xang, int nx, int ny);
void  trfscl (float xscl, float yscl);
void  trfshf (int nx, int ny);
void  tria3d (const float *xtri, const float *ytri, const float *ztri);
int   triang (const float *xray, const float *yray, int n, 
              int *i1ray, int *i2ray, int *i3ray, int nmax); 
void  trifll (const float *xray, const float *yray);
void  triplx (void);
void  tripts (const float *xray, const float *yray, const float *zray, int n, 
              const int *i1ray, const int *i2ray, const int *i3ray, int ntri,
              float zlev, float *xpts, float *ypts, int maxpts, 
              int *nptray, int maxray, int *nlins);
int   trmlen (const char *cstr);
void  tube3d (float x1, float y1, float z1, 
              float x2, float y2, float z2, float r, int nsk1, int nsk2); 
void  txtjus (const char *copt);
void  txture (int *itmat, int nx, int ny); 
void  unit   (void *fp);
void  units  (const char *copt);
void  upstr  (char *cstr);
int   utfint (char *cstr, int *iray, int n);
void  vang3d (float a);
void  vclp3d (float x1, float x2);
void  vecclr (int iclr);
void  vecf3d (const float *xv, const float *yv, const float *zv, 
              const float *xp, const float *yp, const float *zp,
              int n, int ivec);
void  vecfld (const float *xv, const float *yv, 
              const float *xp, const float *yp, int n, int ivec);
void  vecmat (const float *xmat, const float *ymat, int nx, int ny, 
              const float *xp, const float *yp, int ivec); 
void  vecmat3d (const float *xv, const float *yv, const float *zv, 
		int nx, int ny, int nz, const float *xp, const float *yp,
                const float *zp, int ivec); 
void  vecopt (float x, const char *copt);
void  vector (int nx1, int ny1, int nx2, int ny2, int ivec);
void  vectr3 (float x1, float y1, float z1,
              float x2, float y2, float z2, int ivec);
void  vfoc3d (float x, float y, float z, const char *cview);
void  view3d (float xvu, float yvu, float zvu, const char *cvu);
void  vkxbar (int nvfx);
void  vkybar (int nvfy);
void  vkytit (int nvfy);
void  vltfil (const char *cfl, const char *copt);
void  vtx3d  (const float *xray, const float *yray, const float *zray,
              int n, const char *copt);
void  vtxc3d (const float *xray, const float *yray, const float *zray,
              const int *ic, int n, const char *copt);
void  vtxn3d (const float *xray, const float *yray, const float *zray, 
              const float *xn, const float *yn, const float *zn, 
              int n, const char *copt);
void  vup3d  (float a);
int   wgapp  (int ip, const char *clab);
int   wgbas  (int ip, const char *ctyp);
int   wgbox  (int ip, const char *cstr, int isel);
int   wgbut  (int ip, const char *cstr, int ival);
int   wgcmd  (int ip, const char *clab, const char *cmd);
int   wgdlis (int ip, const char *cstr, int isel);
int   wgdraw (int ip);
int   wgfil  (int ip, const char *clab, const char *cstr, const char *cmask);
void  wgfin  (void);
int   wgini  (const char *ctyp);
int   wglab  (int ip, const char *cstr);
int   wglis  (int ip, const char *cstr, int isel);
int   wgltxt (int ip, const char *clab, const char *ctext, int iper);
int   wgok   (int ip);
int   wgpbar (int ip, float x1, float x2, float xstp);
int   wgpbut (int ip, const char *clab);
int   wgpop  (int ip, const char *clab);
int   wgquit (int ip);
int   wgscl  (int ip, const char *cstr, float x1, float x2, float xval, 
              int ndez);
int   wgsep  (int ip);
int   wgstxt (int ip, int nsize, int nmax);
int   wgtbl (int ip, int nrows, int ncols);
int   wgtxt  (int ip, const char *cstr);
void  widbar (int nzb);
void  wimage (const char *cfil);  
void  winapp (const char *copt);  
void  windbr (float xk, int nx, int ny, int nwidth, float a);
void  window (int nx, int ny, int nw, int nh);
void  winfin (int iopt);
void  winfnt (const char *cfont);
int   winid  (void);
void  winkey (const char *copt);
void  winmod (const char *copt);
void  winopt (int iopt, const char *copt);
void  winsiz (int nw, int nh);
void  wintit (const char *cstr);
void  wmfmod (const char *cmod, const char *ckey);
void  world  (void);
void  wpixel (int ix, int iy, int iclr);
void  wpixls (const unsigned char *iray, int ix, int iy, int nw, int nh);
void  wpxrow (const unsigned char *iray, int ix, int iy, int n);
int   writfl (int nu, const unsigned char *cbuf, int nbyte); 
void  wtiff  (const char *cfil);
void  x11fnt (const char *cfont, const char *copt);
void  x11mod (const char *copt);
float x2dpos (float x, float y);
float x3dabs (float x, float y, float z);
float x3dpos (float x, float y, float z);
float x3drel (float x, float y, float z);
void  xaxgit (void);
void  xaxis  (float xa, float xe, float xorg, float xstp, 
              int nl, const char *cstr, int it, int nx, int ny);
void  xaxlg  (float xa, float xe, float xorg, float xstp,
              int nl, const char *cstr, int it, int nx, int ny);
void  xaxmap (float xa, float xe, float xorg, float xstp,
              const char *cstr, int it, int ny);
void  xcross (void);
void  xdraw  (float x, float y);
float xinvrs (int n);
void  xmove  (float x, float y);
float xposn  (float x);
float y2dpos (float x, float y);
float y3dabs (float x, float y, float z);
float y3dpos (float x, float y, float z);
float y3drel (float x, float y, float z);
void  yaxgit (void);
void  yaxis  (float ya, float ye, float yor, float ystp, 
              int nl, const char *cstr, int it, int nx, int ny);
void  yaxlg  (float ya, float ye, float yor, float ystp,
              int nl, const char *cstr, int it, int nx, int ny);
void  yaxmap (float ya, float ye, float yor, float ystp,
              const char *cstr, int it, int ny);
void  ycross (void);
float yinvrs (int n);
float yposn  (float y);
float z3dpos (float x, float y, float z);
void  zaxis  (float za, float ze, float zor, float zstp, 
              int nl, const char *cstr, int it, int id, int nx, int ny);
void  zaxlg  (float za, float ze, float zor, float zstp, 
              int nl, const char *cstr, int it, int id, int nx, int ny);
void  zbfers (void);
void  zbffin (void);
int   zbfini (void);
void  zbflin (float x1, float y1, float z1, float x2, float y2, float z2);
void  zbfmod (const char *copt);
void  zbfres (void);
void  zbfscl (float x);
void  zbftri (const float *x, const float *y, const float *z, const int *ic);
void  zscale (float za, float ze);

#ifdef __cplusplus
  }
#endif
