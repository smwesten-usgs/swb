/****************************************************************/
/**                        DISLIN.H                            **/
/**                                                            **/
/** INCLUDE file for DISLIN C routines.                        **/
/**                                                            **/
/** Date     : 08.08.2010                                      **/
/** Functions: 727                                             **/
/** Version  : 10.0 / Double Precision                         **/
/****************************************************************/

#ifdef __cplusplus
  extern "C" {
#endif

void  abs3pt (double x, double y, double z, double *xp, double *yp);
void  addlab (const char *cstr, double v, int itic, const char *cax);
void  angle  (int ngrad);
void  arcell (int nx, int ny, int na, int nb, double a, double b, double t);
void  areaf  (const int *nxray, const int *nyray, int n);
void  autres (int ixdim, int iydim);
void  ax2grf (void);
void  ax3len (int nxl, int nyl, int nzl);
void  axclrs (int nclr, const char *copt, const char *cax);
void  axends (const char *cstr, const char *cax);
void  axgit  (void);
void  axis3d (double x3, double y3, double z3);
void  axsbgd (int nclr);
void  axslen (int nxl, int nyl);
void  axsorg (int nxa, int nya);
void  axspos (int nxa, int nya);
void  axsscl (const char *cscl, const char *cax);
void  axstyp (const char *copt);
void  barbor (int iclr);
void  barclr (int ic1, int ic2, int ic3);
void  bargrp (int ngrp, double gap);
void  barmod (const char *cmod, const char *copt);
void  baropt (double xf, double a);
void  barpos (const char *copt);
void  bars   (double *xray, double *y1ray, double *y2ray, int n);
void  bars3d (const double *xray, const double *yray, const double *z1ray, 
              const double *z2ray, const double *xwray, const double *ywray,
              const int *icray, int n);
void  bartyp (const char *ctyp);
void  barwth (double factor);
void  basalf (const char *calph);
void  basdat (int id, int im, int iy); 
void  bezier (const double *xray, const double *yray, int nray, 
              double *x, double *y, int n);
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
void  cgmbgd (double xr, double xg, double xb);
void  cgmpic (const char *cstr);
void  cgmver (int nclr);
void  chaang (double angle);
void  chacod (const char *copt);
void  chaspc (double xspc);
void  chawth (double xwth);
void  chnatt (void);
void  chncrv (const char *copt);
void  chndot (void);
void  chndsh (void);
void  chnbar (const char *copt);
void  chnpie (const char *copt);
void  circ3p (double x1, double y1, double x2, double y2, double x3, double y3,
              double *xm, double *ym, double *r);
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
void  colray (const double *zray, int *nray, int n);
void  complx (void);
void  conclr (const int *nray, int n);
void  concrv (const double *xray, const double *yray, int n, double zlev);
void  cone3d (double xm, double ym, double zm, double r, 
              double h1, double h2, int nsk1, int nsk2); 
void  confll (const double *xray, const double *yray, const double *zray, int n, 
              const int *i1ray, const int *i2ray, const int *i3ray, int ntri, 
              const double *zlev, int nlev);
void  congap (double xfac);
void  conlab (const char *clab);
void  conmat (const double *zmat, int n, int m, double zlev);
void  conmod (double xfac, double xquot);
void  conn3d (double x, double y, double z);
void  connpt (double x, double y);
void  conpts (const double *xray, int n, const double *yray, int m, 
              const double *zmat, double zlev, double *xpts, double *ypts, 
              int maxpts, int *nray, int maxray, int *nlins);
void  conshd (const double *xray, int n, const double *yray, int m,
              const double *zmat, const double *zlev, int nlev);
void  conshd3d (const double *xray, int n, const double *yray, int m, 
              const double *zmat, const double *zlev, int nlev);
void  contri (const double *xray, const double *yray, const double *zray, int n, 
              const int *i1ray, const int *i2ray, const int *i3ray, 
              int ntri, double zlev);
void  contur (const double *xray, int n, const double *yray, int m,
              const double *zmat, double zlev);
void  cross  (void);
void  crvmat (const double *zmat, int n, int m, int ixpts, int iypts);
void  crvqdr (const double *xray, const double *yray, const double *zray, 
              int n); 
void  crvt3d (const double *xray, const double *yray, const double *zray,
              const double *rray, const int *icray, int n);
void  crvtri (const double *xray, const double *yray, const double *zray, int n, 
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
void  curv3d (const double *xray, const double *yray, const double *zray, int n);
void  curve  (const double *xray, const double *yray, int n);
void  curve3 (const double *xray, const double *yray, const double *zray, int n);
void  curvmp (const double *xray, const double *yray, int n);
void  curvx3 (const double *xray, double y, const double *zray, int n);
void  curvy3 (double x, const double *yray, const double *zray, int n);
void  cyli3d (double xm, double ym, double zm, double r, double h, 
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
void  disk3d (double xm, double ym, double zm, double r1, double r2, 
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
void  errbar (const double *x, const double *y, 
              const double *err1, const double *err2, int n);
void  errdev (const char *cdev);
void  errfil (const char *cfil);
void  errmod (const char *cstr, const char *copt);
void  eushft (const char *cnat, const char *cshf);
void  expzlb (const char *cstr);
void  fbars  (const double *xray, const double *y1ray, const double *y2ray, 
              const double *y3ray, const double *y4ray, int n);
int   fcha   (double x, int ndig, char *cstr);
void  field  (const double *xray, const double *yray,
              const double *uray, const double *vray, int n, int ivec);
void  field3d (const double *x1ray, const double *y1ray, const double *z1ray,
              const double *x2ray, const double *y2ray, const double *z2ray,
              int n, int ivec);
void  filbox (int nx, int ny, int nw, int nh);
void  filclr (const char *copt);
void  filmod (const char *cmod);
void  filopt (const char *copt, const char *ckey);
void  fixspc (double xfac);
void  flab3d (void);
int   flen   (double x, int ndig);
void  frame  (int nfrm);
void  frmclr (int nclr);
void  frmess (int nfrm);
void  gapcrv (double xgap);
void  gaxpar (double a1, double a2, char *copt, char *cax,
              double *a, double *b, double *org, double *stp, int *ndig);
char *getalf (void);
int   getang (void);
int   getbpp (void);
void  getclp (int *nx, int *ny, int *nw, int *nh);
int   getclr (void);
void  getdig (int *nxdig, int *nydig, int *nzdig);
char *getdsp (void);
char *getfil (void);
void  getgrf (double *a, double *e, double *org, double *step, const char *cax);
int   gethgt (void);
int   gethnm (void);
void  getind (int index, double *xr, double *xg, double *xb);
void  getlab (char *cx, char *cy, char *cz);
void  getlen (int *nxl, int *nyl, int *nzl);
int   getlev (void);
int   getlin (void);
int   getlit (double xp, double yp, double zp,
              double xn, double yn, double zn);
void  getmat (const double *xray, const double *yray, const double *zray, int n,
              double *zmat, int nx, int ny, double zval, int *imat, double *wmat);
char *getmfl (void);
char *getmix (const char *copt);
void  getor  (int *nx0, int *ny0);
void  getpag (int *nxpag, int *nypag);
long  getpat (void);
int   getplv (void);
void  getpos (int *nxa, int *nya);
void  getran (int *nca, int *nce);
void  getres (int *npb, int *nph);
void  getrgb (double *xr, double *xg, double *xb);
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
double getver (void);
void  getvk  (int *nv, int *nvfx, int *nvfy);
char *getvlt (void);
int   getwid (void);
void  getwin (int *ix, int *iy, int *nwidth, int *nheight);
int   getxid (const char *copt);
void  gifmod (const char *cmod, const char *ckey);
int   gmxalf (const char *copt, char *ca, char *cb);
void  gothic (void);
void  grace  (int ngrace);
void  graf   (double xa, double xe, double xorg, double xstp,
              double ya, double ye, double yorg, double ystp);
void  graf3  (double xa, double xe, double xorg, double xstp,
              double ya, double ye, double yorg, double ystp,
              double za, double ze, double zorg, double zstp);
void  graf3d (double xa, double xe, double xorg, double xstp,
              double ya, double ye, double yorg, double ystp,
              double za, double ze, double zorg, double zstp);
void  grafmp (double xa, double xe, double xorg, double xstp,
              double ya, double ye, double yorg, double ystp);
void  grafp  (double xe, double xorg, double xstp, double yorg, double ystp);
void  grdpol (int ixgrid, int iygrid);
void  grffin (void);
void  grfini (double x1, double y1, double z1,
              double x2, double y2, double z2, 
              double x3, double y3, double z3);
void  grid   (int ixgrid, int iygrid);
void  grid3d (int ixgrid, int iygrid, const char *copt);
void  gridmp (int ixgrid, int iygrid);
int   gwgatt (int id, const char *copt);
int   gwgbox (int id);
int   gwgbut (int id);
void  gwgfil (int id, char *cfile);
double gwgflt (int id);
int   gwgint (int id);
int   gwglis (int id);
double gwgscl (int id);
double gwgtbf (int id, int i, int j);
int   gwgtbi (int id, int i, int j);
void  gwgtbl (int id, double *xray, int n, int idx, char *copt);
void  gwgtbs (int id, int i, int j, char *s);
void  gwgtxt (int id, char *ctext);
int   gwgxid (int id);
void  height (int nhchar);
void  helve  (void);
void  helves (void);
void  histog (const double *xray, int n, double *x, double *y, int *m);
void  hname  (int nhchar);
void  hpgmod (const char *cmod, const char *ckey);
void  hsvrgb (double  xh, double  xs, double  xv, 
              double *xr, double *xg, double *xb);
void  hsym3d (double x);
void  hsymbl (int nhsym);
void  htitle (int nhtit);
void  hwfont (void);
void  hwmode (const char *copt, const char *ckey);
void  hworig (int nx, int ny);
void  hwpage (int nw, int nh);
void  hwscal (double xfac);
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
int   indrgb (double xr, double xg, double xb);
void  intax  (void);
int   intcha (int nx, char *cstr);
int   intlen (int nx);
int   intrgb (double xr, double xg, double xb);
int   intutf (int *iray, int n, char *cstr, int nmax);
void  isopts (const double *xray, int nx, const double *yray, int ny, 
              const double *zray, int nz, const double *wmat, double wlev,
              double *xtri, double *ytri, double *ztri, int nmax, int *ntri);
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
void  legopt (double x1, double x2, double x3);
void  legpat (int ityp, int ithk, int isym, int iclr, long ipat, int ilin);
void  legpos (int nx, int ny);
void  legtit (const char *cstr);
void  legval (double x, char *copt);
void  lfttit (void);
void  licmod (const char *cmod, const char *ckey);
void  licpts (const double *xv, const double *yv, int nx, int ny, 
              const int *itmat, int *iwmat, double *wmat); 
void  light  (const char *copt);
void  lincyc (int index, int ityp);
void  line   (int nx, int ny, int nu, int nv);
void  linesp (double xfac);
void  lintyp (int ntyp);
void  linwid (int i);
void  litmod (int id, const char *copt);
void  litop3 (int id, double xr, double xg, double xb, const char *copt);
void  litopt (int id, double xval, const char *copt);
void  litpos (int id, double x, double y, double z, const char *copt);
void  lncap  (const char *copt);
void  lnjoin (const char *copt);
void  lnmlt  (double x);
void  logtic (const char *cmod);
void  mapbas (const char *cmod);
void  mapfil (const char *cfil, const char *copt);
void  mapimg (const char *cfil, double x1, double x2, double x3, double x4,
              double x5, double x6);
void  maplab (const char *copt, const char *ckey);
void  maplev (const char *cmod);
void  mapmod (const char *cmod);
void  mappol (double xpol, double ypol);
void  mapopt (const char *copt, const char *ckey);
void  mapref (double ylower, double yupper);
void  mapsph (double xrad);
void  marker (int nsym);
void  matop3 (double xr, double xg, double xb, const char *copt);
void  matopt (double xval, const char *copt);
void  mdfmat (int nx, int ny, double weight);
void  messag (const char *cstr, int nx, int ny);
void  metafl (const char *cstr);
void  mixalf (void);
void  mixleg (void);
void  mpaepl (int i);
void  mplang (double x);
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
void  mysymb (const double *xray, const double *yray, int n, int isym, 
              int iflag);
void  myvlt  (const double *xr, const double *xg, const double *xb, int n);
void  namdis (int ndis, const char *cax);
void  name   (const char *cstr, const char *cax);
void  namjus (const char *copt, const char *cax);
void  neglog (double eps);
void  newmix (void);
void  newpag (void);
int   nlmess (const char *cstr);
int   nlnumb (double x, int ndig);
void  noarln (void);
void  nobar  (void);
void  nobgd  (void);
void  nochek (void);
void  noclip (void);
void  nofill (void);
void  nograf (void);
void  nohide (void);
void  noline (const char *cax);
void  number (double x, int ndig, int nx, int ny);
void  numfmt (const char *copt);
void  numode (const char *cdec, const char *cgrp, 
              const char *cpos, const char *cfix);
int   nwkday (int id, int im, int iy);
int   nxlegn (const char *cbuf);
int   nxpixl (int ix, int iy);
int   nxposn (double x);
int   nylegn (const char *cbuf);
int   nypixl (int ix, int iy);
int   nyposn (double y);
int   nzposn (double z);
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
void  penwid (double x);
void  pie    (int nxm, int nym, int nr, double a, double b);
void  piebor (int iclr);
void  piecbk (void (*callbck) (int iseg, double xdat, double xper, int *nrad,
              int *noff, double *angle, int *nvx, int *nvy, int *idrw, 
              int *iann));
void  pieclr (const int *ic1, const int *ic2, int n);
void  pieexp (void);
void  piegrf (const char *cbuf, int nlin, const double *xray, int nseg);
void  pielab (const char *clab, const char *cpos);
void  pieopt (double xf, double a);
void  pierot (double angle);
void  pietyp (const char *ctyp);
void  pieval (double x, const char *copt);
void  pievec (int ivec, const char *copt);
void  pike3d (double x1, double y1, double z1, 
              double x2, double y2, double z2, double r, int nsk1, int nsk2); 
void  plat3d (double xm, double ym, double zm, double xl, const char *copt); 
void  pngmod (const char *cmod, const char *ckey);
void  point  (int nx, int ny, int nb, int nh, int ncol);
void  polar  (double xe, double xorg, double xstp, double yorg, double ystp);
void  polcrv (const char *cpol);
int   polclp (const double *xray, const double *yray, int n, 
              double *xout, double *yout, int nmax, double xv, 
              const char *cedge);
void  polmod (const char *cpos, const char *cdir);
void  pos2pt (double x, double y, double *xp, double *yp);
void  pos3pt (double x, double y, double z, double *xp, double *yp, double *zp);
int   posifl (int nu, int nbyte); 
void  projct (const char *cproj);
void  psfont (const char *cfont);
void  psmode (const char *cmod);
void  pyra3d (double xm, double ym, double zm, double xl, 
              double h1, double h2, int n); 
void  qplbar (const double *yray, int n);
void  qplclr (const double *zmat, int n, int m);
void  qplcon (const double *zmat, int n, int m, int nlv);
void  qplcrv (const double *xray, const double *yray, int n, const char *copt);
void  qplot  (const double *xray, const double *yray, int n);
void  qplpie (const double *xray, int n);
void  qplsca (const double *xray, const double *yray, int n);
void  qplscl (double a, double e, double org, double stp, const char *copt);
void  qplsur (const double *zmat, int n, int m);
void  quad3d (double xm, double ym, double zm, 
              double xl, double yl, double zl);
int   rbfpng (char *cbuf, int nmax);
void  rbmp   (const char *cfil);
int   readfl (int nu, unsigned char *cbuf, int nbyte); 
void  reawgt (void);
void  recfll (int nx, int ny, int nw, int nh, int ncol);
void  rectan (int nx, int ny, int nw, int nh);
void  rel3pt (double x, double y, double z, double *xp, double *yp);
void  resatt (void);
void  reset  (const char *cname);
void  revscr (void);
void  rgbhsv (double xr, double xg, double xb, double *xh, double *xs, double *xv);
void  rgif   (const char *cfil);
void  rgtlab (void);
void  rimage (const char *cfil);
void  rlarc  (double xm, double ym, double xa, double xb,
              double a,  double b,  double t);
void  rlarea (const double *xray, const double *yray, int n);
void  rlcirc (double xm, double ym, double r);
void  rlconn (double x, double y);
void  rlell  (double xm, double ym, double a, double b);
void  rline  (double x, double y, double u, double v);
void  rlmess (const char *cstr, double x, double y);
void  rlnumb (double x, int ndig, double xp, double yp);
void  rlpie  (double xm, double ym, double r, double a, double b);
void  rlpoin (double x, double y, int nb, int nh, int ncol);
void  rlrec  (double x, double y, double xw, double xh);
void  rlrnd  (double x, double y, double xb, double xh, int irnd);
void  rlsec  (double xm, double ym, double r1, double r2,
              double a,  double b,  int ncol);
void  rlstrt (double x, double y);
void  rlsymb (int nsym, double x, double y);
void  rlvec  (double x1, double y1, double x2, double y2, int ivec);
void  rlwind (double xk, double x, double y, int nwidth, double a);
void  rndrec (int nx, int ny, int nb, int nh, int irnd);
void  rot3d  (double xa, double ya, double za); 
void  rpixel (int ix, int iy, int *iclr);
void  rpixls (unsigned char *iray, int ix, int iy, int nw, int nh);
void  rpng   (const char *cfil);
void  rppm   (const char *cfil);
void  rpxrow (unsigned char *iray, int ix, int iy, int n);
void  rtiff  (const char *cfil);
void  rvynam (void);
void  scale  (const char *cscl, const char *cax);
void  sclfac (double xfac);
void  sclmod (const char *cmode);
void  scrmod (const char *cmode);
void  sector (int nx, int ny, int nr1, int nr2, double a, double b, int ncol);
void  selwin (int id);
void  sendbf (void);
void  sendmb (void);
void  sendok (void);
void  serif  (void);
void  setbas (double xfac);
void  setcbk (void (*callbck) (double *x, double *y), const char *copt);
void  setclr (int ncol);
void  setcsr (const char *copt);
void  setexp (double fexp);
void  setfce (const char *copt);
void  setfil (const char *cfil);
void  setgrf (const char *c1, const char *c2, const char *c3, const char *c4);
void  setind (int index, double xr, double xg, double xb);
void  setmix (const char *cstr, const char *cmix);
void  setpag (const char *cpag);
void  setres (int npb, int nph);
void  setrgb (double xr, double xg, double xb);
void  setscl (const double *xray, int n, const char *cax);
void  setvlt (const char *cvlt);
void  setxid (int id, const char *copt);
void  shdafr (const int *inray, const long *ipray, const int *icray, int n);
void  shdasi (const int *inray, const long *ipray, const int *icray, int n);
void  shdaus (const int *inray, const long *ipray, const int *icray, int n);
void  shdcha (void);
void  shdcrv (const double *x1ray, const double *y1ray, int n1,
              const double *x2ray, const double *y2ray, int n2);
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
void  shlell (int nx, int ny, int na, int nb, double t);
int   shlind (void);
void  shlpie (int nx, int ny, int nr, double a, double b);
void  shlpol (const int *nxray, const int *nyray, int n);
void  shlrct (int nx, int ny, int nw, int nh, double t);
void  shlrec (int nx, int ny, int nw, int nh);
void  shlres (int n);
void  shlsur (void);
void  shlvis (int id, const char *cmode);
void  simplx (void);
int   skipfl (int nu, int nbyte); 
void  smxalf (const char *calph, const char *c1, const char *c2, int n);
void  solid  (void);
void  sortr1 (double *xray, int n, const char *copt);
void  sortr2 (double *xray, double *yray, int n, const char *copt);
void  sphe3d (double xm, double ym, double zm, double r, int n, int m);
void  spline (const double *xray, const double *yray,  int n,
              double *xsray, double *ysray, int *nspl);
void  splmod (int ngrad, int npts);
void  stmmod (const char *cmod, const char *ckey);
void  stmopt (int n, const char *copt);
void  stmpts (const double *xmat, const double *ymat, int nx, int ny, 
              const double *xp, const double *yp, double x0, double y0, 
              double *xray, double *yray, int nmax, int *nray);
void  stmpts3d (const double *xv, const double *yv, const double *zv,
	      int nx, int ny, int nz, const double *xp, const double *yp, 
	      const double *zp, double x0, double y0, double z0, 
              double *xray, double *yray, double *zray, int nmax, int *nray);
void  stmtri (const double *xv, const double *yv, const double *xp, 
              const double *yp, int n, const int *i1ray, const int *i2ray, 
              const int *i3ray, int ntri, const double *xs, 
              const double *ys, int nray);
void  stmval (double x, const char *copt);
void  stream (const double *xmat, const double *ymat, int nx, int ny, 
              const double *xp, const double *yp, 
              const double *xs, const double *ys, int n);
void  stream3d (const double *xv, const double *yv, const double *zv,
	      int nx, int ny, int nz, const double *xp, const double *yp, 
              const double *zp, const double *xs, const double *ys, 
              const double *zs, int n);
void  strt3d (double x, double y, double z);
void  strtpt (double x, double y);
void  surclr (int ictop, int icbot);
void  surfce (const double *xray, int n, const double *yray, int m, 
              const double *zmat);
void  surfcp (double (*zfun)(double x, double y, int i), 
                         double a1, double a2, double astp,
                         double b1, double b2, double bstp);
void  surfun (double (*zfun)(double x, double y), int ixpts, double xdel, 
                     int iypts, double ydel);
void  suriso (const double *xray, int nx, const double *yray, int ny, 
              const double *zray, int nz, const double *wmat, double wlev);
void  surmat (const double *zmat, int nx, int ny, int ixpts, int iypts);
void  surmsh (const char *copt);
void  suropt (const char *copt);
void  surshd (const double *xray, int n, const double *yray, int m, 
              const double *zmat);
void  sursze (double xmin, double xmax, double ymin, double ymax);
void  surtri (const double *xray, const double *yray, const double *zray, int n, 
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
void  swgclr (double xr, double xg, double xb, const char *copt);
void  swgdrw (double x);
void  swgfil (int ip, const char *cval);
void  swgflt (int ip, double xv, int ndig);
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
void  swgray (double *xray, int nray, char *copt);
void  swgscl (int ip, double xval);
void  swgsiz (int nx, int ny);
void  swgspc (double xw, double xh);
void  swgstp (double step);
void  swgtbf (int id, double xval, int ndig, int irow, int icol, char *copt);
void  swgtbi (int id, int ival, int irow, int icol, char *copt);
void  swgtbl (int id, double *xray, int nray, int ndig, int idx, char *copt);
void  swgtbs (int id, char *cstr, int irow, int icol, char *copt);
void  swgtit (const char *ctit);
void  swgtxt (int ip, const char *cval);
void  swgtyp (const char *ctyp, const char *cwidg);
void  swgval (int ip, double xval);
void  swgwin (int nx, int ny, int nw, int nh);
void  swgwth (int nchar);
void  symb3d (int n, double xm, double ym, double zm); 
void  symbol (int nsym, int nx, int ny);
void  symfil (const char *cdev, const char *cstat);
void  symrot (double angle);
int   tellfl (int nu); 
void  texmod (const char *copt);
void  texopt (const char *copt, const char *ctyp);
void  texval (double x, const char *copt);
void  thkc3d (double x);
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
void  torus3d (double xm, double ym, double zm, double r1, double r2, 
	       double h, double a1, double a2, int n, int m);
void  tprfin (void);
void  tprini (void);
void  tprmod (char *cmod, char *ckey);
void  tprval (double x);
void  tr3res (void);
void  tr3rot (double a, double b, double c);
void  tr3scl (double x, double y, double z);
void  tr3shf (double x, double y, double z);
void  trfco1 (double *xray, int n, const char *cfrom, const char *cto); 
void  trfco2 (double *xray, double *yray, int n, 
              const char *cfrom, const char *cto); 
void  trfco3 (double *xray, double *yray, double *zray, int n, 
              const char *cfrom, const char *cto);
void  trfdat (int ndays, int *id, int *im, int *iy); 
void  trfmat (const double *zmat, int nx, int ny, 
              double *zmat2, int nx2, int ny2);
void  trfrel (double *xray, double *yray, int n);
void  trfres (void);
void  trfrot (double xang, int nx, int ny);
void  trfscl (double xscl, double yscl);
void  trfshf (int nx, int ny);
void  tria3d (const double *xtri, const double *ytri, const double *ztri);
int   triang (const double *xray, const double *yray, int n, 
              int *i1ray, int *i2ray, int *i3ray, int nmax); 
void  trifll (const double *xray, const double *yray);
void  triplx (void);
void  tripts (const double *xray, const double *yray, const double *zray, 
              int n, 
              const int *i1ray, const int *i2ray, const int *i3ray, int ntri,
              double zlev, double *xpts, double *ypts, int maxpts, 
              int *nptray, int maxray, int *nlins);
int   trmlen (const char *cstr);
void  tube3d (double x1, double y1, double z1, 
              double x2, double y2, double z2, double r, int nsk1, int nsk2); 
void  txtjus (const char *copt);
void  txture (int *itmat, int nx, int ny); 
void  unit   (void *fp);
void  units  (const char *copt);
void  upstr  (char *cstr);
int   utfint (char *cstr, int *iray, int n);
void  vang3d (double a);
void  vclp3d (double x1, double x2);
void  vecclr (int iclr);
void  vecf3d (const double *xv, const double *yv, const double *zv, 
              const double *xp, const double *yp, const double *zp,
              int n, int ivec);
void  vecfld (const double *xv, const double *yv, 
              const double *xp, const double *yp, int n, int ivec);
void  vecmat (const double *xmat, const double *ymat, int nx, int ny, 
              const double *xp, const double *yp, int ivec);
void  vecmat3d (const double *xv, const double *yv, const double *zv,
                int nx, int ny, int nz, const double *xp, const double *yp, 
                const double *zp, int ivec);
void  vecopt (double x, const char *copt);
void  vector (int nx1, int ny1, int nx2, int ny2, int ivec);
void  vectr3 (double x1, double y1, double z1,
              double x2, double y2, double z2, int ivec);
void  vfoc3d (double x, double y, double z, const char *cview);
void  view3d (double xvu, double yvu, double zvu, const char *cvu);
void  vkxbar (int nvfx);
void  vkybar (int nvfy);
void  vkytit (int nvfy);
void  vltfil (const char *cfl, const char *copt);
void  vtx3d  (const double *xray, const double *yray, const double *zray,
              int n, const char *copt);
void  vtxc3d (const double *xray, const double *yray, const double *zray,
              const int *ic, int n, 
              const char *copt);
void  vtxn3d (const double *xray, const double *yray, const double *zray, 
              const double *xn, const double *yn, const double *zn, 
              int n, const char *copt);
void  vup3d  (double a);
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
int   wgpbar (int ip, double x1, double x2, double xval);
int   wgpbut (int ip, const char *clab);
int   wgpop  (int ip, const char *clab);
int   wgquit (int ip);
int   wgscl  (int ip, const char *cstr, double x1, double x2, double xval, 
              int ndez);
int   wgsep  (int ip);
int   wgstxt (int ip, int nsize, int nmax);
int   wgtbl  (int ip, int nrows, int ncols);
int   wgtxt  (int ip, const char *cstr);
void  widbar (int nzb);
void  wimage (const char *cfil);  
void  winapp (const char *copt);  
void  windbr (double xk, int nx, int ny, int nwidth, double a);
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
double x2dpos (double x, double y);
double x3dabs (double x, double y, double z);
double x3dpos (double x, double y, double z);
double x3drel (double x, double y, double z);
void  xaxgit (void);
void  xaxis  (double xa, double xe, double xorg, double xstp, 
              int nl, const char *cstr, int it, int nx, int ny);
void  xaxlg  (double xa, double xe, double xorg, double xstp,
              int nl, const char *cstr, int it, int nx, int ny);
void  xaxmap (double xa, double xe, double xorg, double xstp,
              const char *cstr, int it, int ny);
void  xcross (void);
void  xdraw  (double x, double y);
double xinvrs (int n);
void  xmove  (double x, double y);
double xposn  (double x);
double y2dpos (double x, double y);
double y3dabs (double x, double y, double z);
double y3dpos (double x, double y, double z);
double y3drel (double x, double y, double z);
void  yaxgit (void);
void  yaxis  (double ya, double ye, double yor, double ystp, 
              int nl, const char *cstr, int it, int nx, int ny);
void  yaxlg  (double ya, double ye, double yor, double ystp,
              int nl, const char *cstr, int it, int nx, int ny);
void  yaxmap (double ya, double ye, double yor, double ystp,
              const char *cstr, int it, int ny);
void  ycross (void);
double yinvrs (int n);
double yposn  (double y);
double z3dpos (double x, double y, double z);
void  zaxis  (double za, double ze, double zor, double zstp, 
              int nl, const char *cstr, int it, int id, int nx, int ny);
void  zaxlg  (double za, double ze, double zor, double zstp, 
              int nl, const char *cstr, int it, int id, int nx, int ny);
void  zbfers (void);
void  zbffin (void);
int   zbfini (void);
void  zbflin (double x1, double y1, double z1, double x2, double y2, double z2);
void  zbfmod (const char *copt);
void  zbfres (void);
void  zbfscl (double x);
void  zbftri (const double *x, const double *y, const double *z, const int *ic);
void  zscale (double za, double ze);

#ifdef __cplusplus
  }
#endif
