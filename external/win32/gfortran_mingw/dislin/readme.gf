********************************************************************
**                                                                **
**               IBM-PC Installation of DISLIN                    **
**                                                                **
**  Contents:  1.)  Introduction                                  **
**             2.)  Installation of DISLIN                        **
**             3.)  Getting a DISLIN License                      **
**                                                                **
**  Date   :   15.12.2009                                         **
**  Version:   10.0  /  Windows, GFORTRAN (C, F77, F95)           **
**  Author :   Helmut Michels                                     **
**             MPI fuer Sonnensystemforschung,                    **
**             Max-Planck-Str. 2, 37191 Katlenburg-Lindau,        **
**             Germany                                            **
**             E-mail: michels@mps.mpg.de                         **
********************************************************************

1.)  Introduction

     This file describes the  installation  of the  data  plotting
     software DISLIN on IBM-PCs  for Windows and the GFortran com-
     piler.


2.)  Installation of DISLIN

     The DISLIN version for the GFortran compiler is distributed in
     the  zipped file  dl_10_gf.zip.  An utility for unpacking the
     DISLIN  files is available  from the  same location where you
     have downloaded  dl_10_gf.zip, or from the DISLIN CD-ROM. 

     To install DISLIN, make the following steps:

  a) Create a temporary directory and copy the files dl_10_gf.zip
     and unzip.exe to it:

     for example:   md  c:\temp
                    cd  c:\temp
                    copy e:\dislin\windows\unzip.exe    *.*
                    copy e:\dislin\windows\dl_10_gf.zip *.*

  b) Extract the DISLIN files with the command:

                 unzip  -o dl_10_gf.zip

  c) Run the setup program with the command

                  setup

     -  Choose OK

     -  Give the Installation Directory where  DISLIN  should be in-
        stalled. The default directory is C:\DISLIN.

  d) Reconfigure the System

     Set the DISLIN environment variable to c:\dislin and include
     c:\dislin\win in your path. If you have installed DISLIN in a
     different directory, you have to use that directory for the 
     environment variables above.

     The setting of environment depends on your Windows version:

     Windows 98/ME

        Add the following commands to the autoexec.bat file

        PATH=c:\dislin\win;%PATH%
        SET DISLIN=c:\dislin   
   
     Windows NT/2000/XP/Vista

        Environment variables can be set or modified with the
        Control Panel 
        (see Control Panel -> System -> Advanced -> Environment
        Variables).
   
  e) Now you can compile,  link and run the example programs in the
     DISLIN subdirectory examples with the commands

                    clink    -a  exa_c      (gcc) 
                    dlink    -a  exa_f77    (g77)
                    f90link  -a  exa_f90    (g95)
     and
                    clink    -a  map_c      (gcc) 
                    dlink    -a  map_f77    (g77)
                    f90link  -a  map_f90    (g95)

     Notes: - clink, dlink and f90link link by default with the
              single precision library disgf.a. A double precision
              version of the library is also included (disgf_d.a). 

           -  If the pre-compiled DISLIN Fortran 90 module file 
              'dislin.mod' is not compatible to your gfortran compiler
              version, you have to recompile the file 'dislin.f90' in
              the DISLIN\gf directory with the command 
              'gfortran -c dislin.f90'.

            - The Windows API library libgdi32.a is needed by DISLIN
              and may not be included in the GFortran compiler distri-
              bution. libgdi32.a is available from the Mingw site
              http://www.mingw.org in the file winapi-3.5.tar.gz. 
              libgdi32.a can be copied to the lib subdirectory of the
              GFortran directory.               


3.)  Getting a DISLIN License

     DISLIN is free for non-commercial use. Licenses for commercial
     use, or for just supporting DISLIN, are available from the site 
     http://www.dislin.de. 

