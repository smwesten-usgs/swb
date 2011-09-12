
                      swb - version: 1.0.1
          Soil Water Balance Model for calculating Recharge


NOTE: Any use of trade, product or firm names is for descriptive purposes
      only and does not imply endorsement by the U.S. Government.

swb version 1.0.1 is packaged for personal computers using one of the
Microsoft Windows operating systems. An executable file is provided as
well as the source code. The executable file was compiled on a personal
computer with the Intel(R) Core(TM)2 Duo CPU T9500 chipset, running the
Microsoft Windows 7 Professional operating system, using GNU gfortran
version 4.6.1. The source code and Makefiles are provided to aid users
in compilation on other computers. However, no support is provided
for compilation.

IMPORTANT: Users should review the file Summary_swb.txt for a description
of, and references for, this software. Users should also review the file
release.txt, which describes changes that have been introduced into swb
with each official release; these changes may substantially affect users.

Instructions for installation, execution, and testing of this version of
swb are provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING THE SOFTWARE
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following self-extracting distribution file is for use on personal
computers:

         swb.exe

The distribution file contains:

          GSFLOW documentation.
          Related documentation for PRMS and MODFLOW.
          A GSFLOW example problem.

The distribution file is a self-extracting program for use on personal
computers running a Windows XP or greater operating system.  Execution
of the distribution file creates numerous individual files contained in
several directories. The extraction program allows you to specify the
directory in which the files should be restored.  The default installation
directory is C:\WRDAPP.  You have the opportunity to specify an alternate
installation directory during extraction of the software. The following
directory structure will be created in the installation directory:


   |
   |--GSFLOW_1.1.4
   |    |--bin           ; Compiled GSFLOW executable for personal computers
   |    |--data
   |         |--sagehen  ; Input and output files for a GSFLOW sample problem
   |    |--doc           ; Written documentation
   |        |--GSFLOW    ; GSFLOW docmentation report and additional
   |                       references
   |        |--MODFLOW   ; MODFLOW-2005 documentation report and additional
   |                       references for the SFR2 and UZF Packages
   |        |--PRMS      ; PRMS and MMS documentation reports
   |    |--src
   |        |--gsflow    ; Source code for GSFLOW Modules
   |        |--mms       ; Source code for MMS software
   |        |--modflow   ; Source code for MODFLOW-2005 Packages
   |        |--prms      ; Source code for PRMS Modules
   |    |--utilities     ; Utility program for analysis of GSFLOW output


It is recommended that no user files are kept in the GSFLOW_1.1.4 directory
structure.  If you do plan to put your own files in the GSFLOW_1.1.4
directory structure, do so only by creating additional subdirectories of
the GSFLOW_1.1.4\data subdirectory.

Included in directory GSFLOW_1.1.4\doc\GSFLOW is the GSFLOW documentation
report, which is a Portable Document Format (PDF) file. The PDF file is
readable and printable on various computer platforms using Acrobat Reader
from Adobe. The Acrobat Reader is freely available from the following World
Wide Web site:
      http://www.adobe.com/


B. INSTALLING

To make the executable version of GSFLOW accessible from any
directory, the directory containing the executable (GSFLOW_1.1.4\bin)
should be included in the PATH environment variable (see explanation below).
Also, if a prior release of GSFLOW is installed on your system, the directory
containing the executables for the prior release should be removed from the
PATH environment variable.

As an alternative, the executable file, gsflow.exe, in the
GSFLOW_1.1.4\bin directory can be copied into a directory already included
in the PATH environment variable.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 2000 OR XP SYSTEMS

From the Start menu, select Settings and then Control Panel.  Double click
System and select the Advanced tab.  Click on Environment Variables.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click Edit.  In the Edit User Variable window, add
";C:\WRDAPP\GSFLOW_1.1.4\bin" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.  If
a PATH user variable is not already defined, in the User variables pane of
the Environment Variables window, click New.  In the New User Variable
window, define a new variable PATH as shown above.  Click OK.  Click OK
in the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command Prompt window.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
                 WINDOWS VISTA SYSTEMS

From the Start menu, select Settings and then Control Panel.  Select
System & Maintenance followed by System.  Choose the Advanced System
option.  Select the Settings Task, and then select the Environmental
Variables button.  In the System Variables pane, select the PATH
variable followed by Edit.  In the Edit window, add
";C:\WRDAPP\GSFLOW_1.1.4\bin" to the end of the Variable Value (ensure
that the current contents of the User Value are not deleted) and click
OK. Click OK in the Environment Variables window and then exit from the
control panel windows.  Initiate and use a new Windows Command window.


C. EXECUTING THE SOFTWARE

After the executable file in the GSFLOW_1.1.4\bin directory is installed in
a directory that is included in your PATH, GSFLOW is initiated in
a Windows Command-Prompt window using the command:

          gsflow.exe [Fname]

The optional Fname argument is the name of the GSFLOW Control File.  If
no argument is used, then GSFLOW will look for a Control File named
"control" in the user’s current directory.

The arrays in GSFLOW are dynamically allocated, so models are not limited
by the size of input data. However, it is best to have at least 2 MB of
random-access memory (RAM) for model execution and more RAM for large models.
If there is less available RAM than the model requires, which depends
on the size of the application, the program will use virtual memory; however,
this can slow execution significantly. If there is insufficient memory to
run the model, then GSFLOW will not initiate the beginning of the simulation;
however, the Windows Command-Prompt window may continue to indicate that
GSFLOW is executing. For this circumstance, the program must be terminated
manually using the Windows Task Manager application.

Some of the files written by GSFLOW are unformatted files. The structure
of these files depends on the compiler and options in the code. GSFLOW is
compiled with the unformatted file type specified as "BINARY". Any program
that reads the unformatted files produced by GSFLOW must be compiled with a
compiler that produces programs that use the same structure for unformatted
files.  For example, Zonebudget and Modpath use unformatted budget files
produced by the MODFLOW component of GSFLOW. Another example are head files
that are generated by one GSFLOW simulation and used in a following simulation
as initial heads. Both simulations must be run using an executable version
of GSFLOW that uses the same unformatted file structure.


D. TESTING

An example problem with GSFLOW data sets is provided to verify that GSFLOW
is correctly installed and running on the system.  The example problem may
also be looked at as an example of how to use the program. A description
of the file structure for the example problem and of directions for
running the example problem are described in the 'Readme.sagehen.txt' file
located in directory GSFLOW_1.1.4\data\sagehen.


E. COMPILING

The executable file provided in GSFLOW_1.1.4\bin was created using the Intel
Visual Fortran 10.1 and Microsoft Visual C++ 2005 compilers.  Although an
executable version of the program is provided, the source code also is
provided in the GSFLOW_1.1.4\src directory so that GSFLOW can be recompiled
if necessary.  However, the USGS cannot provide assistance to those
compiling GSFLOW. In general, the requirements are a Fortran compiler, a
compatible C compiler, and the knowledge of using the compilers. Makefiles
are included in the GSFLOW_1.1.4\src directories as an example for compiling
GSFLOW.
