
                      swb - version: 1.0.1
          Soil Water Balance Model for calculating Recharge

QUICK START:
------------

If you are viewing this file after running the 'swb_setup.exe' file,
presumably swb has already been successfully on your system. To run the
swb example files, click on the Windows 'Start' button, click on 'All Programs',
click on the 'Soil Water Balance' folder, and finally, click on the
'Soil Water Balance Code' menu entry. This will open up a Windows command
window in the directory which contains the swb example files. At the command
prompt type 'swb.exe recharge.ctl'; swb should begin running the example
files.

------------

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

Instructions for installation, execution, and testing of this version of
swb are provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. UNINSTALLING
                         D. EXECUTING THE SOFTWARE
                         E. TESTING
                         F. COMPILING


A. DISTRIBUTION FILE

The distribution file "swb_setup.exe" is a Windows installation
program for use on personal computers running Windows XP or Windows 7.
The installation program and swb executables *may* work with Windows versions
other than XP or 7; this has not been tested. The installation file requires
that the user has administrative rights for the computer that swb is
being installed. Users who do not possess administrative rights on their
computer systems should seek assistance from their IT support personnel.

The user will be presented with the option to install any or all of the
following components:

          1) swb documentation
          2) a swb example problem
          3) swb source code
          4) swb executables, compiled for 32-bit Windows

In addition, the user may choose to allow the installation program to
add swb to the path of the target computer system.

The default installation directory is C:\USGS\swb.  You have the opportunity
to specify an alternate installation directory during extraction of the
software. The following directory structure will be created in
the installation directory, asssuming that all package components are installed:

USGS
   |
   |--swb
       |--bin                         ; Compiled swb and swbstat executable
       |--build                       ; Empty directory structure for building
       |    |--win32                  ; from source code files
       |--doc                         ; User's manual for the swb code
       |--example                     ; Files that make up a complete test case
       |    |--climate                  ; climate data files - Dane County Reg. Airport
       |    |--input                    ; gridded input files for example
       |    |--output                   ; suggested directory structure for output
       |    |    |--annual
       |    |    |--monthly
       |    |    |--daily
       |    |    |--future
       |    |--images                   ; suggested directory structure for images
       |    |    |--annual
       |    |    |--monthly
       |    |    |--daily
       |    |--std_input
       |--include                     ; contains NetCDF and Dislin mod files
       |    |--win32                  ; needed for compiling from source
       |    |    |--gfortran_mingw
       |--lib                         ; contains NetCDF and Dislin library files
       |    |--win32                  ; needed for compiling from source
       |         |--win32
       |              |--gfortran_mingw
       |--src                         ; source code files


WE RECOMMEND THAT YOU DO NOT STORE YOUR VALUABLE FILES ANYWHERE IN
THE swb DIRECTORY; they may be deleted in the event that you run the swb
program uninstaller.  If you do plan to put your own files in the swb
directory structure, it is suggested that you place files in a seperate
subdirectory, for example "swb\data\myprojectfiles"

Included in directory swb\doc is the swb documentation
report, which is a Portable Document Format (PDF) file. The PDF file is
readable and printable on various computer platforms using Acrobat Reader
from Adobe. The Acrobat Reader is freely available from the following World
Wide Web site:
      http://www.adobe.com/


B. INSTALLING

Double-click on the installation program file 'swb_setup.exe'. On Windows 7
or later, the operating system will likely prompt you for the username and
password of someone with administrative rights. If you do not have administrative
rights on your machine, you must find someone who does in order to continue.

Click 'Next' to continue, click 'I Agree' after reading through the user
rights notice, click on the checkboxes to remove swb components and
click on 'Next', modify the default installation path if desired and
click on 'Install'. The setup program will now copy all of the selected
components to your hard drive.

The installation program will modify the PATH variable on your computer
so that swb is accessible from any directory. The discussion that follows
may be skipped if the installer is allowed to change the PATH variable
automatically.

To make the executable version of swb accessible from any
directory, the directory containing the executable (USGS\swb\bin)
should be included in the PATH environment variable (see explanation below).
Also, if a prior release of GSFLOW is installed on your system, the directory
containing the executables for the prior release should be removed from the
PATH environment variable.

As an alternative, the executable file, swb.exe, in the
swb\bin directory can be copied into a directory already included
in the PATH environment variable.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
             WINDOWS 2000 OR XP SYSTEMS

From the Start menu, select 'Settings' and then 'Control Panel'.  Double click
System and select the 'Advanced' tab.  Click on 'Environment Variables'.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click 'Edit'.  In the Edit User Variable window, add
";C:\USGS\swb\bin" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.  If
a PATH user variable is not already defined, in the User variables pane of
the Environment Variables window, click 'New'.  In the New User Variable
window, define a new variable PATH as shown above.  Click 'OK'.  Click 'OK'
in the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command Prompt window.


       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
                 WINDOWS VISTA SYSTEMS

From the Start menu, select 'Settings' and then 'Control Panel'.  Select
'System & Maintenance' followed by 'System'.  Choose the 'Advanced System'
option.  Select the 'Settings' Task, and then select the 'Environmental
Variables' button.  In the System Variables pane, select the PATH
variable followed by 'Edit'.  In the Edit window, add
";C:\USGS\swb\bin" to the end of the Variable Value (ensure
that the current contents of the User Value are not deleted) and click
'OK'. Click 'OK' in the Environment Variables window and then exit from the
control panel windows.  Initiate and use a new Windows Command window.

       HOW TO ADD TO THE PATH ENVIRONMENT VARIABLE
                 WINDOWS 7 SYSTEMS

From the Start menu, select Control Panel.  Single click
System and single click on 'Advanced system settings' in the left-hand panel.
Click on 'Environment Variables' button at the bottom of the dialog box.  If
a PATH user variable already is defined, click on it in the User Variables
pane, then click 'Edit'.  In the Edit User Variable window, add
";C:\USGS\swb\bin" to the end of the Variable Value (ensure that
the current contents of the User Value are not deleted) and click OK.  If
a PATH user variable is not already defined, in the User variables pane of
the Environment Variables window, click 'New'.  In the New User Variable
window, define a new variable PATH as shown above.  Click 'OK'.  Click 'OK'
in the Environment Variables window and again in the System Properties
window.  Initiate and use a new Windows Command Prompt window.

C. UNINSTALLING

Click on the Windows 'Start' button, click on 'All Programs',
click on the 'Soil Water Balance' folder, and finally, click on the
'Uninstall' menu entry. If you're sure you want to remove swb *and* all
files within the swb directory structure, click 'Yes'; the uninstall
program will remove all example, source code, and executable files. If you
have placed your own data in a seperate subfolder (i.e. swb\data), you
should move the data to a folder outside of the swb directory and
subdirectories: the uninstaller WILL DESTROY ANY FILES that remain in the
swb directory.

D. EXECUTING THE SOFTWARE

After the executable file in the swb\bin directory is installed in
a directory that is included in your PATH, swb may be started in
a Windows Command-Prompt window using the command:

          swb.exe [filename]

The optional 'filename' argument is the name of the swb control file.  If
no argument is used, then swb will halt after printing out a brief summary
of available options, along with the compilation date and compilation options.

The arrays in swb are dynamically allocated, so models are not limited
by the size of input data. However, it is best to have at least 2 MB of
random-access memory (RAM) for model execution and more RAM for large models.
If there is less available RAM than the model requires, which depends
on the size of the application, the program will use virtual memory; however,
this can slow execution significantly. If there is insufficient memory to
run the model, then swb will not initiate the beginning of the simulation;
however, the Windows Command-Prompt window may continue to indicate that
swb is executing. For this circumstance, the program must be terminated
manually using the Windows Task Manager application.

swb writes binary output to files with the extension *.bin; these files
may be found in the 'output' subdirectory following an swb run. An experimental
Python code is included (in swb\bin) which will read the output binary
files and perform some basic analysis on them.

E. TESTING

An example problem with swb data sets is provided to verify that swb
is correctly installed and running on the system.  The example problem may
also be looked at as an example of how to use the program. A description
of the file structure for the example problem may be found in the PDF
documentation file. The example problem may be started by opening up a
Windows command prompt (i.e. on Windows 7, select 'Run' from the Start
menu, type 'cmd.exe' into the 'Run' dialog box, and press 'Enter' on
your keyboard).

Once the command window is open, change directories to the location where
swb is installed and navigate to the 'example' subdirectory. After navigating
to the 'example' subdirectory, the example swb run is started by typing
'swb.exe recharge.ctl'.

F. COMPILING

The executable file provided in swb\bin was created using the
GNU gfortran version 4.6.1.  Although an executable version of the program
is provided, the source code also is provided in the swb\src directory
so that swb can be recompiled if necessary.  However, the USGS cannot
provide assistance to those compiling swb. In general, you must have a copy
of GNU gfortran. On Windows it is easiest to grab a precompiled version.
See http://gcc.gnu.org/wiki/GFortranBinaries#Windows

You'll also need to know about the use of the GNU compilers. Makefiles
are included in the swb\build\win32 directory as an example for compiling
swb. In addition, the CMake build system is used for compiling swb; in
theory this should make it easier for others to compile on their own systems.
See http://www.cmake.org/ for more information.

One tip regarding compilation: unless you use the *exact* same version of
GNU gfortran as is mentioned above, you will certainly need to recompile the
associated library Fortran 90 module files before attemting to compile swb.
This may be done within the 'include\win32\mingw_gfortran' subdirectory using
the following command syntax: 'gfortran -c netcdf.F90'.
