README.TXT


                  MODFLOW-USG - Version: 1.2.00
 Three-dimensional, unstructured, finite-difference groundwater flow model


NOTE: Any use of trade, product or firm names is for descriptive purposes 
      only and does not imply endorsement by the U.S. Government.

This version of MODFLOW is referred to as MODFLOW-USG in order to distinguish
it from other versions of the code. This version of MODFLOW-USG is packaged 
for personal computers using the Microsoft Windows XP or 7 operating
systems.  Executable files for personal computers are provided as well as the
source code.  The source code can be compiled to run on other computers.

IMPORTANT: Users should review the file mfusg.txt for a description of, and
references for, this software. Users should also review the file release.txt,
which describes changes that have been introduced into MODFLOW-USG with each
official release; these changes may substantially affect users.

Instructions for installation, execution, and testing of MODFLOW-USG are
provided below.



                            TABLE OF CONTENTS

                         A. DISTRIBUTION FILE
                         B. INSTALLING
                         C. EXECUTING MODFLOW-USG
                         D. TESTING
                         E. COMPILING


A. DISTRIBUTION FILE

The following distribution file is for use on personal computers:

         mfusg_v1_2.zip

The distribution file contains:

          Compiled runfiles and source code for MODFLOW-USG.
          Supplementary MODFLOW-USG documentation in PDF and text files.
          Test data sets.

The distribution file is a zip file, which can be unzipped to create
numerous individual files.  The following directory structure
will be created when the distribution file is unzipped:


 |
 |--mfusg.1_2
    |--bin            ; MODFLOW-USG executables for personal computers
    |--doc            ; Documentation files
    |--msvs           ; Microsoft Visual Studio Project files
    |--pymake         ; A python script for compiling MODFLOW-USG
    |--src            ; MODFLOW-USG source code
       |zonebudusg    ; Source for zonebudget post processing program
    |--test           ; Input and output files for verification tests
       |--01A_nestedgrid_nognc; Nested grid test problem
          |--output   ; Results
          |--zonbudusg; Files for running Zonebudget
             |--output; Zonebudget Results
       |--01B_nestedgrid_gnc; Nested grid test problem with gnc
          |--output   ; Results
          |--zonbudusg; Files for running Zonebudget
             |--output; Zonebudget Results
       |--02_quadtree ; **NOT INCLUDED HERE.  AVAILABLE AT: ** 
                        http://water.usgs.gov/ogw/mfusg/02_quadtree.zip
          |--arrays   ; external arrays needed to run simulation
          |--input    ; model input files
          |--output   ; folder where simulation output will be created
          |--output_bak; Provided simulation output
       |--03_conduit_confined 
          |--output   ; Results
       |--03A_conduit_unconfined
          |--output   ; Results
       |--03B_conduit_unconfined
          |--output   ; Results
       |--03C_conduit_unconfined
          |--output   ; Results
       |--03D_conduit_unconfined
          |--output   ; Results


It is recommended that no user files are kept in the mfusg.1_2 directory
structure.  If you do plan to put your own files in the mfusg.1_2
directory structure, do so only by creating additional subdirectories.

Included in directory mfusg.1_2\doc are various documentation files.  Some
of them are Portable Document Format (PDF) files. The PDF files are readable
and printable on various computer platforms using Acrobat Reader from Adobe.
The Acrobat Reader is freely available from the following World Wide Web
site:
      http://www.adobe.com/


B. INSTALLING

To make the executable versions of MODFLOW-USG accessible from any
directory, the directory containing the executables (mfusg.1_2\bin)
should be included in the PATH environment variable.  Also, if a
prior release of MODFLOW-USG is installed on your system, the
directory containing the executables for the prior release should
be removed from the PATH environment variable.

As an alternative, the executable files, mfusg.exe and zonbudusg.exe,
in the mfusg.1_2\bin directory can be copied into a directory already
included in the PATH environment variable.


C. EXECUTING MODFLOW-USG

Two MODFLOW-USG runfiles for use on personal computers are provided. 
mfusg.exe is 32-bit version of MODFLOW-USG whereas mfusg_x64.exe is a
64-bit version that may not run on some computers.  Bot versions use 
mixed single and double precision for computations and internal data 
storage, which was determined to be useful for a wide range of 
simulations.  

After the executable files in the mfusg.1_2\bin directory are installed
in a directory that is included in your PATH, MODFLOW-USG is initiated in
a Windows Command-Prompt window using one of the following commands:

          mfusg [Fname]
or
          mfusg_x64 [Fname]

The optional Fname argument is the name file.  If no argument is used,
the user is prompted to enter the name file.  If the name file ends in
".nam", then the file name can be specified without including ".nam". 
For example, if the name file is named abc.nam, then the simulation can
be run using mixed precision by entering:

          mfusg abc

The data arrays in MODFLOW-USG are dynamically allocated, so models
are not limited by hard-coded array limits. However, it is best to have
enough random-access memory (RAM) available to hold all of the required
data.  If there is less available RAM than this, the program will use
virtual memory, but this slows computations significantly.

Some of the files written by MODFLOW-USG are unformatted files.  The 
structure of these files depends on the precision of the data in the 
program, the compiler, and options in the Fortran write statement.  Any 
program that use the unformatted files produced by MODFLOW must read the 
files in the same way they were written. For example, Zonebudget
uses unformatted budget files produced by MODFLOW.  Current versions of
Zonebudget automatically detect the precision of the data in
unformatted files and the runfiles provided by the USGS are compatible
with the structure of the unformatted files produced by this release of
MODFLOW-USG.

Another example of unformatted files is head files that are generated by
one MODFLOW simulation and used in a following simulation as initial
heads.  Both simulations must be run using an executable version of
MODFLOW that uses the same unformatted file structure.  MODFLOW-USG does
not automatically detect precision of the data in these files, so both
simulations must be run using a runfile having the same precision.

This issue of unformatted files is described here so that users will
be aware of the possibility of problems caused by unformatted files. 


D. TESTING

Test data sets are provided to verify that MODFLOW-USG is correctly
installed and running on the system.  The tests may also be looked
at as examples of how to use the program.  The directory
mfusg.1_2\test contains the input data for running each test and the
output data, for comparison. The tests are described in the file 
problems.txt.

The directory mfusg.1_2\test can be used to conveniently run the
tests without destroying the original results in the MFusg.1_2\test-out
directory.  The test directory contains MODFLOW name files, which end
with ".nam", for running the tests.  Each test can be run by executing
the run.bat file contained in the folder.


E. COMPILING

The executable files provided in mfusg.1_2\bin were created using the Intel
Visual Fortran 13.1 compiler.  Although executable versions of the program 
are provided, the source code is provided in the mfusg.1_2\src directory so 
that MODFLOW-USG can be recompiled if necessary.  However, the USGS cannot 
provide assistance to those compiling MODFLOW-USG. In general, the 
requirements are a Fortran compiler and the knowledge of using the compilers.

The mfusg executable provided here is compiled with the following Intel
compiler switches:
  -O2 -heap-arrays:0 -fpe:0 -traceback


