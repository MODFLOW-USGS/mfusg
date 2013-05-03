# Makefile for MODFLOW-USG distribution.
# This file has only been used with the NMAKE utility
# NMAKE clean should be used prior to each build to
# avoid dependency issues with Fortran modules.
SOURCELOC = 
UTILITYLOC = 
NEWMOD = 
PROGRAM = mfusg
INCDIR= .

# Define the Fortran compiler flags

#release
F90FLAGS= -O2 -heap-arrays:0 -fpe:0 -traceback

#debug
#F90FLAGS= -debug:full -traceback -heap-arrays:0

#Fortran compiler
F90= ifort

# 
# Define the C compile flags
#CFLAGS= -D_UF -O3 
#CC= gcc
CFLAGS= 
CC= cl

# Define the libraries
USRLIB  = 

# Define all object files which make up mfusg
# Note the order of these files is important
OBJECTS = \
        gmodules.obj \
        GLO2btnU1.obj \
        gwf2chd7U1.obj \
        gwf2drn7U1.obj \
        gwf2fhb7U1.obj \
        gwf2ghb7U1.obj \
        gwf2hfb7U1.obj \
        gwf2riv7U1.obj \
        gwf2rch8U1.obj \
        gwf2evt8U1.obj \
        Lak_Gag_Sfr_Modules.obj \
        gwf2sfr7U1.obj \
        gwf2str7U1.obj \
        gwf2lak7U1.obj \
        gwf2sub7U1.obj \
        gwf2wel7U1.obj \
        gwf2gag7U1.obj \
        CLN2Props1.obj \
        gwf2basU1.obj \
        GWF2BCF-LPF-U1.obj \
        xmdlib_2.obj \
        DISU2GNCb1.obj \
        DISU2GNCn1.obj \
        xmd.obj \
        parutl7.obj \
        pcgu7.obj \
        utl7U1.obj \
        GLO2SMS-U1.obj \
        glo2basU1.obj \
        mfusg.obj \
        CLN2BasU1.obj 


install: mfusg

# Define Task Function Program Modtools

all: mfusg

# Define what mfusg is

mfusg: $(OBJECTS)
	-$(F90) $(F90FLAGS) -o mfusg $(OBJECTS) $(USRLIB) $(SYSLIBS)

# mfusg_Object codes

mfusg.obj: mfusg.f
	$(F90) $(F90FLAGS) -c mfusg.f

# Pth_Object codes of mfusg

.f.obj:
	$(F90) $(F90FLAGS) -c $<

.f90.obj:
	$(F90) $(F90FLAGS) -c $<

.c.obj:
	$(CC) $(CFLAGS) -c $<

clean:
	- del *.obj del *.mod
#
#  end
