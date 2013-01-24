# Makefile example for compiling GMG and linking GMG to MF2K:

# Makefile for USGS distribution
#
#
# SHELL=/bin/ksh
SOURCELOC = 
UTILITYLOC = 
NEWMOD = 
PROGRAM = mfusg
INCDIR= .

# Define the Fortran compiler flags

#F90FLAGS= -O3 -tpp7 -axW -Vaxlib -cm

#F90= ifc

#F90FLAGS= -O2 -fno-second-underscore -r8
#F90FLAGS= -g -fbounds-check -fno-second-underscore -Wall -ftrace=full -r8
#F90FLAGS= -g -fbounds-check -fno-second-underscore -Wall -ftrace=full
#F90FLAGS= -O2 -fno-second-underscore 
#F90= g95a

#F90FLAGS= -O2 -fno-second-underscore -fdefault-real-8
#F90FLAGS= -O2 -fno-second-underscore 
#F90FLAGS= -g -fbounds-check -fno-second-underscore -Wall -ffpe-trap=invalid,zero 
#F90= gfortran

#release
F90FLAGS= -O2 -heap-arrays:500 -fpe:0
#debug
#F90FLAGS= -debug:full -traceback -heap-arrays:0

F90= ifort


# 
# Define the C compile flags
# -D_UF defines UNIX naming conventions for mixed language compilation.
# 
#CFLAGS= -D_UF -O3 -ansi -pedantic
#CFLAGS= -D_UF -O3 
#CC= gcc
CFLAGS= 
CC= cl

# Define the libraries

#SYSLIBS= -lmisalign -ldgc -lm 
#SYSLIBS= -lc
USRLIB  = 

# Define all object files which make up mfusg

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
