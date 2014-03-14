#! /usr/bin/env python
"""
Make the binary executable for MODFLOW-USG.
"""
__author__ = "Christian D. Langevin"
__date__ = "March 14, 2014"
__version__ = "1.0.0"
__maintainer__ = "Christian D. Langevin"
__email__ = "langevin@usgs.gov"
__status__ = "Production"

import os
import subprocess
import shutil
import sys
from dag import order_source_files

def compilewin(srcfiles, fc, compileflags, target, makeclean, platform):
    """
    Make target on Windows OS
    """
    if 'ifort' in fc:
        cpvars = os.environ.get('IFORT_COMPILER13') + 'bin/compilervars.bat'
        #cpvars = 'C:/Program Files (x86)/Intel/Composer XE 2013/bin/compilervars.bat'
    f = open('compileusg.bat', 'w')
    line = 'call ' + '"' + os.path.normpath(cpvars) + '" ' + platform + '\n'
    f.write(line)

    #build object files
    for srcfile in srcfiles:
        cmd = fc + ' '
        for switch in compileflags:
            cmd += switch + ' '
        cmd += '-c' + ' '
        cmd += srcfile
        cmd += '\n'
        f.write(cmd)

    #build executable
    cmd = fc + ' '
    for switch in compileflags:
        cmd += switch + ' '
    cmd += '-o' + ' ' + target + ' ' + '*.obj' + '\n'
    f.write(cmd)
    f.close()

    #run the batch file
    subprocess.check_call(['compileusg.bat', ],)

    #clean things up
    if makeclean:
        print 'making clean...'
        filelist = os.listdir('.')
        delext = ['.mod', '.obj']
        for f in filelist:
            for ext in delext:
                if f.endswith(ext):
                    os.remove(f)
    return

def compilemac(srcfiles, fc, compileflags, target, makeclean):
    """
    Make target on Mac OS
    """

    #build object files
    objfiles = []
    for srcfile in srcfiles:
        cmdlist = []
        cmdlist.append(fc)
        for switch in compileflags:
            cmdlist.append(switch)
        cmdlist.append('-c')
        cmdlist.append(srcfile)
        print 'check call: ', cmdlist
        subprocess.check_call(cmdlist)
        srcname, srcext = os.path.splitext(srcfile)
        srcname = srcname.split(os.path.sep)[-1]
        objfiles.append(srcname + '.o')

    #build executable
    cmd = fc + ' '
    cmdlist = []
    cmdlist.append(fc)
    for switch in compileflags:
        cmd += switch + ' '
        cmdlist.append(switch)
    cmd += '-o' + ' ' + target + ' ' + '*.obj'
    cmdlist.append('-o')
    cmdlist.append(os.path.join('.',target))
    for objfile in objfiles:
        cmdlist.append(objfile)
    print 'check call: ', cmdlist
    subprocess.check_call(cmdlist)

    #clean things up
    if makeclean:
        print 'making clean...'
        filelist = os.listdir('.')
        delext = ['.mod', '.o']
        for f in filelist:
            for ext in delext:
                if f.endswith(ext):
                    os.remove(f)

    return

def main():
    """
    Create the binary executable(s)
    """
    srcdir = '../src'
    target = 'mfusg'
    makeclean = True
    srcfiles = ['gmodules.f',
                 'sparse.f',
                 'glo2btnu1.f', 
                 'gwf2chd7u1.f', 
                 'gwf2drn7u1.f', 
                 'gwf2fhb7u1.f', 
                 'gwf2ghb7u1.f', 
                 'gwf2hfb7u1.f', 
                 'gwf2riv7u1.f', 
                 'gwf2rch8u1.f', 
                 'gwf2evt8u1.f', 
                 'lak_gag_sfr_modules.f', 
                 'gwf2sfr7u1.f', 
                 'gwf2str7u1.f', 
                 'gwf2lak7u1.f', 
                 'gwf2sub7u1.f', 
                 'gwf2wel7u1.f', 
                 'gwf2gag7u1.f', 
                 'cln2props1.f', 
                 'gwf2basu1.f', 
                 'gwf2bcf-lpf-u1.f', 
                 'xmdlib_2.f', 
                 'disu2gncb1.f', 
                 'disu2gncn1.f', 
                 'xmd.f', 
                 'parutl7.f', 
                 'pcgu7.f', 
                 'utl7u1.f', 
                 'glo2sms-u1.f', 
                 'glo2basu1.f', 
                 'mfusg.f', 
                 'cln2basu1.f' 
                 ]

    srcfileswithpath = []
    for srcfile in srcfiles:
        s = os.path.join(srcdir, srcfile)
        srcfileswithpath.append(s)

    #order the source files using the directed acyclic graph in dag.py
    orderedsourcefiles = order_source_files(srcfileswithpath)

    platform = sys.platform
    if platform.lower() == 'darwin':
        fc = 'gfortran'
        compileflags = []
        try:
            compilemac(orderedsourcefiles, fc, compileflags, target, makeclean)
        except:
            print 'Error.  Could not build target...'
       
    else:
        fc = 'ifort.exe'
        if platform == 'ia32_intel64' or platform == 'intel64':
           target += '_x64'
        compileflags = [
                       '-O2',
                       '-heap-arrays:0',
                       '-fpe:0',
                       '-traceback',
                       ]
        #create a 32-bit executable
        try:
            compilewin(orderedsourcefiles, fc, compileflags, target, makeclean,
                  'ia32')
        except:
            print 'Error.  Could not build 32-bit target...'

        #create a 64-bit executable
        try:
            compilewin(orderedsourcefiles, fc, compileflags, target+'_x64', 
                  makeclean, 'intel64')
        except:
            print 'Error.  Could not build 64-bit target...'
                  
    print 'Done...'
    return

if __name__ == "__main__":    
    main()



    