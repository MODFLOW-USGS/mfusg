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
    return

def main():
    """
    Create the binary executable(s)
    """
    
    makeclean = True
    targetpth = os.path.join('..', 'bin')
    target = os.path.join(targetpth, 'mfusg')

    #remove the target if it already exists
    try:
        os.remove(target)
    except:
        pass    
    
    #copy the original source to a src directory
    srcdir_origin = os.path.join('..', 'src')
    try:
        shutil.rmtree('src')
    except:
        pass
    shutil.copytree(srcdir_origin, 'src')
    srcdir_temp = os.path.join('.', 'src')
        
    #create a list of all f and f90 source files
    templist = os.listdir(srcdir_temp)
    srcfiles = []
    for f in templist:
        if f.endswith('.f') or f.endswith('.f90'):
            srcfiles.append(f)
            
    srcfileswithpath = []
    for srcfile in srcfiles:
        s = os.path.join(srcdir_temp, srcfile)
        srcfileswithpath.append(s)

    #order the source files using the directed acyclic graph in dag.py
    orderedsourcefiles = order_source_files(srcfileswithpath)

    platform = sys.platform
    if platform.lower() == 'darwin':
        fc = 'gfortran'
        compileflags = ['-O2']
        
        #need to change openspec.inc
        fname = os.path.join(srcdir_temp, 'openspec.inc')
        f = open(fname, 'w')
        f.write(
'''c -- created by makebin.py   
      CHARACTER*20 ACCESS,FORM,ACTION(2)
      DATA ACCESS/'STREAM'/
      DATA FORM/'UNFORMATTED'/
      DATA (ACTION(I),I=1,2)/'READ','READWRITE'/
c -- end of include file
'''
        )
        f.close()

        try:
            compilemac(orderedsourcefiles, fc, compileflags, target, makeclean)
        except:
            print 'Error.  Could not build target...'
       
    else:
        fc = 'ifort.exe'
        #production version compile flags
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
                  
    #clean things up
    if makeclean:
        print 'making clean...'
        filelist = os.listdir('.')
        delext = ['.mod', '.o']
        for f in filelist:
            for ext in delext:
                if f.endswith(ext):
                    os.remove(f)
        shutil.rmtree(srcdir_temp)

    print 'Done...'
    return

if __name__ == "__main__":    
    main()



    