"""
Python code to create a MODFLOW-USG distribution.  This has been used mostly
on Windows and requires Python with the pymake package.

To make a distribution:
  1.  Create a release branch
  3.  Run this script
  4.  Post the distribution zip file
  5.  Merge the release changes into the master branch
  6.  Tag the master branch with the correct version
  7.  Merge master into develop

"""


import sys
import os
import shutil
import subprocess
import zipfile
from contextlib import contextmanager
import pymake


@contextmanager
def cwd(path):
    oldpwd=os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(oldpwd)


def zipdir(dirname, zipname):
    """
    Create a zipfile of dirname

    """
    print('Zipping directory: {}'.format(dirname))
    zipf = zipfile.ZipFile(zipname, 'w', zipfile.ZIP_DEFLATED)
    for root, dirs, files in os.walk(dirname):
        for file in files:
            if '.DS_Store' not in file:
                fname = os.path.join(root, file)
                print('  Adding to zip: ==> ', fname)
                zipf.write(fname, arcname=fname)
    zipf.close()
    print('\n')
    return


def copytree(src, dst, symlinks=False, ignore=None):
    """
    Copy a folder from src to dst.  If dst does not exist, then create it.

    """
    if not os.path.isdir(dst):
        os.mkdir(dst)

    for item in os.listdir(src):
        s = os.path.join(src, item)
        d = os.path.join(dst, item)
        if os.path.isdir(s):
            print('  copying {} ===> {}'.format(s, d))
            shutil.copytree(s, d, symlinks, ignore)
        else:
            print('  copying {} ===> {}'.format(s, d))
            shutil.copy2(s, d)
    return


def convert_line_endings(folder, windows=True):
    """
    Convert all of the line endings to windows or unix

    """
    # Prior to zipping, enforce os line endings on all text files
    print(2 * '\n')
    print('Converting line endings...')
    print('\n')

    platform = sys.platform
    cmd = None
    if platform.lower() == 'darwin':
        if windows:
            cmd = "find . -name '*' | xargs unix2dos"
        else:
            cmd = "find . -name '*' | xargs dos2unix"
    else:
        if windows:
            cmd = 'for /R %G in (*) do unix2dos "%G"'
        else:
            cmd = 'for /R %G in (*) do dos2unix "%G"'
    p = subprocess.Popen(cmd, cwd=folder, shell=True)
    print(p.communicate())
    print('\n')
    return


def distribution_setup(name, dest, subdirs):
    """
    Setup the folder structure, and return a dictionary of subfolder name
    and the full path in destpath.

    """
    print(2 * '\n')
    print('Setting up distribution: {}'.format(name))
    print('\n')

    if os.path.exists(dest):
        print('Clobbering destination directory: {}'.format(dest))
        print('\n')
        shutil.rmtree(dest)
    os.mkdir(dest)

    print('Creating subdirectories')
    folderdict = {}
    for sd in subdirs:
        fullpath = os.path.join(dest, sd)
        print('  creating ==> {}'.format(fullpath))
        os.mkdir(fullpath)
        folderdict[sd] = fullpath
    print('\n')

    return folderdict


if __name__ == '__main__':

    # Define distribution information and setup the folder structure
    win_target_os = False
    distribution_program = 'mfusg'
    distribution_version = '1_5'
    distribution_name = distribution_program + distribution_version
    distribution_path = os.path.join('.', distribution_name)
    distribution_subdirs = ['bin', 'doc', 'msvs', 'make', 'src', 'test']
    folder_dict = distribution_setup(distribution_name, distribution_path,
                                     distribution_subdirs)

    # Copy the MODFLOW-USG source folders
    print(2 * '\n')
    print('Copying the MODFLOW-USG source')
    print('\n')
    src = '../src'
    dst = folder_dict['src']
    copytree(src, dst)

    # Create a makefile for mfusg
    print(2 * '\n')
    print('Creating make file')
    print('\n')
    makedir = folder_dict['make']
    with cwd(makedir):
        src = '../src'
        pymake.main(src, 'mfusg', 'gfortran', 'gcc',
                    makeclean=True, dryrun=True, include_subdirs=False,
                    makefile=True, extrafiles=None)

    # Build the MODFLOW-USG executable
    print(2 * '\n')
    print('Build MODFLOW-USG executable')
    print('\n')
    srcdir = folder_dict['src']
    target = os.path.join(folder_dict['bin'], distribution_program)
    if win_target_os:
        fc = 'ifort'
        cc = 'cl'
    else:
        fc = 'gfortran'
        cc = 'gcc'
    pymake.main(srcdir, target, fc, cc, makeclean=True, include_subdirs=False)
    if win_target_os:
        target += '.exe'
    if not os.path.isfile(target):
        raise Exception('Did not build target: {}'.format(target))

    # Copy the ZONEBUDUSG source files
    print(2 * '\n')
    print('Copy zonebudusg source files')
    print('\n')
    src = '../zbudusg/src'
    dst = os.path.join(dst, 'zonebudusg')
    copytree(src, dst)

    # Compile the ZONBUDUSG code
    print(2 * '\n')
    print('Build zonebudusg executable')
    print('\n')
    srcdir = os.path.join(folder_dict['src'], 'zonebudusg')
    target = os.path.join(folder_dict['bin'], 'zonbudusg')
    pymake.main(srcdir, target, fc, cc, makeclean=True, include_subdirs=False)
    if win_target_os:
        target += '.exe'
    if not os.path.isfile(target):
        raise Exception('Did not build target: {}'.format(target))

    # Copy documentation files
    print(2 * '\n')
    print('Copy documentation files')
    print('\n')
    documentation_files = ['tm6-A45.pdf', 'zonbudusg.pdf',
                           'mfusgio/mfusg_io_v_1_4.pdf']
    for docfile in documentation_files:
        docfile_wpath = os.path.join('../doc', docfile)
        dest = folder_dict['doc']
        print('  copying {} ===> {}'.format(docfile_wpath, dest))
        shutil.copy2(docfile_wpath, dest)

    # Copy readme text files
    print(2 * '\n')
    print('Copy readme text files')
    print('\n')
    documentation_files = ['mfusg.txt', 'problems.txt', 'readme.txt',
                           'release.txt']
    for docfile in documentation_files:
        docfile_wpath = os.path.join('../doc', docfile)
        dest = distribution_path
        print('  copying {} ===> {}'.format(docfile_wpath, dest))
        shutil.copy2(docfile_wpath, dest)

    # Copy Visual Studio project file
    print(2 * '\n')
    print('Copying visual studio project file')
    print('\n')
    vsprojfile = '../msvs/mfusg.vfproj'
    dest = folder_dict['msvs']
    print('  copying {} ===> {}'.format(vsprojfile, dest))
    shutil.copy2(vsprojfile, dest)

    # Copy the examples
    print(2 * '\n')
    print('Copying the examples')
    print('\n')
    example_subdirs = [dirname for dirname in os.listdir('../examples')
                       if os.path.isdir('../examples/' + dirname)]
    for example in example_subdirs:
        src = os.path.join('../examples', example)
        dst = os.path.join(folder_dict['test'], example)
        copytree(src, dst)

    # Prior to zipping, enforce os line endings on all text files
    print(2 * '\n')
    print('Converting line endings')
    print('\n')
    windows_line_endings = win_target_os
    convert_line_endings(distribution_path, windows_line_endings)

    # Run the examples and save the output
    print(2 * '\n')
    print('Running the examples')
    print('\n')
    update_python_file = '../examples/update_output.py'
    dest = folder_dict['test']
    print('  copying {} ===> {}'.format(update_python_file, dest))
    shutil.copy2(update_python_file, dest)
    cmd = 'python update_output.py'
    p = subprocess.Popen(cmd, cwd=folder_dict['test'], shell=True)
    print(p.communicate())
    return_code = p.returncode
    msg = 'Updating the examples failed.  Return code = {}'.format(return_code)
    assert return_code == 0, msg

    # Zip the distribution
    uflag = 'u'
    if win_target_os:
        uflag = ''
    zipname = distribution_name + uflag + '.zip'
    print(2 * '\n')
    print('Creating zip file {}'.format(zipname))
    print('\n')
    if os.path.exists(zipname):
        print('Removing existing file: {}'.format(zipname))
        os.remove(zipname)
    print('Creating zipped file: {}'.format(zipname))
    zipdir(distribution_path, zipname)
    print('\n')

    print('Done...')
    print('\n')
