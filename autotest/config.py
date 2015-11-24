import os
import platform

# Autotest information
testdir = 'temp'
retain = True

# Compiling information
fc = 'gfortran'
# fc = 'ifort'
target_extension = ''
target_arch = 'intel64'
if platform.system() in 'Windows':
    target_extension = '.exe'

# Development version information
testpaths = [os.path.join('..', 'examples'),
             os.path.join('..', 'test-dev')]
srcdir = os.path.join('..', 'src')
program = 'mfusg'
version = '1.3.00'
target = os.path.join('temp', program + '_' + version + target_extension)

# Release version information
url_release = 'http://water.usgs.gov/ogw/mfusg/mfusg.1_2_00.zip'
dir_release = os.path.join(testdir, 'mfusg.1_2')
srcdir_release = os.path.join(dir_release, 'src')
version_release = '1.2.00'
target_release = os.path.join('temp', program + '_' + version_release +
                              target_extension)
