import os
import platform

# Autotest information
testdir = 'temp'
if not os.path.isdir(testdir):
    os.mkdir(testdir)
target_dict = {}

exclude = None #('MNW2-Fig28',) #, 'swi2ex4sww') #None
retain = False

# Compiling information
fc = 'gfortran'
# fc = 'ifort'
target_extension = ''
target_arch = 'intel64'
if platform.system() in 'Windows':
    target_extension = '.exe'

# Development version information
testpaths = [os.path.join('..', 'examples'),
             os.path.join('..', 'test-reg'),
             os.path.join('..', 'test-cmp')]
srcdir = os.path.join('..', 'src')
program = 'mfusg'
version = '1.4.00'
target = os.path.join('temp', program + '_' + version + target_extension)
target_dict[os.path.basename(target)] = target

# Released version information
url_release = 'http://water.usgs.gov/ogw/mfusg/mfusg.1_3.zip'
dir_release = os.path.join(testdir, 'mfusg.1_3')
srcdir_release = os.path.join(dir_release, 'src')
version_release = '1.3.00'
target_release = os.path.join('temp', program + '_' + version_release +
                              target_extension)
target_dict[os.path.basename(target_release)] = target_release
target_dict[program] = target_release

# Comparison information
target_dict['mfnwt'] = 'mfnwt' + target_extension
