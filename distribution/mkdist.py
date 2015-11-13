"""
Python code to create a MODFLOW-USG distribution

"""
import os
import sys
import shutil
import zipfile

def zipdir(dirname, zipname):
    zipf = zipfile.ZipFile(zipname, 'w')
    for root, dirs, files in os.walk(dirname):
        for file in files:
            zipf.write(os.path.join(root, file))
    zipf.close()
    return

destpath = '.'
version = 'mfusg.1_3'
dest = os.path.join(destpath, version)

print 2*'\n'
print 'Creating MODFLOW-USG distribution: {}'.format(version)
print '\n'

if os.path.exists(dest):
    # Raise Exception('Destination path exists.  Kill it first.')
    print 'Clobbering destination directory: {}'.format(dest)
    print '\n'
    shutil.rmtree(dest)


# Create subdirectories
binpath = os.path.join(dest, 'bin')
docpath = os.path.join(dest, 'doc')
msvspath = os.path.join(dest, 'msvs')
pymakepath = os.path.join(dest, 'pymake')
sourcepath = os.path.join(dest, 'src')
expath  = os.path.join(dest, 'test')
subdirs = [dest, binpath, docpath, msvspath, sourcepath]
print 'Creating directories'
for sd in subdirs:
    print ' {}'.format(sd)
    os.mkdir(sd)
print '\n'


# Copy the executables
print 'Copying mfusg executables'
bins = ['mfusg.exe', 'mfusg_x64.exe', 'zonbudusg.exe']
for b in bins:
    fname = os.path.join('..', 'bin', b)
    shutil.copy(fname, os.path.join(binpath, b))
print '  {} ===> {}'.format(fname, os.path.join(binpath, b))
print '\n'


# Copy the documentation
doclist = [os.path.join('..', 'doc', 'mfusgio', 'mfusg_io_v_1_3.pdf'),
		   os.path.join('..', 'doc', 'tm6-A45.pdf'),
		   os.path.join('..', 'doc', 'zonbudusg.pdf')]
print 'Copying documentation'
for d in doclist:
	print '  {} ===> {}'.format(d, docpath)	
	shutil.copy(d, docpath)
print '\n'


# Copy the test folder to the distribution folder
print('Copying test folder')
shutil.copytree('../test', expath)
print('  {} ===> {}'.format('../test', expath))
print('\n')


# Copy the pymake folder to the distribution folder
print('Copying pymake folder')
shutil.copytree('../pymake', pymakepath)
print('  {} ===> {}'.format('../pymake', pymakepath))
print('\n')


# Zip the distribution
zipname = version + '.zip'
if os.path.exists(zipname):
    print 'Removing existing file: {}'.format(zipname)
    os.remove(zipname)
print 'Creating zipped file: {}'.format(zipname)
zipdir(dest, zipname)
print '\n'

print 'Done...'
print '\n'