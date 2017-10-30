"""
Python code to create a MODFLOW-USG distribution

"""
import os
import sys
import shutil
import zipfile
import subprocess
import shlex

def zipdir(dirname, zipname):
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

destpath = '.'
version = 'mfusg.1_4'
dest = os.path.join(destpath, version)

print(2*'\n')
print('Creating MODFLOW-USG distribution: {}'.format(version))
print('\n')

if os.path.exists(dest):
    # Raise Exception('Destination path exists.  Kill it first.')
    print('Clobbering destination directory: {}'.format(dest))
    print('\n')
    shutil.rmtree(dest)


# Create subdirectories
binpath = os.path.join(dest, 'bin')
docpath = os.path.join(dest, 'doc')
msvspath = os.path.join(dest, 'msvs')
pymakepath = os.path.join(dest, 'pymake')
sourcepath = os.path.join(dest, 'src')
expath  = os.path.join(dest, 'test')
subdirs = [dest, binpath, docpath, msvspath, sourcepath]
print('Creating directories')
for sd in subdirs:
    print(' {}'.format(sd))
    os.mkdir(sd)
print('\n')


# Copy the executables
print('Copying mfusg executables')
bins = ['mfusg.exe', 'mfusg_x64.exe', 'zonbudusg.exe']
for b in bins:
    fname = os.path.join('..', 'bin', b)
    print('  {} ===> {}'.format(fname, os.path.join(binpath, b)))
    shutil.copy(fname, os.path.join(binpath, b))
print('\n')


# Copy the documentation
doclist = [os.path.join('..', 'doc', 'mfusgio', 'mfusg_io_v_1_4.pdf'),
           os.path.join('..', 'doc', 'tm6-A45.pdf'),
           os.path.join('..', 'doc', 'zonbudusg.pdf')]
print('Copying documentation')
for d in doclist:
    print('  {} ===> {}'.format(d, docpath))
    shutil.copy(d, docpath)
print('\n')

# Copy release notes
doclist = [os.path.join('..', 'doc', 'mfusg.txt'),
           os.path.join('..', 'doc', 'problems.txt'),
           os.path.join('..', 'doc', 'readme.txt'),
           os.path.join('..', 'doc', 'release.txt')]
print('Copying release notes')
for d in doclist:
    print('  {} ===> {}'.format(d, docpath))
    shutil.copy(d, dest)
print('\n')


# Copy the test folder to the distribution folder
print('Copying test folder')
shutil.copytree('../examples', expath, ignore=shutil.ignore_patterns('.DS_Store', 'tmp*', 'update_output*'))
print('  {} ===> {}'.format('../examples', expath))
print('\n')


# Copy the pymake folder to the distribution folder
print('Copying pymake folder')
shutil.copytree('../pymake', pymakepath, ignore=shutil.ignore_patterns('.DS_Store', 'tmp*'))
print('  {} ===> {}'.format('../pymake', pymakepath))
print('\n')

# Copy the source code
print('Copying the source code')
fnames = os.listdir('../src')
for f in fnames:
    shutil.copy(os.path.join('../src', f), os.path.join(sourcepath, f))
    print('  {} ===> {}'.format(os.path.join('../src', f), os.path.join(sourcepath, f)))
print('\n')

# Copy the zonebudget source code
print('Copying zonebudget source code')
zbdsrc = os.path.join(sourcepath, 'zonebudusg')
os.mkdir(zbdsrc)
fnames = os.listdir('../zbudusg/src')
for f in fnames:
    shutil.copy(os.path.join('../zbudusg/src', f), os.path.join(zbdsrc, f))
    print('  {} ===> {}'.format(os.path.join('../zbudusg/src', f), os.path.join(zbdsrc, f)))
print('\n')
    

# Copy Visual Studio files
print('Copying the Visual Studio file')
fnames = ['mfusg.vfproj']
for f in fnames:
    shutil.copy(os.path.join('../msvs', f), os.path.join(msvspath, f))
    print('  {} ===> {}'.format(os.path.join('../msvs', f), os.path.join(msvspath, f)))
print('\n')


# Prior to zipping, enforce windows line endings on all text files
cmd = 'for /R %G in (*) do unix2dos "%G"'
args = shlex.split(cmd)
p = subprocess.Popen(cmd, cwd=dest, shell=True)
print(p.communicate())


# Zip the distribution
zipname = version + '.zip'
if os.path.exists(zipname):
    print('Removing existing file: {}'.format(zipname))
    os.remove(zipname)
print('Creating zipped file: {}'.format(zipname))
zipdir(dest, zipname)
print('\n')

print('Done...')
print('\n')
