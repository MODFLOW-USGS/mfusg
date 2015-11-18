from __future__ import print_function
import os
import shutil

# dstpath is where all of the test takes place
dstpth = os.path.join('temp')
if not os.path.exists(dstpth):
    os.makedirs(dstpth)

# distname is the name of the distribution
distname = 'mfusg.1_3'
mfpth = os.path.join(dstpth, distname)

# information on how to build binary
exe_name = 'mfusg'
srcdir = os.path.join(mfpth, 'src')
target = os.path.join(dstpth, exe_name)

def test_teardown():
    # clean up
    if os.path.isdir(mfpth):
        print('Removing folder ' + mfpth)
        shutil.rmtree(mfpth)

    if os.path.isfile(target):
        print('Removing ' + target)
        os.remove(target)
    return

if __name__ == '__main__':
    test_teardown()
