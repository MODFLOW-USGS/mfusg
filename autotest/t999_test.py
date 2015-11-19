from __future__ import print_function
import os
import shutil
import config

def test_teardown():
    """
    Clean things up.
    """

    if os.path.isdir(config.dir_release):
        print('Removing folder ' + config.dir_release)
        shutil.rmtree(config.dir_release)

    if os.path.isfile(config.target):
        print('Removing ' + config.target)
        os.remove(config.target)
    return

if __name__ == '__main__':
    test_teardown()
