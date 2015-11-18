from __future__ import print_function
import os
import shutil
import pymake
import config


def get_namefiles(pth):
    """
    Search through the path for all .nam files.  Return
    them all in a list.  Namefiles will have paths.

    """
    namefiles = []
    for root, dirs, files in os.walk(pth):
        namefiles += [os.path.join(root, file)
                      for file in files if file.endswith('.nam')]
    return namefiles


def run_mfusg(namefile):
    """
    Run the simulation.

    """

    # Set root as the directory name where namefile is located
    testname = os.path.dirname(namefile).split(os.sep)[-1]

    # Set nam as namefile name without path
    nam = os.path.basename(namefile)

    # Setup
    testpth = os.path.join(config.testdir, testname)
    pymake.setup(namefile, testpth)

    # run test models
    print('running model...{}'.format(testname))
    exe_name = os.path.abspath(config.target)
    success, buff = pymake.run_model(exe_name, nam, model_ws=testpth,
                                     silent=True)

    if success:
        pymake.teardown(testpth)

    assert success is True
    return


def test_mfusg():
    namefiles = get_namefiles(config.testpaths[0])
    for namefile in namefiles:
        yield run_mfusg, namefile
    return


if __name__ == '__main__':
    namefiles = get_namefiles(config.testpaths[0])
    for namefile in namefiles:
        run_mfusg(namefile)
