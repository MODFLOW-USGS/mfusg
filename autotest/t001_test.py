from __future__ import print_function
import os
import shutil
import pymake
import config


def test_compile_dev():
    """
    Compile the program from source

    """

    # Compile
    target = config.target
    pymake.main(config.srcdir, target, 'gfortran', 'gcc', makeclean=True,
                expedite=False, dryrun=False, double=False, debug=False,
                include_subdirs=False)

    # Ensure target has been built
    assert os.path.isfile(target) is True

    return


def test_compile_ref():
    """
    Remove the distribution folder if it exists, download the distribution,
    and compile the program.

    """
    # Remove the existing distribution directory if it exists

    dir_release = config.dir_release
    if os.path.isdir(dir_release):
        print('Removing folder ' + dir_release)
        shutil.rmtree(dir_release)

    # Setup variables
    url = config.url_release
    srcdir = config.srcdir_release
    target = config.target_release

    # Download the MODFLOW-USG distribution
    pymake.download_and_unzip(url, pth=config.testdir)

    # compile
    pymake.main(srcdir, target, 'gfortran', 'gcc', makeclean=True,
                expedite=False, dryrun=False, double=False, debug=False,
                include_subdirs=False)

    assert os.path.isfile(target) is True

    return

if __name__ == '__main__':
    test_compile_dev()
    test_compile_ref()
