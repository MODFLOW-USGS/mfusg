from __future__ import print_function
import os
import shutil
import flopy
import pymake
from pymake.autotest import compare_budget, compare_heads
import config


def compare(namefile1, namefile2):
    """
    Compare the results from two simulations

    """

    # Compare budgets from the list files in namefile1 and namefile2
    outfile = os.path.join(os.path.split(namefile1)[0], 'bud.cmp')
    success1 = compare_budget(namefile1, namefile2, max_cumpd=0.01,
                             max_incpd=0.01, outfile=outfile)

    outfile = os.path.join(os.path.split(namefile1)[0], 'hds.cmp')
    success2 = compare_heads(namefile1, namefile2, htol=0.001,
                             outfile=outfile)

    success = False
    if success1 and success2:
        success = True

    return success


def run_mfusg(regression=True):
    """
    Download run and compare biscayne quadtree problem

    """

    # Set the testname
    testname = '02_quadtree'
    testpth = os.path.join(config.testdir, testname)

    # Delete folder if exists, then download the MODFLOW-USG distribution
    if os.path.isdir(testpth):
        shutil.rmtree(testpth)
    url = "http://water.usgs.gov/ogw/mfusg/02_quadtree.zip"
    pymake.download_and_unzip(url, pth=config.testdir)

    # Go through name file and replace \ with /
    oldnam = os.path.join(testpth, 'biscayne.nam')
    newnam = os.path.join(testpth, 'biscayne.new.nam')
    fold = open(oldnam, 'r')
    fnew = open(newnam, 'w')
    for i, line in enumerate(fold):
        line = line.replace('\\', '/')
        line = line.replace('biscayne.disu', 'biscayne.new.disu')
        line = line.replace('biscayne.oc', 'biscayne.new.oc')
        fnew.write(line)
    fold.close()
    fnew.close()

    # Change the number of stress periods from 1000 to 10
    olddis = os.path.join(testpth, 'input', 'biscayne.disu')
    newdis = os.path.join(testpth, 'input', 'biscayne.new.disu')
    fold = open(olddis, 'r')
    fnew = open(newdis, 'w')
    for i, line in enumerate(fold):
        if i == 1:
            line = line.replace('1000', '10')
        fnew.write(line)
    fold.close()
    fnew.close()

    # Write oc so that head and budget are always saved.
    newoc = os.path.join(testpth, 'input', 'biscayne.new.oc')
    fnew = open(newoc, 'w')
    fnew.write('HEAD SAVE UNIT 51'+ '\n')
    fnew.write('COMPACT BUDGET'+ '\n')
    for kper in range(10):
        fnew.write('PERIOD {} STEP 1'.format(kper + 1) + '\n')
        fnew.write('  PRINT BUDGET'+ '\n')
        fnew.write('  SAVE BUDGET'+ '\n')
        fnew.write('  SAVE HEAD'+ '\n')
    fnew.close()

    # run test models
    print('running model...{}'.format(testname))
    nam = 'biscayne.new.nam'
    exe_name = os.path.abspath(config.target)
    success, buff = flopy.run_model(exe_name, nam, model_ws=testpth,
                                    silent=True)
    assert success, 'biscayne model did not run successfully.'


    # If it is a regression run, then setup and run the model with the
    # release target
    if regression:
        testname_reg = testname + '_reg'
        testpth_reg = os.path.join(config.testdir, testname_reg)
        pymake.setup(os.path.join(testpth, nam), testpth_reg)
        outdir = os.path.join(testpth_reg, 'output')
        if not os.path.isdir(outdir):
            os.mkdir(outdir)
        print('running regression model...{}'.format(testname_reg))
        exe_name = os.path.abspath(config.target_release)
        success, buff = flopy.run_model(exe_name, nam, model_ws=testpth_reg,
                                         silent=True)

        # Make comparison
        success = compare(os.path.join(testpth, nam),
                          os.path.join(testpth_reg, nam))
        assert success, 'Biscayne regression test did not pass'

    # Clean things up
    if success and not config.retain:
        pymake.teardown(testpth)
        if regression:
            pymake.teardown(testpth_reg)

    return


def test_mfusg_bisqtg():
    run_mfusg()
    return


if __name__ == '__main__':
    run_mfusg()
