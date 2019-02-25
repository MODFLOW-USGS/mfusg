import sys
import os
import shutil
import subprocess

outext = ['lst', 'list', 'hds', 'ddn', 'clnhds', 'cbb', 'cbc', 'cbw', 'afr']
exe_extension = ''
shell = False
if sys.platform.lower().startswith('win'):
    exe_extension = '.exe'
    shell = True

def get_output(folder):
    # get list of output files
    files = os.listdir(folder)
    output_files = []
    for fname in files:
        if not any([fname.endswith(ext) for ext in outext]):
            continue
        fwpath = os.path.join(folder, fname)
        if os.path.isfile(fwpath):
            output_files.append(fwpath)    
    return output_files

    
def del_output(folder):
    output_files = get_output(folder)
    for f in output_files:
        print('  removing {}'.format(f))
        os.remove(f)
    return


def run_model(folder):
    exe = os.path.join('..', 'bin', 'mfusg' + exe_extension)
    exe = os.path.abspath(exe)
    nam = [f for f in os.listdir(folder) if f.endswith('.nam')][0]
    print('  running command {} {}'.format(exe, nam))
    subprocess.check_call([exe, nam], cwd=folder, shell=shell)
    return


def run_zb(folder):
    exe = os.path.join('..', 'bin', 'zonbudusg' + exe_extension)
    exe = os.path.abspath(exe)
    rsp = 'zbud.rsp'
    cmd = exe + ' < ' + rsp
    print('  running command {}'.format(cmd))
    subprocess.check_call(cmd, cwd=folder, shell=True)
    #with open(os.path.join(folder, 'zbud.rsp')) as input_pipe:
    #    subprocess.check_call(exe, cwd=folder, stdin=input_pipe)
    return


def copy_output(folder):
    output_files = get_output(folder)
    for f in output_files:
        src = f
        dst = os.path.join(folder, 'output', os.path.basename(f))
        print('  copying {} to {}'.format(src, dst))
        if os.path.isfile(dst):
            os.remove(dst)
        shutil.copy(src, dst)        
    return


folderlist = [folder for folder in os.listdir('.') if os.path.isdir(folder)]
for folder in folderlist[:]:
    print('\nProcessing folder {}'.format(folder))
    del_output(folder)
    run_model(folder)
    copy_output(folder)
    
    zbdir = os.path.join(folder, 'zonbudusg')
    if os.path.isdir(zbdir):
        del_output(zbdir)
        run_zb(zbdir)
        copy_output(zbdir)
        del_output(zbdir)
        
    del_output(folder)
