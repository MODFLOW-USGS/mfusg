import sys
import numpy as np
pth = 'D:/users/langevin/ModelDevelopment'
if pth not in sys.path: sys.path.append(pth)

from flopy2 import *

nlay = 10
nrow = 21
ncol = 21

ibound=np.ones( (nlay, nrow, ncol), dtype=int)
ibound[:, :, 0] = -1
ibound[:, :, -1] = -1

top = 0.
dz = (top - 100.) / nlay
botm = np.linspace(top+dz, -100., -dz)

lrq = [
       #sp 1
       [ [5, 11, 11, -1000],
         [5, 11, 11, -1000] ], 
       #sp 2
       [ [5, 11, 11, -1],
         [5, 11, 11, -1] ], 
       #sp 3
       [ [5, 11, 11, -1000],
         [5, 11, 11, -1000] ]
      ]

mf = Modflow(modelname='afr')
dis = ModflowDis(mf, nlay=nlay, nrow=nrow, ncol=ncol, 
                nper=3, delr=1.0, delc=1.0, 
                laycbd=0, top=top, botm=botm, 
                perlen=1, nstp=1, tsmult=1, steady=True)
bas = ModflowBas(mf, ibound=ibound)
lpf = ModflowLpf(mf, laytyp=4, laywet=0, novfc=True)
wel = ModflowWel(mf, layer_row_column_Q=lrq, options=['autoflowreduce', 'IUNITAFR', 77])
mf.write_input()

