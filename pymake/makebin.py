#! /usr/bin/env python
import pymake

#get the arguments
args = pymake.pymake.parser()

#call main -- note that this form allows main to be called
#from python as a function.
pymake.pymake.main(args.srcdir, args.target, args.fc, args.cc, args.makeclean, 
                   args.expedite, args.dryrun, args.double, args.debug)
