import pymake

#get the arguments
args = pymake.pymake.parser()
print(args)

#call main -- note that this form allows main to be called
#from python as a function.
pymake.pymake.main(args.srcdir, args.target, args.fc, args.makeclean, args.expedite,
                   args.dryrun, args.double, args.debug)
