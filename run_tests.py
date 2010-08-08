#!/usr/bin/env python

import os
import glob

opts = '-g --clean -d wrap'
cmd = '../../fortwrap.py'

os.chdir('test')
tests = glob.glob('*')

num_err = 0

for test in tests:
    print "Running test:", test
    os.chdir(test)
    # Run wrapper generator
    stat = os.system(cmd + ' ' + opts)
    if stat!=0:
        print "Error running fortwrap.py"
        num_err += 1
        continue
    # Build test program
    stat = os.system('make')
    if stat!=0:
        print "Error building program"
        num_err += 1
        continue
    # Run test program
    stat = os.system('./prog')
    if stat!=0:
        print "Error running test program"
        num_err += 1

if num_err == 0:
    print "Tests successful"
else:
    print num_err, "error(s)"
