#!/usr/bin/env python

import os
import glob

OPTS = '-g --clean -d wrap'
cmd = '../../fortwrap.py'

custom_opts = { 'c_arrays':OPTS+' --c-arrays' }

os.chdir('tests')
tests = glob.glob('*')
tests.remove( glob.glob('*.mk')[0] )

num_err = 0

# Hack so can go up a directory at start of loop
if tests:
    os.chdir(tests[0])

failed_tests = []

for test in tests:
    os.chdir('..')
    print "Running test:", test,
    os.chdir(test)
    # Run wrapper generator
    if test in custom_opts:
        opts = custom_opts[test]
    else:
        opts = OPTS
    stat = os.system(cmd + ' ' + opts + ' > /dev/null')
    if stat!=0:
        num_err += 1
        failed_tests.append((test,'wrapper'))
        print "[FAIL: wrapper]"
        continue
    # Build test program
    stat = os.system('make > /dev/null')
    if stat!=0:
        num_err += 1
        failed_tests.append((test,'build'))
        print "[FAIL: build]"
        continue
    # Run test program
    stat = os.system('./prog')
    if stat!=0:
        num_err += 1
        failed_tests.append((test,'run'))
        print "[FAIL: run]"
        continue
    print "[PASS]"

if num_err == 0:
    print "Tests successful"
else:
    print num_err, "error(s):", failed_tests
