#!/usr/bin/env python

# Execute this script to run through all tests (wrap, build, run).
# Check that the correct Fortran compiler is set in Tests.mk.  If
# necessary, add "-c gfortran" to OPTS below (g95 name mangling is
# currently the default)

import sys
import os
import glob

OPTS = '-g --clean -d wrap'     # FortWrap options
cmd = '../../fortwrap.py'

custom_opts = { 'c_arrays' : OPTS + ' --c-arrays', 
                'interface_file' : OPTS + ' -i interface.i' }

# Tests for demonstration purposes only:
excludes = [ 'comments' ]

os.chdir('tests')
tests = glob.glob('*')
tests.remove( glob.glob('*.mk')[0] )

num_err = 0

# Use a command arg to force a make clean on each test
make_clean = False
if len(sys.argv) > 1:
    print "Making clean"
    make_clean = True

# Hack so can go up a directory at start of loop
if tests:
    os.chdir(tests[0])

failed_tests = []

for test in tests:
    if test in excludes:
        continue
    os.chdir('..')
    print "Running test:", test,
    os.chdir(test)
    # Run wrapper generator
    if test in custom_opts:
        opts = custom_opts[test]
    else:
        opts = OPTS
    if make_clean:
        os.system('make clean > /dev/null')
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
