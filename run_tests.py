#!/usr/bin/env python

# Execute this script to run through all tests (wrap, build, run).
# Check that the correct Fortran compiler is set in Tests.mk.  If
# necessary, add "-c gfortran" to OPTS below (gfortran name mangling
# is currently the default)

import sys
import os
import glob
import subprocess

OPTS = '-g --clean -d wrap'     # FortWrap options
cmd = os.path.normpath('../../fortwrap.py')

custom_opts = { 'c_arrays' : OPTS + ' --no-vector', 
                'interface_file' : OPTS + ' -i interface.i',
                'multidim_arrays' : OPTS + ' --no-vector --no-fmat',
                'cstrings' : OPTS + ' --no-string' }

# Tests for demonstration purposes only:
excludes = [ 'comments' ]

os.chdir('tests')
tests = glob.glob('*')
tests.remove( glob.glob('*.mk')[0] )

num_err = 0

# Use a command arg to prevent making clean
make_clean = True
if len(sys.argv) > 1:
    print "Not making clean"
    make_clean = False

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
        os.system('make clean > ' + os.devnull)
    stat = os.system(cmd + ' ' + opts + ' > ' + os.devnull)
    if stat!=0:
        num_err += 1
        failed_tests.append((test,'wrapper'))
        print "[FAIL: wrapper]"
        continue
    # Build test program
    stat = os.system('make > ' + os.devnull)
    if stat!=0:
        num_err += 1
        failed_tests.append((test,'build'))
        print "[FAIL: build]"
        continue
    # Run test program
    #
    # This command can be tricky on Windows if the path contains
    # spaces.  With os.system, they need to be protected with outer
    # quotes (so Windows sees the quotes); that isn't necessary with
    # subprocess.call
    stat = subprocess.call(os.path.abspath('prog'))
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
