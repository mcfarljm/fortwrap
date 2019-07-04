#!/usr/bin/env python

# Execute this script to run through all tests (wrap, build, run).
# Check that the correct Fortran compiler is set in Tests.mk.  If
# necessary, add "-c gfortran" to OPTS below (gfortran name mangling
# is currently the default)

from __future__ import print_function
import sys
import os
import glob
import subprocess

OPTS = '-g --clean -d wrap'     # FortWrap options

# Add the executable to the command.  This way the tests are run with
# the chosen python instead of the default
cmd = sys.executable + ' ' + os.path.normpath('../../fortwrap.py')

custom_opts = { 'c_arrays' : OPTS + ' --no-vector', 
                'interface_file' : OPTS + ' -i interface.i',
                'multidim_arrays' : OPTS + ' --no-vector --no-fmat',
                'strings2' : OPTS + ' --string-out=wrapper',
                'strings3' : OPTS + ' --string-out=c',
                'overloading' : OPTS + ' -i config.txt' }

# Tests for demonstration purposes only:
excludes = [ 'comments' ]

excludes.append('no_module')

tests_dir = os.path.abspath('tests')
os.chdir('tests')
tests = glob.glob('*')

num_err = 0

FNULL = open(os.devnull, 'w')

# Use a command arg to prevent making clean
make_clean = True
if len(sys.argv) > 1:
    print("Not making clean")
    make_clean = False

failed_tests = []

for test in tests:
    os.chdir(tests_dir)
    if test in excludes or (not os.path.isdir(test)):
        continue
    if not os.path.exists(os.path.join(test, 'Makefile')):
        # This can happen if there are leftover directories for tests
        # that only exist on a different branch
        continue
    print("Running test:", test, end=' ')
    os.chdir(test)
    # Create "wrap" directory if doesn't exist:
    if not os.path.exists('wrap'):
        os.makedirs('wrap')
    # Run wrapper generator
    if test in custom_opts:
        opts = custom_opts[test]
    else:
        opts = OPTS
    if make_clean:
        subprocess.call('make clean', stdout=FNULL, shell=True)
    p = subprocess.Popen(cmd + ' ' + opts, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True)
    stdout, stderr = p.communicate()
    if p.returncode != 0 or 'Error:' in stderr.decode():
        num_err += 1
        failed_tests.append((test,'wrapper'))
        print("[FAIL: wrapper]")
        continue
    # Build test program
    stat = subprocess.call('make', stdout=FNULL)
    if stat!=0:
        num_err += 1
        failed_tests.append((test,'build'))
        print("[FAIL: build]")
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
        print("[FAIL: run]")
        continue
    print("[PASS]")

if num_err == 0:
    print("Tests successful")
else:
    print(num_err, "error(s):", failed_tests)
