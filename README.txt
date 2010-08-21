SUMMARY

FortWrap is a script that parses Fortran 90/95/200X source files and
generates wrapper code for interfacing with the original Fortran code
from C++. FortWrap is intended to be used with Fortran code that takes
an object oriented approach and makes use of Fortran derived
types. The resulting wrapper code provides a C++ interface that wraps
the Fortran derived types with C++ "proxy classes".

Currently FortWrap supports the g95 and gfortran Fortran compilers,
but the generated C++ code should work with any C++ compiler,
including g++.

The latest version is available at sourceforge.net/projects/fortwrap/


RUNNING FORTWRAP

fortwrap.py is an executable python script that may be run using
either "python fortwrap.py [args]" or "fortwrap.py [args]".  The
latter requires that the execute permission be set.  Use "fortwrap.py
-h" to print usage information.


DOCUMENTATION

Refer to docs/manual.html for help getting started.  The tests/
directory provides working examples of most of the main FortWrap
features.


USING THE TESTS

The easiest way to get started is to look at the simple test programs
in the tests directory.

To run the tests, edit the makefile tests/Tests.mk to specify your
Fortran compiler.  The root directory contains a python script
run_tests.py to execute all tests.  For each test, the script will
change to the individual test directory, execute fortwrap.py to
generate wrapper code, execute make to compile and link a simple test
program, and finally run the test program.

To manually run a test, first make sure the compiler specified in
tests/Tests.mk is valid.  Then change to a test directory, for
example, tests/arrays.  Execute "../../fortwrap.py -g --clean -d wrap"
to generate the C++ wrapper code.  Then execute "make" to build the
simple test program in that directory, prog.cpp.


STATUS

8/18/2010

The internals of FortWrap are in a stable state and have been used
successfully to wrap very large Fortran projects (~40,000 lines of
code).  However, FortWrap is not intended to wrap all Fortran
constructs.  In particular, FortWrap is geared towards wrapping
derived types and procedures that operate on them.  In particular,
FortWrap should not be used with Fortran 77 code.  For more details
regarding the Fortran constructs that FortWrap is set up to wrap,
refer to the html documentation and the tests directory.

The command line options are fairly new, so haven't been tested
extensively and may be subject to change in the future, but I believe
they should work as described by "fortwrap.py -h".


RELEASE NOTES

Version 0.9.2 (8/20/10): Added proper handling for top-level
procedures (print warning) and multi-module files.  Expanded
documentation.

Version 0.9.1 (8/19/10): Added support for lowercase Fortran keywords

Version 0.9 (8/18/10): Initial release


CONTACT

Feel free to contact me at mcfarljm@gmail.com with questions, problems,
suggestions, etc.

-- John
