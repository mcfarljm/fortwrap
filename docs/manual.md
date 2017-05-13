# FortWrap Manual

* [Introduction](#introduction)
* [Main Features](#main-features)
* [Unsupported Features](#unsupported-features)
* [Getting Started](#getting-started)
* [Walkthrough](#walkthrough)
* [The Configuration File](#the-configuration-file)
* [Swig Tips](#swig-tips)
* [Wrapper Details](#wrapper-details)
* [Generated Files](#generated-files)

## Introduction

FortWrap is a Python script that parses Fortran 90/95/200X source
files and generates wrapper code for interfacing with the original
Fortran code from C++.  FortWrap is intended to be used with Fortran
code that takes an object oriented approach and makes use of Fortran
derived types.  The resulting wrapper code provides a C++ interface
that wraps the Fortran derived types with C++ proxy classes.

For example, consider the following Fortran code:

```Fortran
MODULE m

  TYPE Object
    ... 
  END TYPE Object

CONTAINS
    
  SUBROUTINE object_ctor(o, n, x)
    TYPE (Object) :: o
    INTEGER, INTENT(in) :: n
    REAL, INTENT(in) :: x(n)
    ...
  END SUBROUTINE object_ctor

  SUBROUTINE process(o)
    TYPE (Object) :: o
    ...
  END SUBROUTINE process
  
END MODULE m
```

After wrapping with FortWrap, these routines can be utilized in a
natural way from C++:

```C++
std::vector&lt;float&gt; x(10);
... // Define values for x
Object o(&x); // Automatically calls object_ctor
o.process();
```

## Main Features

* <a href="#details-dt">**Derived Types**</a>: Transparent
  translation of Fortran derived types into C++ classes.  This is the
  main objective of FortWrap.
  * Translation of Fortran "ctor" functions into C++ constructors</li>
  * Fortran "dtor" functions automatically called by C++ destructor
* **Classes (experimental)**: Translate Fortran CLASSes and
	type bound procedures into C++ classes.  Fortran inheritance
    structure and polymorphism are retained in C++.
* <a href="#details-opt">**Optional arguments**</a>: Fortran
    optional arguments are fully supported, with automatic NULL
    default values in C++.
* <a href="#details-procpointers">**Procedure pointers**</a>:
    C++ function pointers may be passed natively where Fortran expects
    a procedure pointer.  Right now this requires that the Fortran
    procedure pointer have an explicit `ABSTRACT INTERFACE`
* <a href="#details-arrays">**Arrays**</a>: By default,
    one-dimensional arrays are translated into C++ vector containers.
    Subroutine arguments used to define the Fortran array size are
    automatically calculated based on the C++ vector.
* <a href="#details-matrices">**Matrices**</a>: A
    "FortranMatrix" C++ class is provided for interfacing with
    two-dimensional Fortran arrays (matrices).  This class takes care
    to store data internally in Fortran order.
* <a href="#details-strings">**Strings**</a>
    with `INTENT(IN)` or `INTENT(OUT)` are wrapped using
    C++ strings or character arrays.  The string length may be assumed
    (`len=*`) or a literal or named constant.
* **Enumerations**: equivalent `enum` definitions are
    generated in the C++ wrapper code.
* "Top-level" (a.k.a. global or non-module) procedures are wrapped
* Name mangling support for the gfortran compiler
* Where possible, pass by value is used in C++ (e.g. scalar
  arguments that are not optional)</li>
* **Doxygen comments**: Doxygen-style comments used for
    Fortran symbols (derived types, subroutines, arguments) are
    transferred to C++ doxygen comments in the wrapper code.
* Automatic handling of Fortran `PUBLIC`
  and `PRIVATE` statements.  By default, only `PUBLIC` routines are wrapped.
* Generates clean, human-readable wrapper code

## Unsupported Features

Many features of Fortran 90/95/2003 are not supported by FortWrap.  In
some cases (e.g. assumed shape arrays) this is because the Fortran
language standard does not provide an interoperability mechanism.  In
other cases, this is because there was not a need for certain features
when the original version of FortWrap was being developed.

In most situations, it is possible to get FortWrap to wrap these
types of routines by providing a Fortran wrapper that is interoperable
and calls the target routine.

The following argument types/constructs/features are not
supported:

* `ALLOCATABLE` or `POINTER` arguments
* Assumed shape arrays (a declaration that looks like `INTEGER:: X(:)`).
  The Fortran standard does not provide an
  interoperability mechanism for assumed shape arrays.
* Strings with `INTENT(INOUT)`.  Arrays of strings
* Arrays of a derived type.  These can be wrapped by creating a
  new derived type to contain an array of the derived type of
  interest.  Create an "append" function in Fortran that accepts
  only scalars but allows you to add items to the array container
  one at a time.
* Fortran functions with non-primitive return types are not wrapped.
* `COMPLEX` data type is not supported

Note that FortWrap can still wrap procedures that use unsupported
arguments if those arguments are optional.  In these cases, the
offending arguments are hidden from the generated interface
code.



## Getting Started

* See `README.md`
* Run "`fortwrap.py -h`" for usage information
* Look in the tests directory at examples
* See <a href="#Walkthrough">Walkthrough</a> below

Any one or more of three mechanisms can be used to specify the
Fortran source file(s) to be wrapped:
  
1. Name the files on the command line using full or relative paths
1. The `-g` option will wrap all `.f90`
   and `.F90` source files found in the current working directory.
1. Use the `--file-list` option to name a file that
   contains a list of source files to be wrapped.  The format of this
   file is a newline-separated list of files to be wrapped.

### Notes about the Fortran compiler

FortWrap only supports name mangling for g95 and gfortran.  If
using the Makefile `tests/Tests.mk`, edit `FC` to
specify your Fortran compiler.  When running FortWrap, use
the `-c` option to specify the name mangling you want.
Currently the default wrapping is for gfortran, but this can be changed
by editing the `compiler = 'gfortran'` line near the beginning
of `fortwrap.py`


## Walkthrough

Change directory to `tests/derived_types`.  The first step
is to run FortWrap to generate wrapper code from the Fortran
source code.  In this case the code to be wrapped
is `DerivedTypes.f90`.  Use the following command to run
FortWrap ("`[derived_types]$`" will represent the command
prompt):

```
[derived_types]$ ../../fortwrap.py -c gfortran -g -d wrap
```
This should not produce any output on the command line, which
means that FortWrap thinks it found something to wrap and did not
run into any problems.

* The `-c` option tells FortWrap which Fortran compiler
  will be used once you get around to compiling (running FortWrap
  does not actually compile anything).  Substitute g95 for
  gfortran if necessary.  A default Fortran compiler is specified near
  the beginning of `fortwrap.py`, via the line `compiler
  = 'gfortran'`; feel free to change this.
* The `-g` option stands for "glob" and tells FortWrap to
  try and wrap all files in the current directory that
  match `*.f90` or `*.F90`.  This is just for
  convenience, and we could have named the file explicitly instead
  by adding "`DerivedTypes.f90`" to the end of the command
  line.
* The `-d wrap` option tells FortWrap to put the
  generated wrapper files in the `wrap` directory (which
  should already exist).  Without this option, the files will be
  generated in the working directory.
    
After running FortWrap, you should have a set of wrapper files in
the `wrap` directory:

```
[derived_types]$ ls wrap/
CppWrappers.f90  InterfaceDefs.h  ObjectA.h    ObjectB.h
FortWrap.h       ObjectA.cpp      ObjectB.cpp
```

These files are explained more <a href="#Files">here</a>.  The
main things to note are that the classes FortWrap created are
defined in the header files `ObjectA.h`
and `ObjectB.h` (if wrapping non-method procedures, a dummy
class called `FortFuncs` is created by default).
Also, `FortWrap.h` is the "catch-all" header file that you
want to include when writing a program that uses the wrappers.

The next step is to build the project by compiling the original
Fortran file, the wrapper code, and a test program, and linking
them all together.  The makefile `tests/Tests.mk` is
provided as an example of how this might be done.  On my system,
running `make` produces the following commands:

```
[derived_types]$ make
g++ -I wrap   -c -o prog.o prog.cpp
gfortran -I wrap -o DerivedTypes.o -c DerivedTypes.f90
gfortran -I wrap -o wrap/CppWrappers.o -c wrap/CppWrappers.f90
g++ -I wrap   -c -o wrap/ObjectA.o wrap/ObjectA.cpp
g++ -I wrap   -c -o wrap/ObjectB.o wrap/ObjectB.cpp
gfortran -o prog prog.o DerivedTypes.o wrap/CppWrappers.o wrap/ObjectA.o wrap/ObjectB.o -lstdc++
```

In this case Fortran is used as the linker language and the
standard C++ library is included on the link line.  It should also
be possible to use C++ as the linker language, in which case
Fortran runtime libraries may need to be specified on the link
line.

For this example, the compilation produces the
executable `prog`.  Running the executable should produce
no output.  The FortWrap tests are set up to use return values to
indicate expected behavior (0 means success).  For example:

```
[derived_types]$ ./prog 
[derived_types]$ echo $?
0
```

## The Configuration File

The `-i` option can be used to specify a file to read
simple configuration directives from.  This is not required, but can
provide more control over the generated wrapper interface, and may
be useful when wrapping very large projects.

The format of the configuration file is a simple list of
directives separated by newlines.  The directives start with
the `%` character: any line that doesn't match a directive
is treated as a comment and ignored, so comments may be
interspersed freely without any special prefix.  Also any
character can be inserted before a valid directive to comment it
out, as in "`// %ignore foo`"

The following directives are supported:

* `%ignore <name>`
* `%hide <procedure> <argument>`
* `%include <name>`
* `%rename <old> <new>`
* `%pattern <pattern> [replacement]`
* `%ctor <regex>`
* `%dtor <regex>`

### Ignore directive

The ignore directive is used to prevent FortWrap from creating
wrapper code for certain constructs.  This may be useful when
creating a C++ API for a large Fortran project, particularly when
some Fortran declarations must be kept public even though they are
really not part of the API (keep in mind that FortWrap will
respect `PUBLIC` and `PRIVATE` declarations, so
there is no need to use the ignore directive for `PRIVATE`
names.)

`<name>` may be the identifier for either a derived
type or a procedure.  If a derived type is ignored, the
corresponding proxy class is not created.  If a procedure is
ignored, it is not included in the generated wrapper code.

### Hide directive

The hide directive allows specific procedure arguments to be
hidden from the generated interface.  This is only valid if the
specified argument of the specified procedure is optional.  All
calls from the C++ interface treat the argument as not present.
This directive can be useful for simplifying the generated C++
API.

### Include directive

The include directive can be used to force-include a wrapper for
either a derived type or a procedure that is `PRIVATE` in the
Fortran module.

### Rename directive

The rename directive can operate on either a derived type or a
procedure.  The name `<old>` found in the Fortran
source is replaced by the name `<new>` in the
generated wrapper code.

### Pattern directive

The pattern directive can be used to perform "bulk renames" based
on a regular expression pattern matching.  The first argument is a
regular expression to be matched, and the second argument is
optional replacement text.  The second argument can be omitted, in
which case the replacement text is empty.

### Constructor and destructor directives
  
These directives allow custom regular expressions to be used for
determining which procedures to be treated as constructors and
destructors.  The search will be case insensitive, and the
Python `re match` method is used, meaning the regular
expression must match the beginning of the name


## Swig tips

One of the principal advantages of wrapping code with FortWrap is
that the generated wrappers work well as input for Swig.  This
makes FortWrap useful even if you don't want to work with C++.
Just tell Swig to wrap the FortWrap-generated header files for the
target language of your choice.

Here are some tips:

* Make sure to use the `-c++` option with Swig
* If your target language supports it, use `-keyword`
  (Python supports this).  This way, even though your optional
  arguments must be passed by location in C++, they can be passed by
  keyword in the target language.
* Use `%apply int *INPUT { const int* };` to create an
  input typemap (repeat for `double*`).  With the default
  options, this will only catch scalars because FortWrap uses
  classes (`std::vector` and `FortranMatrix`) for
  arrays (or "`[]`" in the case of `--no-vector`).
  This is compatible with the way optional arguments are treated.
  (Note that using `--no-fmat` or otherwise wrapping
  higher-dimensional arrays will result in wrapping these arrays
  with basic pointers, which may interfere with this type of Swig
  typemap.)
* Use `%apply int *OUTPUT { int* };` to create an
  output typemap.  Note, however, that `INTENT(OUT)`
  and `INTENT(INOUT)` arguments are not distinguishable
  based on the C header file, so this typemap will not work when
  the input value really is used.  This is why FortWrap writes
  warnings for arguments that are not explicitly declared
  either `INTENT(IN)` or `INTENT(OUT)`.  Caution is
  also needed if using this typemap in conjunction with optional
  arguments: because of the way the typemap works, there will be
  no way to "not pass" the optional argument from the target
  language.  This limitation can be worked around by creating a
  custom typemap.
* Be careful with using
  Swig's <a href="http://www.swig.org/Doc1.3/Library.html#Library_nn15">`std_vector.i`</a>
  library.  This drastically increases the size of the wrapper
  code and is not compatible with keyword arguments.  I am also
  not aware of a way to use this library
  with `vector<>*` arguments that are meant to store
  outputs (which typically play a big role in the wrapper code
  that FortWrap generates, since input arguments are qualified
  by `const`).  You are probably better off writing your
  own typemaps to handle the `const` and
  non-`const` `vector` arguments.  If you would
  rather have Swig handle C-style arrays, use FortWrap
  with `--no-vector`
* To wrap string outputs, you may <tt>%include
  "<a href="http://www.swig.org/Doc1.3/Library.html#Library_nn14">std_string.i</a>"</tt>,
  but you will need to write an `argout` typemap to
  handle the string pointer as an output.
* There are a couple different ways to work with function
  pointers in the target language (in Python, at least).  See
  the <a href="http://www.swig.org/Doc1.3/SWIG.html#SWIG_nn30">Swig
  documentation</a>.  The simplest is to use Swig directives to
  wrap an existing C/C++ function as a `%constant` that can
  be passed as an argument in the target language.  With Python,
  it is also possible to define the callback function in the
  target language; this is
  explained <a href="http://docs.python.org/release/2.5.2/ext/callingPython.html">here</a>
  in the Python documentation.
  
## Wrapper Details

* <a href="#details-dt">Derived Types</a>
* <a href="#details-class">CLASS</a>
* <a href="#details-opt">Optional Arguments</a>
* <a href="#details-orphans">Non-method procedures</a>
* <a href="#details-arrays">Arrays</a>
* <a href="#details-matrices">Matrices</a>
* <a href="#details-procpointers">Procedure Pointers</a>
* <a href="#details-strings">Strings</a>
* <a href="#details-comments">Doxygen comments</a>

### Derived Types

The main goal of FortWrap is to allow users to develop C++
interfaces to Fortran code that applies object oriented techniques
by making use of derived types.  In order to interface with
Fortran derived types, FortWrap uses an object handle approach,
where a pointer to the derived type is stored and passed to
arguments that operate on that derived type.  Thus the derived
type can not be directly inspected from C++, but the object handle
(pointer) can be passed to Fortran routines that operate on
it.

FortWrap scans the Fortran source code for definitions of derived
types and Fortran subroutines that operate on those derived types.
For each derived type it finds, it creates a corresponding C++
class.  After parsing the Fortran source code, C++ methods are
then created by associating Fortran subroutines with derived
types.  The rule for this association process is
simple: <span class="hl">each Fortran subroutine with a derived
type as its first argument is translated into a method of the
corresponding C++ class.</span>

FortWrap also provides special mechanisms for wrapping certain
Fortran routines as C++ constructors and destructors.  If a
Fortran routine can be classified as a method and contains the
string "`_ctor`", it is translated into a C++ constructor
for the corresponding class.  Multiple constructors are supported,
provided the resulting C++ code will compile (i.e. the function
signatures are different).  Similarly, if a Fortran routine
contains the string "`_dtor`", it is treated as a
destructor and automatically called from the C++ destructor.
These types of methods are useful for derived types that make use
of dynamically allocated memory.  The regular expressions used to
identify constructors and destructors can be modified by using
the `%ctor` and `%dtor` directives in the
configuration file.
    
### CLASS and polymorphism (experimental)

FortWrap includes experimental support for wrapping CLASS data
structures, which support type bound procedures, inheritance, and
polymorphism.  The objective is to create a set of C++ classes and
methods that mirror the structure of the original Fortran code; this
includes inheritance and polymorphism.

A key distinction is the use of "type bound procedures", which
look like:

```Fortran
TYPE Object
CONTAINS
  PROCEDURE :: foo => foo_def
END TYPE Object

...

SUBROUTINE foo_def(o)
  CLASS(Object) :: o
END SUBROUTINE foo_def
```

Notice that the use of type bound procedures requires defining
the procedure dummy arguments as `CLASS` instead
of `TYPE`.  FortWrap will create a wrapper C++ class with
methods for each type bound procedure, even when the actual
procedures are declared private to the Fortran module (the use of
type bound procedures in this way allows one to reduce name
pollution in large Fortran projects).

As with basic derived type wrapping, FortWrap identifies
constructor and destructor procedures based on pattern matching with
the string "`__ctor`" (see <a href="#details-dt">Derived
Types</a>).  The latest Fortran standards allow creating more
natural constructors by writing a procedure that returns a derived
type and using an interface statement to assign that procedure to
the same name as the derived type.  This can also facilitate dynamic
instantiation of inherited types by using the constructor with
the `SOURCE` attribute inside the `ALLOCATE`
statement.  Currently, FortWrap cannot wrap procedures that return
derived types, but the workaround is to write a wrapper procedure
that is a subroutine, which calls the constructor function.  See the
examples in `tests/classes` of the FortWrap installation.


### Optional Arguments

Fortran 90 provides for optional procedure arguments that may be
passed by position or keyword from Fortran.  Although
interoperating with optional arguments is not explicitly provided
for by the Fortran standard, FortWrap takes advantage of the fact
that both the g95 and gfortran compilers implement optional
arguments in a simple manner that is easy to mimic from C code.
Specifically, these compilers use null pointers to indicate
optional arguments that are not present.

FortWrap exploits this by allowing the arguments to be optional
from C++.  The usage is not quite as friendly as it would be from
Fortran, though, since C++ does not support keyword arguments.
However, FortWrap tries to make the usage as simple as possible by
automatically giving all optional arguments default values
of `NULL`.  Note also that one consequence of using
optional arguments is that they may not be passed by value from
C++.  Normally FortWrap will wrap primitive scalar types
with `INTENT(IN)` as pass-by-value arguments from C++, but
FortWrap must resort to pass-by-reference for optional arguments
(however, see the<a href="#Swig">Swig notes</a>; if re-wrapping
with Swig, this is not an issue).

Consider the following Fortran subroutine:

```Fortran
SUBROUTINE foo(o,a,b,c,d)
  TYPE(Object) :: o
  INTEGER, INTENT(IN) :: a
  INTEGER, INTENT(IN), OPTIONAL :: b, c
  INTEGER, INTENT(OUT), OPTIONAL :: d
  ...
END SUBROUTINE foo
```

Given that an instance `o` as well as `int`'s `a`, `b`, `c`,
and `d` have been declared, the following are examples of
valid C++ calls:

```C++
o.foo(a);
o.foo(a,&b);
o.foo(a,NULL,&c);
o.foo(a,&b,&c,&d);
o.foo(a,NULL,NULL,&d);
```

Note that `a` is passed by value, whereas the optional
arguments must be passed by reference.  Also note that it makes
sense to define the most used optional arguments at the start of the
argument list, because the optional arguments must be passed by
position, and intermediate optional arguments not being provided
must be passed `NULL`.
    
### Non-method procedures

FortWrap can wrap procedures that do not operate on a derived
type.  In keeping with the object-oriented slant (and also to
reduce namespace pollution), such procedures are by default
wrapped as static methods of a dummy class
named `FortFuncs`.  The name of this class can be changed
using the `--dummy-class` option.

As an alternative, the `--global` option can be used to
wrap non-method procedures as global functions defined in
the `FortFuncs.h` header file.

Default wrapping of these procedures can also be disabled by
using `--no-orphans`.  It is still possible to include
specific non-method procedures in the interfaces by
using `%include` directives.
    

### Arrays

By default, one-dimensional arrays are wrapped
to `std::vector<>*`, with a `const` qualifier
for `INTENT(IN)` arguments.  FortWrap will detect arguments
that are used to define the Fortran array size; these are hidden
from the generated interface and are calculated automatically
based on the size of the vector container that is passed in.
  
  The option `--no-vector` can be used to create C-style
  arrays, although with this option FortWrap can not hide the array
  size arguments from the generated interface.

FortWrap does not currently support assumed shape
(e.g. `X(:)`) array arguments.  However, these cases can be
handled by writing a Fortran wrapper that FortWrap can wrap.  For
example, in order to handle a subroutine like:

```Fortran
SUBROUTINE bad_sub(x)
  INTEGER :: x(:)
  ...
END SUBROUTINE bad_sub
```

write a Fortran wrapper that calls the original routine:

```Fortran
SUBROUTINE bad_sub_wrap(n,x)
  INTEGER, INTENT(IN) :: n
  INTEGER :: x(n)
  CALL bad_sub(x)
END SUBROUTINE bad_sub_wrap
```

There is the possibility of a future version of FortWrap creating
these types of wrappers automatically.

### Matrices

By default, matrices are wrapped using a special template
class `FortranMatrix`.  The header
file `FortranMatrix.h` is created when needed.  Part of its
purpose is to hide the fact that C and Fortran store
multi-dimensional array data in different orders.  Element access
is provided via the overloaded parentheses operator.  Note that
although the base Fortran index is 1 (by default), the base index
used by the `FortranMatrix` class is 0.  Bounds checking is
done via a C assertion, but this can be tweaked or commented-out
as needed.

As an example usage, consider declaring an integer matrix with 4
rows and 2 columns, and setting the value of the 1st row and 2nd
column:

```C++
FortranMatrix<int> X(4,2);
X(0,1) = 10;
```

The `--no-fmat` option can be used to prevent use of
the `FortranMatrix` wrapper class, in which case
two-dimensional arrays are wrapped as pointers.

### Procedure Pointers

The Fortran 2003 standard provides support for procedure pointers
as well as mechanisms for interoperating with procedure pointers
from C.  Procedure pointer arguments are wrapped by FortWrap with
the limitation that the interface must be explicit and defined via
an `ABSTRACT INTERFACE` block.
See `tests/function_pointers` for example usage.

FortWrap will parse the abstract interface definition to generate
the corresponding C function pointer prototype.  Make sure that the
callback target conforms to this prototype.


### Strings

FortWrap provides powerful support for processing strings.  This
is a difficult language feature to handle well, and the Fortran
standard for string interoperability is no help.

FortWrap can wrap any scalar string arguments that are
either `INTENT(IN)` or `INTENT(OUT)`.  The string
length may be assumed (`len=*`), a literal constant, or a
named constant from an `INTEGER, PARAMETER` declaration
that FortWrap has already parsed.

Strings that are `INTENT(IN)` are wrapped as `const
char*`: this allows for string literals to be passed directly
from the C code (see the examples in the `tests`
directory).  Strings that are `INTENT(OUT)` are wrapped as
C++ `std::string`.  In all cases, the wrapper code declares
a statically allocated temporary character array, which is passed
to Fortran.  Conversion between the C null terminator and
Fortran's trailing whitespace, as well as passing the hidden
string length argument, are all handled automatically.

Note that when wrapping a character argument that has assumed
length and is `INTENT(OUT)`, FortWrap will determine the
size of the temporary array to be allocated based on the size of
the `std::string` input.  Thus, consider
using `std::string::resize`; otherwise, if an unitialized
string of size 0 is passed from C++, the Fortran code will receive
a character dummy argument with length 0.


### Doxygen Comments

FortWrap automatically transfers Fortran doxygen comments into
C++ doxygen comments in the generated header files.  This way the
C++ header files can be processed by doxygen to generate C++ API
documentation.

Comments that are transferred may be attached to derived types,
procedures, and arguments.  FortWrap also adds intent in/out
specifiers and ARRAY flags to the C++ doxygen comments for
arguments.  The example in `tests/comments` is provided to
illustrate the format for using doxygen comments in the Fortran
source code.

## Generated Files

### `FortWrap.h`

This is a catch-all include file for all of the C++ wrapper code.
Your C++ program only needs to include this.

### `CppWrappers.f90`

This source file is generated when either derived types or
procedure pointers are wrapped.  For each derived type that is being
wrapped, corresponding "allocate" and "deallocate" wrapper functions
are generated for creating C "handles" to the Fortran derived type.
When applicable, this source file will also define a small
procedure pointer conversion routine.

### Class Wrappers

For each Fortran derived type being wrapped, a corresponding C++
class is created.  Two C++ files are created: a header file defining
the class and a source code file defining the methods.  The header
file is the place to look to find out how to call the methods that
FortWrap has generated.

### Dummy Class or Global Wrappers

When you wrap procedures that do not operate on a derived type,
FortWrap wraps them as static members of a dummy C++ class
called `FortFuncs`.  FortWrap will generate two files to
define this class: `FortFuncs.h`
and `FortFuncs.cpp`.  Note that the name of this class and
its associated files can be changed with
the `--dummy-class` option.

When using the `--global` option, the same files are
created, but the non-method procedures are wrapped as global
functions.  The `--dummy-class` option also functions to
change the associated filenames in this case.

### `FortranMatrix.h`

This is the definition of the `FortranMatrix` template
class that is provided for working with matrix arguments.  This file
is only generated when matrix arguments are wrapped.

### `InterfaceDefs.h`

A small header file for FortWrap to declare a few things.
