#!/usr/bin/env python

# Copyright (c) 2010 John McFarland

# This program is licensed under the MIT license.  See LICENSE.txt

# Author: John McFarland

# This program will parse a selected set of Fortran source files and
# write C++ code to wrap the Fortran derived types in C++ classes

# Run fortwrap.py -h for usage information

import getopt
import re
import glob
from datetime import date
import sys
import os
import traceback


VERSION = '2.0.0'


# SETTINGS ==========================================

# Default Fortran compiler.  Can be overridden by command option
compiler = 'gfortran'

ERROR_FILE_NAME = 'FortWrap-error.txt'

code_output_dir = '.'
include_output_dir = '.'
fort_output_dir = '.'

HEADER_STRING = '/* This source file automatically generated on ' + str(date.today()) + ' using \n   FortWrap wrapper generator version ' + VERSION + ' */\n'

func_pointer_converter = 'convert_c_funcpointer'

misc_defs_filename = 'InterfaceDefs.h'
matrix_classname = 'FortranMatrix'
string_classname = 'FortranString'

orphan_classname = 'FortFuncs'
orphan_class_comments = ['Wrapper class for Fortran routines that do not operate on a derived type']
constants_classname = 'FortConstants'

fort_wrap_file = 'CppWrappers'
SWIG = True  # Whether or not to include preprocessor defs that will
             # be used by swig

# ===================================================


# REGULAR EXPRESSIONS ===============================

fort_type_def = re.compile(r'\s*TYPE(\s+(?P<name0>[a-z]\w*)|.*::\s*(?P<name1>[a-z]\w*))', re.IGNORECASE)
fort_type_extends_def = re.compile(r'EXTENDS\s*\(\s*([a-z]\w*)\s*\)', re.IGNORECASE)
fort_tbp_def = re.compile(r'\s*PROCEDURE\s*(\(\s*(?P<interface>[a-z]\w*)\))?.*::\s*(?P<name>[a-z]\w*)\s*(=>\s*(?P<proc>[a-z]\w*))?', re.IGNORECASE)

fort_proc_def = re.compile(r'\s*(RECURSIVE)?\s*(SUBROUTINE|FUNCTION)\s+\S+\(', re.IGNORECASE)
fort_end_proc = re.compile(r'\s*END\s*(SUBROUTINE|FUNCTION)', re.IGNORECASE)
fort_end_interface = re.compile(r'\s*END\s*INTERFACE', re.IGNORECASE)
fort_comment = re.compile('\s*!')
fort_contains = re.compile(r'\s*CONTAINS\s*$', re.IGNORECASE)

# Data types
primitive_data_str = '(INTEGER|REAL|DOUBLE PRECISION|LOGICAL|CHARACTER|INT|COMPLEX)(\s*(\*(?P<old_kind_spec>[0-9]+)|\(\s*((KIND|len)\s*=)?\s*(?P<kind_spec>(\w+|\*))\s*\)))?'
primitive_data = re.compile(primitive_data_str,re.IGNORECASE)
fort_data_str = r'\s*(' + primitive_data_str + '|(?P<dt_mode>TYPE|CLASS)\s*\((?P<dt_spec>\S*)\)|PROCEDURE\s*\((?P<proc_spec>\S*)\)\s*,\s*POINTER)'
fort_data = re.compile(fort_data_str,re.IGNORECASE)
fort_data_def = re.compile(fort_data_str + '.*::',re.IGNORECASE)
optional_def = re.compile('OPTIONAL.*::', re.IGNORECASE)
byval_def = re.compile('VALUE.*::', re.IGNORECASE)
allocatable_def = re.compile('ALLOCATABLE.*::',re.IGNORECASE)
fort_pointer_def = re.compile('POINTER.*::',re.IGNORECASE)
# CLASS: not yet supported, but print warnings
fort_class_data_def = re.compile(r'\s*CLASS\s*\(\S*\).*::',re.IGNORECASE)

module_def = re.compile(r'\s*MODULE\s+\S',re.IGNORECASE)
end_module_def = re.compile(r'\s*END\s+MODULE',re.IGNORECASE)
# INT below is used to represent the hidden length arguments, passed by value
fort_dox_comments = re.compile(r'\s*!\>')
fort_dox_inline = re.compile(r'\s*\w+.*!\<')
result_name_def = re.compile(r'.*RESULT\s*\(\s*(\w+)\s*\)',re.IGNORECASE)
intent_in_def = re.compile(r'.*INTENT\s?\(\s*IN\s*\)',re.IGNORECASE)
intent_out_def = re.compile(r'.*INTENT\s?\(\s*OUT\s*\)',re.IGNORECASE)
fort_abstract_def = re.compile(r'\s*ABSTRACT\s+INTERFACE',re.IGNORECASE)
integer_param_def = re.compile(r'\s+INTEGER,\s+PARAMETER\s+::',re.IGNORECASE)
# Regular expression to break up arguments.  This is needed to handle
# multi-dimensional arrays (e.g. A(m,n)).  It uses a negative
# lookahead assertion to exclude commas inside the array definition
arg_splitter = re.compile(',(?![^(]*\))')
dimension_def = re.compile(r'DIMENSION\s*\(\s*([^(]+)\s*\)',re.IGNORECASE)
enum_def = re.compile(r'\s*ENUM,\s*BIND',re.IGNORECASE)

# Constructor and desctructor methods (these regexes are configurable):
ctor_def = re.compile('.*_ctor', re.IGNORECASE)
dtor_def = re.compile('.*_dtor', re.IGNORECASE)

# ===================================================


# GLOBAL VARIABLES ==================================

objects = dict() # lower case object name -> DerivedType instance
procedures = []
abstract_interfaces = dict()
name_substitutions = dict()
name_exclusions = set()
name_inclusions = set()
proc_arg_exclusions = set()

# 'INT' is for the hidden name length argument
cpp_type_map = {'INTEGER':{'':'int*','1':'signed char*','2':'short*','4':'int*','8':'long long*','C_INT':'int*'}, 
                'REAL':{'':'float*', '4':'float*', '8':'double*', 'C_DOUBLE':'double*', 'C_FLOAT':'float*'},
                'LOGICAL':{'':'int*'}, 
                'CHARACTER':{'':'char*'}, 
                'INT':{'':'int'}}

special_param_comments = set( ['OPTIONAL', 'ARRAY', 'FORTRAN_ONLY'] )

current_module = ''
module_proc_num = 1 # For keeping track of the order that the
                    # procedures are defined in the Fortran code.  Not
                    # needed now that they are stored in a list, but
                    # keep in case want to use later to achieve a
                    # different ordering than is in the source
dox_comments = []

fort_integer_params = dict()
enumerations = []

file_pointer = -1
file_lines_list = []

PRIVATE=1
PUBLIC=0

# Indicate whether or not we will need procedure pointer wrapper code
proc_pointer_used = False
# Whether or not matrices are used
matrix_used = False
stringh_used = False            # Whether "string.h" is used (needed for strcpy)
fort_class_used = False
string_class_used = False # Whether string outputs are used, which involves wrapping using a string class (either std::string or the generated wrapper class)

# Gets changed to string_classname if --no-std-string is set
string_object_type = 'std::string'

not_wrapped = [] # List of procedures that weren't wrapped

# ===================================================

class FWTypeException(Exception):
    """Raised when an unexpected type is enountered in the code generation
    phase"""
    def __init__(self,type):
        self.type = type
    def __str__(self):
        return self.type

def warning(msg):
    sys.stderr.write('Warning: ' + msg + '\n')

def error(msg):
    sys.stderr.write('Error: ' + msg + '\n')

class Array:
    two_d_warning_written = False
    multi_d_warning_written = False
    def __init__(self,spec):
        global matrix_used
        # spec is the part inside the parentheses
        self.assumed_shape = False
        self.size_var = ''
        self.d = spec.count(',') + 1
        if ':' in spec:
            self.assumed_shape = True
        if self.d == 1:
            self.size_var = spec.strip()
        elif self.d == 2 and not opts.no_fmat:
            matrix_used = True
            self.size_var = tuple([v.strip() for v in spec.split(',')])

        if self.d==2 and opts.no_fmat:
            if not Array.two_d_warning_written:
                warning("Wrapping 2-D arrays as pointers can lead to ambiguity if writing SWIG typemaps for pointers.  This can be avoided by not using --no-fmat")
                Array.two_d_warning_written = True
        if self.d>2:
            if not Array.multi_d_warning_written:
                warning("Wrapping higher-dimensional arrays as pointers can lead to ambiguity if writing SWIG typemaps for pointers.")
                Array.multi_d_warning_written = True

        # Properties queried by the wrapper generator:
        # vec=vector: 1-d array
        self.vec = self.d==1 and not self.assumed_shape
        self.matrix = self.d==2 and not self.assumed_shape
        self.fort_only = self.assumed_shape

class CharacterLength:
    """
    val may be either a number, or in the case of assumed length, a
    string containing the argument name, which is needed in the
    wrapper code
    """
    def __init__(self,val):
        self.val = val
        self.assumed = False
    def set_assumed(self, argname):
        self.val = argname
        self.assumed = True

class DataType:
    complex_warning_written = False
    def __init__(self,type,array=None,str_len=None,hidden=False):
        global proc_pointer_used, stringh_used
        self.type = type
        self.kind = ''
        self.array = array
        self.str_len = str_len
        self.proc_pointer = False
        self.dt = False      # False, 'TYPE', or 'CLASS'
        self.hidden = hidden # hidden name length arg
        # For array, name of argument used to pass the array length:
        self.is_array_size = False # integer defining an array size
        self.is_matrix_size = False
        if type=='CHARACTER':
            stringh_used = True # Really only needed for intent(in)
        elif type.upper()=='COMPLEX':
            if not DataType.complex_warning_written:
                warning("COMPLEX data not supported")
                DataType.complex_warning_written = True

        primitive_data_match = primitive_data.match(type)
        if primitive_data_match:
            # This line must be outside the next two checks since they
            # won't catch the case of just 'real'
            self.type = primitive_data_match.group(1).upper()
            if primitive_data_match.group('old_kind_spec'):
                self.kind = primitive_data_match.group('old_kind_spec')
            elif primitive_data_match.group('kind_spec'):
                self.kind = primitive_data_match.group('kind_spec')

            if type.upper() == 'DOUBLE PRECISION':
                self.type = 'REAL'
                self.kind = '8'

            # Handle use of named parameter as kind:
            if self.kind.upper() in fort_integer_params:
                self.kind = str(fort_integer_params[self.kind.upper()])

            if not self.valid_primitive():
                warning(self.type+'('+self.kind+') not supported')

        if not primitive_data_match and type!='INT':
            # (INT is used to represent the hidden length arguments,
            # passed by value)
            if 'PROCEDURE' in type.upper():
                self.proc_pointer = True
                # Matching broken b/c of "::'
                m = fort_data.match(type)
                self.type = m.group('proc_spec')
                proc_pointer_used = True
            else:
                m = fort_data.match(type)
                if m.group('dt_mode'):
                    self.dt = m.group('dt_mode').upper()
                    self.type = m.group('dt_spec')
                else:
                    raise FWTypeException(type)

        # Properties queried by the wrapper generator:
        # vec=vector: 1-d array
        self.vec = self.array and self.array.vec
        self.matrix = self.array and self.array.matrix

    # Determine whether type is a valid primitive type.  Intended to
    # verify KIND specs.  Doesn't check string lengths.  Kind specs
    # should already be stripped from type string and stored in kind
    # variable
    def valid_primitive(self):
        type_map = cpp_type_map.get(self.type)
        return type_map and self.kind.upper() in type_map

class Argument:
    def __init__(self,name,pos,type=None,optional=False,intent='inout',byval=False,allocatable=False,pointer=False,comment=[]):
        global string_class_used
        self.name = name
        self.pos = pos
        self.type = type
        self.comment = []
        self.optional=optional
        self.intent = intent
        self.byval = byval
        self.allocatable = allocatable
        self.pointer = pointer # (doesn't include procedure pointers)
        self.native = False # Will get defined in
                            # associate_procedures; identifies derived
                            # type arguments that will get passed in
                            # C++ using native classes
        self.exclude = False # Whether it is excluded via the ignores file
        self.cpp_optional = False  # Whether or not it is optional to C++

        # Whether or not the argument is inside an abstract procedrue
        # definition.  This is not set on initialization, but later
        # during parsing. (Added this attribute to get "const"
        # statements to show up in the func pointer defs, which don't
        # use pass_by_val)
        self.in_abstract = False

        if self.optional:
            self.comment.append('OPTIONAL')
        if type:
            if type.array:
                if type.array.vec:
                    self.comment.append('ARRAY')
                elif type.array.fort_only:
                    self.comment.append('FORTRAN_ONLY')
            if type.type=='CHARACTER' and intent!='in':
                string_class_used = True
        if comment:
            for c in comment:
                self.comment.append(c)

    def set_type(self,type):
        self.type = type
        if type.array and type.array.vec:
            self.comment.append('ARRAY')

    def pass_by_val(self):
        """Whether we can pass this argument by val in the C++ interface"""
        return not self.optional and not self.type.dt and self.intent=='in' and not self.type.array and not self.type.type=='CHARACTER' and not self.in_abstract

    def fort_only(self):
        if self.type.hidden:
            return True
        elif self.type.is_array_size or self.type.is_matrix_size:
            return True
        if self.type.array and self.type.array.fort_only:
            return True
        elif self.exclude:
            return True
        elif self.type.dt:
            if self.type.array:
                return True
        elif self.type.proc_pointer:
            if not self.type.type in abstract_interfaces:
                return True
            elif self.intent=='out':
                return True
        elif self.type.type=='CHARACTER':
            if not self.intent=='inout' and self.type.str_len and not self.type.array:
                return False
            else:
                return True
        if self.type.type in cpp_type_map and not self.type.valid_primitive():
            return True
        elif self.type.type.upper()=='COMPLEX':
            return True
        if self.byval and not self.type.type.upper()=='C_PTR':
            # This could be supported for other cases if needed
            return True
        if self.allocatable:
            return True
        if self.pointer:
            return True
        return False
    
    def cpp_const(self):
        """Whether or not it can be const in C++"""
        # matrix and array types excluded because we don't want
        # FortranMatrix<const double>.  Exclude pass_by_val for
        # aesthetic reasons (to keep const from being applied to
        # non-pointer arguments in method definitions)
        return self.intent=='in' and not self.pass_by_val() and not self.type.matrix

    def cpp_type(self):
        """
        Convert a Fortran type to a C++ type.
        """
        if self.type.dt:
            return 'ADDRESS'
        elif self.type.valid_primitive():
            if self.cpp_const():
                prefix = 'const '
            else:
                prefix = ''
            return prefix + cpp_type_map[self.type.type.upper()][self.type.kind]
        elif self.type.proc_pointer and self.type.type in abstract_interfaces:
            #print "Proc pointer not implemented yet:", self.type.type
            proc = abstract_interfaces[self.type.type]
            if proc.retval:
                string = cpp_type_map[proc.retval.type.type.upper()][proc.retval.type.kind][:-1] + ' '
            else:
                string = 'void '
            string = string + '(*' + self.name + ')'
            string = string + '(' + c_arg_list(proc,bind=True) + ')'
            return string
        else:
            raise FWTypeException(self.type.type)


class Procedure:
    def __init__(self,name,args,method,retval=None,comment=None):
        self.name = name
        self.retval = retval
        self.args = args # By name
        # TODO: this is a dict but using it as if it is sorted:
        self.args_by_pos = dict()
        self.nargs = len(args)
        self.mod = current_module
        self.comment = comment
        self.method = method # None, 't' (TYPE), 'c' (CLASS)
        self.num = module_proc_num
        self.ctor = False
        self.dtor = False
        self.in_orphan_class = False # Set in associate_procedures
        if self.method:
            if ctor_def.match(name):
                self.ctor = True
            elif dtor_def.match(name) and self.valid_dtor():
                self.dtor = True
        # Check for exclusion:
        for arg in args.itervalues():
            if (name.lower(),arg.name.lower()) in proc_arg_exclusions:
                if arg.optional:
                    arg.exclude = True
                else:
                    warning("Argument exclusions only valid for optional arguments: " + name + ', ' + arg.name)
        # Make position map
        for arg in self.args.itervalues():
            self.args_by_pos[arg.pos] = arg
        # Check for arguments that can have default values of NULL in C++
        for arg in reversed(self.args_by_pos.values()):
            # (Assumes the dictionary iteration order is sorted by key)
            if arg.optional:
                arg.cpp_optional = True
            else:
                break
        self.add_hidden_strlen_args()
        # Check for integer arguments that define array sizes:
        for arg in self.args.itervalues():
            if arg.type.type.upper()=='INTEGER' and arg.intent=='in':
                if not opts.no_vector and self.get_vec_size_parent(arg.name):
                    # The check on opts.no_vector prevents writing
                    # wrapper code that tries to extract the array
                    # size from a C array
                    arg.type.is_array_size = True
                elif not opts.no_fmat and self.get_matrix_size_parent(arg.name):
                    arg.type.is_matrix_size = True
            
    def has_args_past_pos(self,pos,bind):
        """Query whether or not the argument list continues past pos,
        accounting for c++ optional args"""
        has = False
        for p,arg in self.args_by_pos.iteritems():
            if p > pos:
                if not arg.fort_only():
                    has = True
                elif bind:
                    has = True
        return has

    def add_hidden_strlen_args(self):
        """Add Fortran-only string length arguments to end of argument list"""
        nargs = len(self.args)
        pos = nargs + 1
        for arg in self.args_by_pos.itervalues():
            if arg.type.type=='CHARACTER' and not arg.fort_only():
                str_length_arg = Argument(arg.name + '_len__', pos, DataType('INT', str_len=arg.type.str_len ,hidden=True))
                self.args[ str_length_arg.name ] = str_length_arg
                pos = pos + 1
        for arg in self.args.itervalues():
            if arg.pos > nargs:
                self.args_by_pos[arg.pos] = arg

    def get_vec_size_parent(self,argname):
        """Get the first 1d array argument that uses the argument argname to
        define its size"""
        for arg in self.args_by_pos.itervalues():
            if arg.type.vec and not arg.optional and arg.type.array.size_var==argname:
                return arg.name
        return None

    def get_matrix_size_parent(self,argname):
        """Similar to get_vec_size_parent.  Returns (matname,0) if argname
        is the number of rows and (matname,1) if argname is the number of
        columns"""
        for arg in self.args_by_pos.itervalues():
            if arg.type.matrix and not arg.optional and argname in arg.type.array.size_var:
                if argname == arg.type.array.size_var[0]:
                    return arg.name,0
                else:
                    return arg.name,1
        return None

    def has_post_call_wrapper_code(self):
        """Whether or not wrapper code will be needed for after the call to
        the Fortran procedure"""
        for arg in self.args.itervalues():
            # Special string handling for after the call needed
            if arg.type.type=='CHARACTER' and not arg.fort_only() and arg.intent=='out':
                return True
        return False

    def valid_dtor(self):
        """Whether the procedure is a valid dtor.  Mainly this just
        checks that there are no required arguments"""
        for arg in self.args.itervalues():
            if arg.pos > 1 and not arg.optional:
                return False
        return True
        
        
class DerivedType:
    def __init__(self,name,comment=None):
        self.name = name
        self.cname = translate_name(name) # Account for name renaming
        self.procs = []
        self.mod = current_module
        self.comment = comment

        self.is_class = False
        self.abstract = False
        self.extends = None
        self.tbps = {} # proc => tbp instance

    def add_tbp(self, tbp):
        self.tbps[tbp.proc] = tbp

    def ctor_list(self):
        """Return list of associated ctors"""
        ctors = []
        for proc in self.procs:
            if proc.ctor:
                ctors.append(proc)
        return ctors

class TypeBoundProcedure:
    def __init__(self,line,re_match):
        self.name = re_match.group('name')
        self.proc = re_match.group('proc') or self.name
        self.deferred = 'DEFERRED' in line.upper()
        # Initially False or a string, but the string gets changed to
        # a procedure instance in associate_procedures
        self.interface = re_match.group('interface')


def mangle_name(mod,func):
    if mod:
        if compiler == 'g95':
            return mod.lower() + "_MP_" + func.lower()
        else: # gfortran
            return '__' + mod.lower() + '_MOD_' + func.lower()
    else:
        suffix = '_'
        if compiler=='g95' and '_' in func:        
            suffix = suffix + '_'
        return func.lower() + suffix


def get_proc(name):
    """Return the first procedure in the global list that matches name"""
    for proc in procedures:
        if proc.name.lower()==name.lower():
            return proc
    return None

def remove_const(argtype):
    """Remove const from argtype"""
    if argtype.startswith('const'):
        return argtype.split('const ')[1]
    return argtype


def translate_name(name):
    """Use substitution dictionary to translate, or return given name
    if not present"""
    if name.lower() in name_substitutions:
        return name_substitutions[name.lower()]
    else:
        return name

def readline(f):
    """
    Acts like readline, but grabs the line out of a global list.  This
    way can move backwards in the list
    """
    global file_pointer, file_lines_list
    if file_pointer < len(file_lines_list):
        file_pointer = file_pointer + 1
        return file_lines_list[file_pointer-1]
    else:
        return ''


def join_lines(line,file):
    """Join lines possibly continued by & character"""
    while '&' in line.split('!')[0]:
        line1 = line.split('&')[0]
        line2 = readline(file)
        line = line1 + line2.strip()
    return line


def is_public(name):
    """Check whether a name is public and not excluded"""
    if name.lower() in name_exclusions:
        return False
    elif name.lower() in name_inclusions:
        return True
    elif default_protection == PUBLIC:
        return not name.lower() in private_names
    else:
        return name.lower() in public_names
        

# TODO: delete if not being used
def args_have_comment(args):
    """Whether or not there are any comments in a set of arguments"""
    for arg in args.itervalues():
        if arg.comment:
            return True
    return False

def add_type(t):
    if is_public(t):
        objects[t.lower()] = DerivedType(t,dox_comments)
        return True
    else:
        return False

def parse_argument_defs(line,file,arg_list_lower,args,retval,comments):
    count = 0

    line = join_lines(line,file)
    line = line.split('!')[0]

    m = fort_data.match(line)

    # Attributes.
    optional = True if optional_def.search(line) else False
    byval = True if byval_def.search(line) else False
    allocatable = True if allocatable_def.search(line) else False
    pointer = False
    if fort_pointer_def.search(line) and ('procedure' not in line.split('::')[0].lower()):
        pointer = True
    # Check for DIMENSION statement
    dimension = dimension_def.search(line)
    intent = 'inout'
    if intent_in_def.match(line):
        intent = 'in'
    elif intent_out_def.match(line):
        intent = 'out'

    # Get type string [e.g. INTEGER, TYPE(ErrorType),
    # PROCEDURE(template), POINTER]
    type_string = m.group(1)

    # Process character length
    char_len = None
    if type_string.upper().startswith('CHARACTER'):
        type_string = 'CHARACTER'
        if m.group('old_kind_spec'):
            char_len = int( m.group('old_kind_spec') )
        elif m.group('kind_spec'):
            spec = m.group('kind_spec')
            if spec == '*': 
                char_len = '*'
            else:
                try:
                    char_len = int(spec)
                except ValueError:
                    # Check for string in Fortran parameter
                    # dictionary.  Note, this conversion/check could
                    # be done later when writing the wrapper code (so
                    # that all source has been parsed): in that case,
                    # make sure have proper checks so that it doesn't
                    # conflict with treatment of assumed length
                    # strings
                    char_len = fort_integer_params.get(spec.upper(), None)
        else:
            # No char length spec found.  Could use this to wrap
            # scalar CHARACTER
            char_len = None     # Redundant
        char_len = CharacterLength(char_len)

    # Get name(s)
    arg_string = line.split('::')[1].split('!')[0]
    names = [name.strip() for name in arg_splitter.split(arg_string)]
    for name in names:
        array = None
        if dimension:
            array = Array(dimension.group(1))
        elif '(' in name:
            size_desc = name.split('(')[1].split(')')[0]
            array = Array(size_desc)
            name = name.split('(')[0]
        type = DataType(type_string,array,char_len)
        if name.lower() in arg_list_lower:
            count += 1
            if char_len and char_len.val=='*':
                # Assign a new char_len instance and set it to contain
                # a reference to the argument name, which will be
                # needed in the wrapper code
                type.str_len = CharacterLength('*')
                type.str_len.set_assumed(name)
            args[name] = Argument(name,arg_list_lower.index(name.lower())+1,type,optional,intent,byval,allocatable,pointer,comment=comments)
        elif retval and name.lower()==retval.name.lower():
            retval.set_type(type)
    return count

def parse_proc(file,line,abstract=False):
    """
    Parse a procedure definition, given starting line
    """
    global dox_comments, abstract_interfaces, module_proc_num, not_wrapped
    # Store in case decide to overwrite them when parsing arg comments
    proc_comments = dox_comments 

    # First check for line continuation:
    line = join_lines(line,file)
    proc_name = line.split('(')[0].split()[-1]

    arg_string = line.split('(')[1].split(')')[0]
    if re.search(r'\S',arg_string):
        arg_list_lower = arg_string.split(',')
        arg_list_lower = [x.strip().lower() for x in arg_list_lower] # Remove whitespace and convert to lowercase
    else:
        arg_list_lower = []
    args = dict()
    #print arg_list_lower
    retval = None
    if re.match('^\s*function', line, re.IGNORECASE):
        m = result_name_def.match(line)
        if m:
            retval = Argument(m.group(1),0)
        else:
            retval = Argument(proc_name,0)
    # Determine argument types
    method = False
    invalid = False
    arg_comments = []
    while True:
        line = readline(file)
        if not line:
            error("Unexpected end of file in procedure")
            return
        elif fort_end_proc.match(line):
            break
        elif fort_proc_def.match(line):
            # This means we found a contained procedure.  We need to
            # parse through it to the end to make sure that its
            # arguments don't overwrite the parent's arguments, and
            # that we don't wrap the contained procedure itself
            while True:
                line = readline(file)
                if not line:
                    error("Unexpected end of file in contained procedure")
                    return
                elif fort_end_proc.match(line):
                    break                
        elif fort_contains.match(line):
            in_contains = True
        elif fort_dox_comments.match(line):
            arg_comments = parse_comments(file,line)
            continue
        elif fort_dox_inline.match(line):
            # Note: don't CONTINUE here b/c still need to parse this arg
            arg_comments = parse_comments(file,line)
        elif fort_comment.match(line):
            continue
        if fort_data_def.match(line):
            # Could check below return value to see if in local
            # variables (assuming definitions are ordered in code)
            parse_argument_defs(line,file,arg_list_lower,args,retval,arg_comments)
            arg_comments = []
        elif fort_class_data_def.match(line):
            warning("CLASS arguments not currently supported: " + proc_name)
    # Check args:
    if len(args) != len(arg_list_lower):
        missing_args = set(arg_list_lower).difference(set(args.keys()))
        error("Missing argument definitions in procedure %s: %s" % (proc_name, ', '.join(missing_args)))
        invalid = True
    for arg in args.itervalues():
        if arg.fort_only() and not arg.optional:
            # Note: the above fort_only() call depends on the
            # appropriate abstract interfaces already having been
            # defined.  Make sure the Fortran source file that
            # contains those definitions is parsed first
            invalid = True
        if arg.pos==1 and arg.type.dt:
            method = True
    if retval:
        if retval.type:
            if retval.type.dt or retval.type.array:
                invalid = True
            elif retval.type.type == 'CHARACTER':
                invalid = True
        else:
            error("Untyped return value in %s: %s" % (proc_name,retval.name))
            invalid = True
    if invalid:
        not_wrapped.append(proc_name)
    else:
        proc = Procedure(proc_name,args,method,retval,proc_comments)
        # dtors automatically get added.  This way we can hide them
        # with the ignores list, but they still get used in the Class
        # dtor:
        is_tbp = False
        if method:
            try:
                if proc_name in objects[proc.args_by_pos[1].type.type.lower()].tbps:
                    is_tbp = True
            except KeyError:
                # Should mean that the derived type is not public
                pass
        # Note: wrap all abstract procedures (even if they are not
        # public).  This is sort of a hack, as technically we should
        # only wrap non-public abstract procedures if they are used in
        # TBP definitions
        if is_public(proc_name) or proc.dtor or is_tbp or abstract:
            if not abstract:
                procedures.append(proc)
                module_proc_num += 1
            else:
                abstract_interfaces[proc_name] = proc

def parse_abstract_interface(file,line):
    global dox_comments
    while True:
        line = readline(file)
        if line=='':
            print "Unexpected end of file in abstract interface"
            return
        elif fort_dox_comments.match(line):
            # Grab comments and ignore them
            dox_comments = parse_comments(file,line)
            continue
        elif fort_end_interface.match(line):
            break
        elif fort_comment.match(line):
            continue
        elif fort_proc_def.match(line):
            parse_proc(f,line,abstract=True)
            dox_comments = []

def parse_type(file,line):
    global fort_class_used
    m = fort_type_def.match(line)
    typename = m.group('name0') or m.group('name1')
    type_added = add_type(typename)
    # type_added checks below prevent KeyError's in objects[]
    if type_added and 'ABSTRACT' in line.upper():
        objects[typename.lower()].abstract = True
        objects[typename.lower()].is_class = True
        fort_class_used = True
    extends_match = fort_type_extends_def.search(line)
    if type_added and extends_match:
        objects[typename.lower()].extends = extends_match.group(1)
        objects[typename.lower()].is_class = True
        fort_class_used = True
    # if type_added:
    #     print "{} extends: {}".format(typename, objects[typename.lower()].extends)
    # Move to end of type and parse type bound procedure definitions
    while True:
        line = readline(file)
        tbp_match = fort_tbp_def.match(line)
        if line == '':
            error("Unexpected end of file in TYPE " + typename)
            return
        elif line.upper().strip().startswith('END TYPE'):
            return
        elif type_added and tbp_match:
            if 'POINTER' in line.upper():
                pass # Not a TBP
            elif 'PASS' in line.upper():
                warning('PASS and NOPASS not yet supported for type bound procedures')
            else:
                tbp = TypeBoundProcedure(line, tbp_match)
                objects[typename.lower()].add_tbp(tbp)

def parse_comments(file,line):
    """
    Parse doxygen comments.  On inline comments, return file back to
    line containing comment start.
    """
    global file_pointer
    #print "Parsing comment:", line
    com = []
    read_count = 0
    if fort_dox_comments.match(line):
        com.append(line.split('!>')[1].strip())
        # TODO: do I need this:
        com[0] = com[0].replace('!>','!!')
    elif fort_dox_inline.match(line):
        com.append(line.split('!<')[1].strip())
    else:
        warning("Bad line in parse_comments: " + line)
    while True:
        line = readline(file).strip()
        if line.startswith('!!'):
            com.append(line.split('!!')[1].strip())
            read_count+=1
        else:
            # Back up a line, or all the way back to the comment start
            # if it is inline
            if fort_dox_inline.match(line):
                    for i in xrange(read_count):
                        file_pointer -= 1
            file_pointer -= 1
            break
    return com

def parse_enum(file,line):
    """
    Parse enum definition
    """
    enums = []
    while True:
        line = readline(file)
        if line == '':
            print "Unexpected end of file in ENUM"
            return
        if line.strip().upper().startswith('END'):
            break
        else:
            if line.strip().upper().startswith('ENUMERATOR'):
                line = join_lines(line,file)
                try:
                    s = line.split('::')[1].split('!')[0]
                    if s.find('=')>=0:
                        print "Non-default enum values not supported"
                        enums = []
                        break
                    for enum in s.split(','):
                        if is_public(enum.strip()):
                            enums.append(enum.strip())
                except:
                    print "Problem parsing ENUMERATOR:", line
    if len(enums) > 0:
        #print "Adding enum:", enums
        enumerations.append(enums)
    

def initialize_protection():
    global default_protection, private_names, public_names
    default_protection = PUBLIC
    private_names = set()
    public_names = set()


def parse_file(fname):
    global current_module, file_pointer, file_lines_list, dox_comments, default_protection, private_names, public_names
    try:
        f = open(fname)
    except:
        error("Error opening file " + fname)
        return 0
    current_module = ''
    initialize_protection()
    dox_comments = []
    file_lines_list = f.readlines()
    file_pointer = 0
    while True:
        line = readline(f)
        if line == '':
            break
        elif fort_dox_comments.match(line):
            dox_comments = parse_comments(file,line)
            continue
        elif fort_comment.match(line):
            continue
        elif module_def.match(line) and 'PROCEDURE' not in line.upper():
            current_module = line.split()[1]
            #print 'MOD:', current_module
            dox_comments = []
            initialize_protection()
            module_proc_num = 1
        elif end_module_def.match(line):
            current_module = ''
        elif fort_type_def.match(line):
            #print line.split()[1]
            parse_type(f,line)
            dox_comments = []
        elif fort_proc_def.match(line):
            #print line.split()[1].split('(')[0]
            parse_proc(f,line)
            dox_comments = []
        elif fort_abstract_def.match(line):
            parse_abstract_interface(f,line)
            dox_comments = []
        elif line.strip().upper().startswith('PRIVATE'):
            if '::' not in line:
                default_protection = PRIVATE
            else:
                line = join_lines(line,file)
                for name in line.split('::')[1].split(','):
                    private_names.add(name.strip().lower())
        elif line.strip().upper().startswith('PUBLIC'):
            if '::' in line:
                line = join_lines(line,file)
                for name in line.split('::')[1].split(','):
                    public_names.add(name.strip().lower())
        elif integer_param_def.match(line):
            line = join_lines(line,file).split('!')[0]
            for param in line.split('::')[1].split(','):
                name,val = param.split('=')
                fort_integer_params[name.strip().upper()] = int( val.strip() )
        elif enum_def.match(line):
            parse_enum(f,line)
    f.close()
    return 1

def associate_procedures():
    """
    After collecting the global objects and procedures list,
    associate procedures with objects, where applicable
    """
    def flag_native_args(proc):
        # Check for arguments to pass as native classes:
        for pos,arg in proc.args_by_pos.iteritems():
            if pos>1 and arg.type.dt and not arg.type.array and arg.type.type.lower() in objects:
                arg.native = True

    for proc in procedures:
        # Associate methods
        if proc.method:
            typename = proc.args_by_pos[1].type.type
            if typename.lower() in objects:
                # print "Associating procedure:", typename +'.'+proc.name
                objects[typename.lower()].procs.append(proc)
                flag_native_args(proc)
            elif typename.lower() not in name_exclusions:
                error("Method %s declared for unknown derived type %s" % (proc.name, typename))
        # Associate orphan functions with a dummy class
        elif (not opts.no_orphans) or proc.name.lower() in name_inclusions:
            if not orphan_classname.lower() in objects:
                objects[orphan_classname.lower()] = DerivedType(orphan_classname,orphan_class_comments)
            objects[orphan_classname.lower()].procs.append(proc)
            flag_native_args(proc)
            proc.in_orphan_class = True
    # Tag procedure arguments as being inside abstract interface
    for proc in abstract_interfaces.itervalues():
        for arg in proc.args.itervalues():
            arg.in_abstract = True
            

def write_cpp_dox_comments(file,comments,args_by_pos=None,prefix=0):
    # /// Style
    #for c in comments:
    #    file.write(prefix*' ' + '/// ' + c + '\n')
    started = False
    # First write primary symbol comments
    for i,c in enumerate(comments):
        if i == 0:
            file.write(prefix*' ' + '/*! \\brief ' + c + '\n')
            started = True
        else:
            file.write(prefix*' ' + ' *  ' + c + '\n')
    # Write parameter argument comments if provided
    if args_by_pos:
        for pos in xrange(1,len(args_by_pos)+1):
            arg = args_by_pos[pos]
            if arg.fort_only():
                continue
            for i,c in enumerate(arg.comment):
                if started:
                    # If we add an empty line to an existing \param
                    # comment, that will make the rest a detailed
                    # comment, so don't do this with OPTIONAL and
                    # ARRAY
                    if i==0: #or not (c.split()[0] in special_param_comments):
                        file.write(prefix*' ' + ' *\n')
                    file.write(prefix*' ' + ' *  ')
                else:
                    file.write(prefix*' ' + '/*! ')
                    started = True
                if i==0:
                    file.write('\\param')
                    if arg.intent=='in':
                        file.write('[in]')
                    elif arg.intent=='out':
                        file.write('[out]')
                    file.write(' ' + arg.name + ' ')
                file.write(c + '\n')
    if started:
        file.write(prefix*' ' + '*/\n')


def c_arg_list(proc,bind=False,call=False,definition=True):
    """
    Return the C argument list as a string.  definition: defined as
    opposed to declared (prototype)
    """
    # dt check is necessary to handle orphan functions in the dummy class
    if (not call) and (proc.nargs == 0 or (not bind and proc.nargs==1 and proc.args_by_pos[1].type.dt)):
        return 'void'
    string = ''
    # Pass "data_ptr" as first arg, if necessary. dt check is necessary to
    # handle orphan functions in the dummy class correctly
    if bind and call and proc.nargs>0 and proc.args_by_pos[1].type.dt:
        if proc.nargs==1:
            return 'data_ptr' if proc.args_by_pos[1].type.dt=='TYPE' else '&class_data'
        else:
            string = 'data_ptr, ' if proc.args_by_pos[1].type.dt=='TYPE' else '&class_data, '
    # Add argument names and possibly types
    for pos,arg in proc.args_by_pos.iteritems():
        if (call or not bind) and pos == 1 and proc.args_by_pos[1].type.dt:
            # dt check above excludes the cases where this is an
            # orphan function in the dummy class
            continue
        # Print warning for derived types that are not in the
        # interface (i.e. can't be passed "natively").  These don't
        # get caught by fort_only, b/c it isn't aware of the object
        # list (the native property isn't assigned until
        # associate_procedures is run).  Can always exclude these args
        # from the arg list or add the derived type to the interface
        if (not arg.fort_only() and not bind and not call and definition) and (arg.type.dt and not arg.native and arg.type.type.upper()!='C_PTR'):
            error("Derived type argument %s::%s of procedure %s is not defined" % (arg.type.type, arg.name, proc.name))
        if arg.fort_only():
            # Hide from user -- requires special treatement for three of
            # the four cases
            if bind and call:
                if arg.type.hidden:
                    if arg.type.str_len.assumed:
                        # val stores the arg name of the string
                        # itself.  In wrapper code, length variable is
                        # declared as name+'_len__'
                        string = string + arg.type.str_len.val + '_len__'
                    else:
                        string = string + str(arg.type.str_len.val)
                elif arg.type.is_array_size or arg.type.is_matrix_size:
                    string = string + '&' + arg.name
                else:
                    string = string + 'NULL'
                if proc.has_args_past_pos(pos,bind):
                    string = string + ', '
                continue
            elif not bind:
                continue
        if not call:
            # Prepend type spec
            if arg.type:
                if not bind and arg.type.matrix and not opts.no_fmat:
                    # Special matrix handling
                    if arg.intent=='in':
                        # const has to be handled separately for this case
                        string = string + 'const '
                    string = string + matrix_classname + '<' + arg.cpp_type()[:-1] + '> *'
                elif arg.type.proc_pointer and bind and not call:
                    # Fortran function pointer in C prototype:
                    string = string + 'void* '
                elif not bind and arg.native:
                    string = string + arg.type.type + '* '
                elif not bind and arg.pass_by_val():
                    string = string + arg.cpp_type()[:-1] + ' '
                elif not arg.type.dt and arg.type.vec:
                    if not bind and not opts.no_vector:
                        if arg.intent=='in':
                            # const is manually removed inside <>, so
                            # add it before the type declaration
                            string = string + 'const '
                        string = string + 'std::vector<' + remove_const(arg.cpp_type()[:-1]) + '>* '
                    else:
                        string = string + arg.cpp_type() + ' '
                        if not opts.array_as_ptr:
                            # Chop of the * and add [] after the argname below
                            string = string[:-2] + ' '
                elif arg.type.type=='CHARACTER' and not bind and not arg.intent=='in':
                    string = string + string_object_type + ' *' # pass by ref not compat with optional
                else:
                    string = string + arg.cpp_type() + ' '
            else:
                raise FWTypeException(arg.type.type)
        # Change pass-by-value to reference for Fortran
        if call and bind and arg.pass_by_val():
            string = string + '&'
        # Native class arguments that are not optional need & before arg name
        if arg.type.dt=='CLASS' and call and arg.native and not (arg.optional and arg.cpp_optional):
            string = string + '&'
        # Add argument name -------------------------
        if arg.type.proc_pointer and not bind:
            # Arg name is already part of the C function pointer definition
            pass
        elif call and arg.type.proc_pointer:
            # Pass converted Fortran function pointer
            string = string + arg.name + ' ? &FORT_' + arg.name + ' : NULL'
        elif (bind or opts.no_vector) and not call and not arg.type.dt and arg.type.vec and not opts.array_as_ptr:
            string = string + arg.name + '[]'
        elif (not opts.no_vector) and call and not arg.type.dt and arg.type.vec:
            if arg.optional:
                string = string + arg.name + ' ? &(*' + arg.name + ')[0] : NULL'
            else:
                string = string + '&(*' + arg.name + ')[0]'
        else:
            string = string + arg.name
        # -------------------------------------------
        # Special string handling
        if call and arg.type.type=='CHARACTER' and not arg.intent=='inout':
            # For assumed-length intent(in) strings, no special
            # treatment needed here (we just pass along the character
            # array pointer)
            if not (arg.intent=='in' and arg.type.str_len.assumed):
                # Pass NULL if optional arg not present
                string = string + ' ? ' + arg.name + '_c__ : NULL'
        # Special handling for matrix arguments
        if call and arg.type.matrix and not opts.no_fmat:
            if arg.optional:
                string = string + ' ? ' + arg.name + '->data : NULL'
            else:
                string = string + '->data'
        # Special native handling of certain types:
        if call and arg.native:
            if arg.optional and arg.cpp_optional:
                if arg.type.dt == 'TYPE':
                    string = string + ' ? ' + arg.name + '->data_ptr : NULL'
                else:
                    string = string + ' ? &' + arg.name + '->class_data : NULL'
            else:
                if arg.type.dt == 'TYPE':
                    string = string + '->data_ptr'
                else:
                    string = string + '->class_data'
        elif not call and not bind and not definition and arg.cpp_optional:
            string = string + '=NULL'
        if proc.has_args_past_pos(pos,bind):
            string = string + ', '
    return string
    
# TODO: clean up indent prefix: confusing for proc_pointer special treatment
def function_def_str(proc,bind=False,obj=None,call=False,dfrd_tbp=None,prefix='  '):
    """
    Return a string for a function declaration/definition/call.  There
    are four cases:
    -- C prototype declaration: bind
    -- Method declaration:      none
    -- Method definition:       obj
    -- C binding call:          bind, call

    dfrd_tbp points to corresponding deffered type bound procedure, if
    applicable.  This is needed because one abstract interface could
    be used for multiple TBP's, so can't identify the TBP just from
    the (abstract) procedure itself
    """
    s = ''
    # Add wrapper code for function pointers
    if call:
        declared_c_pointer = False
        for arg in proc.args.itervalues():
            if arg.type.proc_pointer and not arg.fort_only():
                if not declared_c_pointer:
                    s = s + prefix + 'generic_fpointer c_pointer;\n'
                    declared_c_pointer = True
                s = s + prefix + 'c_pointer = (generic_fpointer) ' + arg.name + ';\n'
                s = s + prefix + 'long FORT_' + arg.name + ';\n'
                s = s + prefix + 'if (' + arg.name + ') ' + mangle_name(fort_wrap_file,func_pointer_converter) + '(c_pointer' + ', &FORT_' + arg.name + ');\n'
    # Add wrapper code for strings
    if call:
        for arg in proc.args.itervalues():
            if arg.type.type=='CHARACTER' and not arg.fort_only():
                if arg.type.str_len.assumed:
                    str_len = arg.name+'_len__'
                else:
                    str_len = str(arg.type.str_len.val)
                str_len_p1 = str_len + '+1'
                str_len_m1 = str_len + '-1'
            if arg.type.type=='CHARACTER' and not arg.fort_only() and arg.intent=='out':
                if arg.type.str_len.assumed:
                    # Make sure to initialize the str_length arg to 0,
                    # in case of a not-present optional, since a
                    # character array is statically declared with this
                    # length, even if the arg is not present (testing
                    # with gfortran indicates that in case of not
                    # present it passes 0 for the length)
                    s = s + prefix + 'int ' + arg.name + '_len__ = 0;\n'
                    s = s + prefix + 'if (' + arg.name + ') '+ arg.name + '_len__ = '+ arg.name + '->length();\n'
                s = s + prefix + '// Declare memory to store output character data\n'
                s = s + prefix + 'char ' + arg.name + '_c__[' + str_len_p1 + '];\n'
                s = s + prefix + arg.name + '_c__[' + str_len + "] = '\\0';\n"
            elif arg.type.type=='CHARACTER' and not arg.fort_only() and arg.intent=='in':
                if arg.type.str_len.assumed:
                    s = s + prefix + 'int ' + arg.name + '_len__ = 0;\n'
                    s = s + prefix + 'if (' + arg.name + ') '+ arg.name+ '_len__ = strlen('+arg.name+'); // Protect Optional args\n'
                else:
                    s = s + prefix + '// Create C array for Fortran input string data\n'
                    s = s + prefix + 'char ' + arg.name + '_c__[' + str_len_p1 + '];\n'
                    s = s + prefix + 'if (' + arg.name + ') {\n'
                    s = s + prefix + '  int i;\n'
                    s = s + prefix + '  strncpy(' + arg.name + '_c__, ' + arg.name + ', ' + str_len_p1 + '); ' +arg.name+'_c__['+str_len+'] = 0; // strncpy protects in case '+arg.name+' is too long\n'
                    s = s + prefix + '  for (i=strlen('+arg.name+'_c__); i<'+str_len_p1+'; i++) '+arg.name+"_c__[i] = ' '; // Add whitespace for Fortran\n"
                    s = s + prefix + '}\n'
    # Add wrapper code for array size values
    if call:
        for arg in proc.args.itervalues():
            if arg.type.is_array_size:
                s = s + prefix + 'int ' + arg.name + ' = ' + proc.get_vec_size_parent(arg.name) + '->size();\n'
            elif arg.type.is_matrix_size:
                matrix_name, i = proc.get_matrix_size_parent(arg.name)
                size_method = ('num_rows()','num_cols()')[i]
                s = s + prefix + 'int ' + arg.name + ' = ' + matrix_name + '->' + size_method + ';\n'
    s = s + prefix
    # Make dummy class members static (so class doesn't need to be
    # instantiated)
    if proc.in_orphan_class and not bind and not call and not obj and not opts.global_orphans:
        s = s + 'static '        
    # Now write return type:
    if proc.retval:
        if call:
            if not proc.has_post_call_wrapper_code():
                s = s + 'return '
            else:
                # Save return value and return after wrapper code below
                s = s + proc.retval.cpp_type()[:-1] + ' __retval = '
        else:
            s = s + proc.retval.cpp_type()[:-1] + ' ' # Strip off the '*'
    elif not call:
        s = s + 'void '
    # Definition/declaration:
    if not bind:
        # Determine what the C++ method name will be
        if proc.args_by_pos[1].type.dt == 'CLASS' and proc.name in objects[proc.args_by_pos[1].type.type.lower()].tbps:
            # This is a type bound procedure, so name may different
            # from procedure name
            method_name= objects[proc.args_by_pos[1].type.type.lower()].tbps[proc.name].name
        elif dfrd_tbp:
            # proc is the abstract interface for a deferred tbp
            method_name = dfrd_tbp.name
        else:
            method_name= translate_name(proc.name)        
    if bind:
        s = s + mangle_name(proc.mod,proc.name)
    elif obj and not opts.global_orphans:
        s = s + obj.cname + '::' + method_name
    else:
        s = s + method_name
    s = s + '(' + c_arg_list(proc,bind,call,obj!=None) + ')'
    if not obj:
        s = s + ';'
    if call:
        for arg in proc.args.itervalues():
            # Special string handling for after the call
            if arg.type.type=='CHARACTER' and not arg.fort_only() and arg.intent=='out':
                s = s + '\n'
                s = s + prefix + 'if ('+arg.name+') {\n'
                s = s + prefix + '  // Trim trailing whitespace and assign character array to string:\n'
                s = s + prefix + '  for (int i=' + str_len_m1 + '; ' + arg.name + "_c__[i]==' '; i--) " + arg.name + "_c__[i] = '\\0';\n"
                s = s + prefix + '  ' + arg.name + '->assign(' + arg.name + '_c__);\n  }'
        if proc.retval and proc.has_post_call_wrapper_code():
            s = s + '\n' + prefix + 'return __retval;'
    return s

def write_constructor(file,object,fort_ctor=None):
    """Write the c++ code for the constructor, possibly including a
    call to a Fortran ctor"""
    # Not used for dummy class with orphan functions
    if object.name==orphan_classname or object.abstract:
        return
    file.write('// Constructor:\n')
    file.write(object.cname + '::' + object.cname)# + '() {\n')
    if fort_ctor:
        file.write('(' + c_arg_list(fort_ctor,definition=True) + ')' )
    else:
        file.write('()')
    file.write(' {\n')
    file.write('  data_ptr = NULL;\n')
    # Allocate storage for Fortran derived type
    file.write('  ' + mangle_name(fort_wrap_file, 'allocate_' + object.name) + '(&data_ptr); // Allocate Fortran derived type\n')
    if object.is_class:
        # Class data must be set up before calling constructor, in
        # case constructor uses CLASS argument
        file.write('  class_data.vptr = &__{0}_MOD___vtab_{0}_{1}; // Get pointer to vtab\n'.format(object.mod, object.name))
        file.write('  class_data.data = data_ptr;\n')
    # If present, call Fortran ctor
    if fort_ctor:
        file.write(function_def_str(fort_ctor,bind=True,call=True) )
        file.write(' // Fortran Constructor\n')
        file.write('  initialized = true;\n')
    else:
        file.write('  initialized = false;\n')
    file.write('}\n\n')    

def write_destructor(file,object):
    """Write code for destructor"""
    if object.name==orphan_classname or object.abstract:
        return
    file.write('// Destructor:\n')
    file.write(object.cname + '::~' + object.cname + '() {\n')
    # Check for Fortran destructor
    for proc in object.procs:
        if proc.dtor:
            file.write('  ' + 'if (initialized) ' + mangle_name(proc.mod,proc.name) + '(data_ptr')
            # Add NULL for any optional arguments (only optional
            # arguments are allowed in the destructor call)
            for i in xrange(proc.nargs-1):
                file.write(', NULL')
            file.write('); // Fortran Destructor\n')
    # Deallocate Fortran derived type
    file.write('  ' + mangle_name(fort_wrap_file,'deallocate_'+object.name) + '(data_ptr); // Deallocate Fortran derived type\n')
    file.write('}\n\n')    

def write_class(object):

    # Skip over objects in the exclusion list:
    if object.name.lower() in name_exclusions:
        return

    # First write header file:
    file = open( include_output_dir+'/' + object.cname + '.h', 'w')
    file.write(HEADER_STRING + '\n')
    file.write('#ifndef ' + object.cname.upper() + '_H_\n')
    file.write('#define ' + object.cname.upper() + '_H_\n\n')
    if SWIG:
        # Needs to be before the include's in the case of swig -includeall
        file.write('\n#ifndef SWIG // Protect declarations from SWIG\n')
    file.write('#include <cstdlib>\n') # Needed for NULL
    if not opts.no_vector:
        file.write('#include <vector>\n')
    file.write('#include "' + misc_defs_filename + '"\n')
    includes = get_native_includes(object)
    if object.name in includes:
        includes.remove(object.name) # Remove self
    for include in includes:
        if include.startswith('<'):
            file.write('#include ' + include + '\n')
        else:
            file.write('#include "' + translate_name(include) + '.h"\n')
    # Declare external vtab data
    if object.is_class:
        file.write('\n// Declare external vtab data:\n')
        file.write('extern int __{0}_MOD___vtab_{0}_{1}; // int is dummy data type\n'.format(object.mod, object.name))
    # C Bindings
    file.write('\nextern "C" {\n')
    # Write bindings for allocate/deallocate funcs
    if object.name!=orphan_classname and not object.abstract:
        file.write('  void ' + mangle_name(fort_wrap_file, 'allocate_'+object.name) + '(ADDRESS *caddr);\n')
        file.write('  void ' + mangle_name(fort_wrap_file, 'deallocate_'+object.name) + '(ADDRESS caddr);\n')
    for proc in object.procs:
        file.write(function_def_str(proc,bind=True) + '\n')
    file.write('}\n')

    if SWIG:
        file.write('#endif // SWIG\n')
    file.write('\n')
    
    if not opts.global_orphans:
        write_cpp_dox_comments(file,object.comment)
        file.write('class ' + object.cname + ' ')
        if object.extends:
            file.write(': public ' + object.extends + ' ')
        file.write('{\n\n')
        if object.abstract:
            file.write('protected:\n  // {0} can not be instantiated\n  {0}() {{}}\n\n'.format(object.cname))
        file.write('public:\n')
    # Constructor:
    fort_ctors = object.ctor_list()
    if fort_ctors:
        for fort_ctor in fort_ctors:
            write_cpp_dox_comments(file,fort_ctor.comment,fort_ctor.args_by_pos)
            file.write('  ' + object.cname + '(' + c_arg_list(fort_ctor,bind=False,call=False,definition=False) + ');\n')
    elif not object.name==orphan_classname and not object.abstract:
        # Don't declare default constructor (or destructor, below) for
        # the dummy class
        file.write('  ' + object.cname + '();\n')
    # Desctructor:
    if not object.name==orphan_classname:
        if object.abstract:
            file.write('  virtual ~' + object.cname + '() {}\n\n')
        else:
            file.write('  ~' + object.cname + '();\n\n')
    # Method declarations
    for proc in object.procs:
        if proc.ctor or (proc.dtor and not is_public(proc.name)):
            continue
        write_cpp_dox_comments(file,proc.comment,proc.args_by_pos)
        file.write(function_def_str(proc) + '\n\n')
    # Check for pure virtual methods (which have no directly
    # associated procedure)
    for tbp in object.tbps.itervalues():
        if tbp.deferred:
            # Note: this lookup doesn't respect the module USE
            # hierarchy and could potentially point to wrong
            # abstract_interfaces if the project contains multiple
            # abstract_interfaces with different names, in different
            # modules
            if tbp.interface not in abstract_interfaces:
                error('abstract interface {0} for type bound procedure {1} not found'.format(tbp.interface, tbp.name))
            else:
                file.write('  virtual ' + function_def_str(abstract_interfaces[tbp.interface], dfrd_tbp=tbp, prefix='')[:-1] + ' = 0;\n\n')
    #file.write('\nprivate:\n')
    if object.name!=orphan_classname and not object.extends:
        file.write('  ADDRESS data_ptr;\n')
        if object.is_class:
            file.write('  FClassContainer class_data;\n')
        file.write('\nprotected:\n')
        file.write('  bool initialized;\n')
    if not opts.global_orphans:
        file.write('};\n\n')
    if object.name == orphan_classname:
        # Write out constant/enum defs inside a class def.  For now,
        # this small class def piggybacks off the orphan class header
        # file.  When wrapping in Swig for Python, it makes no
        # difference, since the __init__ file brings in everything in
        # this module
        if len(enumerations) > 0:
            file.write('class ' + constants_classname + ' {\n')
            file.write('public:\n')
            for enum_set in enumerations:
                file.write('  enum { ')
                for i,enum in enumerate(enum_set):
                    file.write('{0}{1}'.format(enum,', ' if i+1<len(enum_set) else ''))
                file.write(' };\n')
            file.write('};\n\n')
    file.write('#endif /* ' + object.cname.upper() + '_H_ */\n')
    file.close()


    # Write method code to cpp file
    file = open( code_output_dir+'/' + object.cname + '.cpp', 'w')
    file.write(HEADER_STRING + '\n')
    if stringh_used:
        file.write('#include <cstring> // For strcpy\n')
    file.write('#include "' + object.cname + '.h"\n\n')
    # Constructor(s):
    if fort_ctors:
        for fort_ctor in fort_ctors:
            write_constructor(file,object,fort_ctor)
    else:
        write_constructor(file,object)
    # Destructor
    write_destructor(file,object)
    
    # Other methods:
    for proc in object.procs:
        if proc.ctor or (proc.dtor and not is_public(proc.name)):
            continue
        file.write(function_def_str(proc,obj=object,prefix='') + ' {\n')
        if proc.dtor:
            file.write('  if (initialized) ')
        file.write(function_def_str(proc,bind=True,call=True,prefix='  ') + '\n')
        if proc.dtor:
            file.write('  initialized = false;\n')
        elif proc.name.lower().startswith('new_'):
            file.write('  initialized = true;\n')
        file.write('}\n\n')

    file.close()

def get_native_includes(object):
    """
    After method association, check which native types an object uses
    and return a corresponding string list of include file

    This will also add the include needed for inheritance
    """
    includes = set()
    for proc in object.procs:
        for argname,arg in proc.args.iteritems():
            if arg.native:
                includes.add(arg.type.type)
            if arg.type.matrix and not opts.no_fmat:
                includes.add(matrix_classname)
            if arg.type.type=='CHARACTER' and arg.intent!='in':
                if opts.std_string:
                    # The use of angle brackets is handled specially
                    # in the output code
                    includes.add('<string>')
                else:
                    includes.add(string_classname)
    # For inheritance:
    if object.extends:
        includes.add(object.extends)
    return includes

def write_global_header_file():
    f = open(include_output_dir+'/' + opts.main_header + '.h','w')
    f.write(HEADER_STRING + '\n')
    for obj in objects.itervalues():
        f.write('#include "' + translate_name(obj.name) + '.h"\n')
    if matrix_used:
        f.write('#include "' + matrix_classname + '.h"\n')
    f.write('\n')
    f.close()


def write_misc_defs():
    f = open(include_output_dir+'/' + misc_defs_filename, 'w')
    f.write(HEADER_STRING + '\n')
    f.write('#ifndef ' + misc_defs_filename.upper()[:-2] + '_H_\n')
    f.write('#define ' + misc_defs_filename.upper()[:-2] + '_H_\n\n')
    f.write('typedef void(*generic_fpointer)(void);\n')
    f.write('typedef void* ADDRESS;\n\n')
    if fort_class_used:
        f.write('struct FClassContainer {\n  ADDRESS data;\n  ADDRESS vptr;\n};\n\n')
    f.write('extern "C" {\n')
    if compiler == 'g95':
        f.write('  /* g95_runtime_start and stop are supposed to be called\n')
        f.write('     to start and stop the g95 runtime engine, but I have not\n')
        f.write('     had any problems when leaving them out.  The only thing\n')
        f.write('     I have noticed is that a bunch of "still reachable" memory\n')
        f.write('     reported by valgrind goes away when a g95_runtime_stop call\n')
        f.write('     is used */\n')
        f.write('  void g95_runtime_start(int narg, char* args[]);\n')
        f.write('  void g95_runtime_stop(void);\n\n')
    if proc_pointer_used:
        f.write('  void ' + mangle_name(fort_wrap_file,func_pointer_converter) + '(generic_fpointer c_pointer, void* fortran_pointer);\n')
    f.write('}\n')
    f.write('\n#endif /* ' + misc_defs_filename.upper()[:-2] + '_H_ */\n')
    f.close()

def write_matrix_class():
    if not matrix_used:
        return
    comments = ['A templated class for working with Matrices that store data', 'internally in Fortran order.', '', 'From C++, the data are accessed in the natural order, using', '<tt>x(row,column)</tt> notation, starting with base index 0']
    
    f = open(include_output_dir+'/' + matrix_classname + '.h', 'w')
    f.write(HEADER_STRING + '\n')
    f.write('#ifndef ' + matrix_classname.upper() + '_H_\n')
    f.write('#define ' + matrix_classname.upper() + '_H_\n\n')
    f.write('#include <cstdlib>\n#include <cassert>\n\n')
    write_cpp_dox_comments(f, comments)
    f.write('template <class T>\nclass ' + matrix_classname + '{\n\n')
    f.write('  int nrows, ncols;\n\npublic:\n\n  ')
    f.write('  T *data;\n\n')
    write_cpp_dox_comments(f, ['Create a matrix with m rows and n columns'])
    f.write('  ' + matrix_classname + '(int m, int n) {\n')
    f.write('    data = NULL;\n    assert(m>0 && n>0);\n    nrows=m; ncols=n;\n')
    f.write('    data = (T*) calloc( m*n, sizeof(T) );\n  }\n\n')
    f.write('  ~' + matrix_classname + '() { if(data) free(data); }\n\n')
    write_cpp_dox_comments(f, ['Provides element access via the parentheses operator.','','The base index is 0'])
    f.write('  T& operator()(int i, int j) {\n')
    f.write('    assert( i>=0 && i<nrows && j>=0 && j<ncols );\n')
    f.write('    // i--; j--; // Adjust base\n')
    f.write('    return data[j*nrows+i];\n  }\n\n')
    write_cpp_dox_comments(f, ['Get number of rows'])
    f.write('  inline int num_rows(void) const { return nrows; }\n\n')
    write_cpp_dox_comments(f, ['Get number of columns'])
    f.write('  inline int num_cols(void) const { return ncols; }\n\n')
    f.write('};\n\n')
    f.write('#endif /* ' + matrix_classname.upper() + '_H_ */\n')
    f.write('\n\n// Local Variables:\n// mode: c++\n// End:\n')
    f.close()

def write_string_class():
    """When the option --no-std-string is used, create a wrapper class with similar functionality.
    
    The reason for doing this is it can work around some C++ library
    conflicts on Windows when linking programs against e.g. Java or Qt
    libraries.

    Separate code and header files are used for this class, since it
    is not a template class.
    """
    if not string_class_used:
        return

    # Header files:
    comments = ['Simple wrapper class for handling dynamic allocation of string data','','Used by FortWrap to handle wrapping of character output arguments.  Emulates some of the basic functionality of std::string, but can be used as a way to remove dependency on std::string and avoid C++ library conflicts in some cases']

    f = open(include_output_dir+'/' + string_classname + '.h', 'w')
    f.write(HEADER_STRING + '\n')
    f.write('#ifndef ' + string_classname.upper() + '_H_\n')
    f.write('#define ' + string_classname.upper() + '_H_\n\n')
    
    f.write('#include <cstdlib>\n#include <cstring>\n\n')
    write_cpp_dox_comments(f, comments)
    body = """\
class $CLASSNAME{
  size_t length_;
  char* data_;
  
 public:

  $CLASSNAME();

  $CLASSNAME(size_t length);

  ~$CLASSNAME();

  size_t length(void);

  void resize(size_t length);

  void assign(const char* s);

  int compare(const char* s) const;

  char* data(void);

  char* c_str(void);
};
""".replace('$CLASSNAME', string_classname)
    f.write(body)
    f.write('\n\n')
    f.write('#endif /* ' + string_classname.upper() + '_H_ */\n')
    f.write('\n\n// Local Variables:\n// mode: c++\n// End:\n')
    f.close()
    
    # Code file:
    f = open(include_output_dir+'/' + string_classname + '.cpp', 'w')
    f.write(HEADER_STRING + '\n')
    
    f.write('#include "' + string_classname + '.h"\n\n')
    body = """\
$CLASSNAME::$CLASSNAME() : length_(0), data_(NULL) {}

$CLASSNAME::$CLASSNAME(size_t length) : length_(length), data_(NULL) {
  if (length>0) data_ = (char*) calloc(length+1, sizeof(char));
}

$CLASSNAME::~$CLASSNAME() { if(data_) free(data_); }

size_t $CLASSNAME::length(void) { return length_; }

void $CLASSNAME::resize(size_t length) {
  if (data_) free(data_);
  data_ = (char*) calloc(length+1, sizeof(char));
  length_ = length;
}

void $CLASSNAME::assign(const char* s) {
  length_ = strlen(s);
  resize(length_);
  strncpy(data_, s, length_);
}

int $CLASSNAME::compare(const char* s) const {
  return strncmp(data_, s, length_);
}

char* $CLASSNAME::data(void) { return data_; }

char* $CLASSNAME::c_str(void) { return data_; }
""".replace('$CLASSNAME', string_classname)
    f.write(body)
    f.close()
    
def write_fortran_wrapper():
    """
    Write the Fortran code for allocating/deallocating Fortran derived
    types from C pointers
    """
    # TODO: fix hack for testing whether we need to write this file
    # Hack: check for real (non-orphan) methods
    count = 0
    for obj in objects.itervalues():
        if obj.name != orphan_classname:
            count += 1
    if count == 0 and not proc_pointer_used:
        return
    # Build list of modules we need to USE
    use_mods = set()
    for obj in objects.itervalues():
        if obj.name != orphan_classname:
            use_mods.add(obj.mod)
    f = open(fort_output_dir+'/' + fort_wrap_file + '.f90', "w")
    #f.write(HEADER_STRING + '\n') # Wrong comment style
    f.write('MODULE ' + fort_wrap_file + '\n\n')
    # USE the necessary modules:
    for mod in use_mods:
        f.write('USE ' + mod + '\n')
    f.write('USE ISO_C_BINDING\n\nCONTAINS\n\n')
    if proc_pointer_used:
        f.write('  SUBROUTINE '+func_pointer_converter+'(cpointer,fpointer)\n')
        f.write('    USE ISO_C_BINDING\n')
        f.write('    TYPE(C_FUNPTR), VALUE :: cpointer\n')
        f.write('    PROCEDURE(), POINTER :: fpointer\n')
        f.write('    CALL C_F_PROCPOINTER(cpointer,fpointer)\n')
        f.write('  END SUBROUTINE '+func_pointer_converter+'\n\n')
    for obj in objects.itervalues():
        if obj.name == orphan_classname or obj.abstract:
            continue
        cptr = obj.name + '_cptr'
        fptr = obj.name + '_fptr'
        # Write allocate function
        f.write(' SUBROUTINE allocate_' + obj.name + '(' + cptr + ')\n')
        f.write('    TYPE (C_PTR) :: ' + cptr + '\n\n')
        f.write('    TYPE (' + obj.name + '), POINTER :: ' + fptr + '\n\n')
        f.write('    ALLOCATE( ' + fptr + ' )\n')
        f.write('    ' + cptr + ' = C_LOC(' + fptr + ')\n')
        f.write('  END SUBROUTINE allocate_' + obj.name + '\n\n')
        # Write deallocate function
        f.write(' SUBROUTINE deallocate_' + obj.name + '(' + cptr + ')\n')
        f.write('    TYPE (C_PTR), VALUE :: ' + cptr + '\n\n')
        f.write('    TYPE (' + obj.name + '), POINTER :: ' + fptr + '\n\n')
        f.write('    CALL C_F_POINTER(' + cptr + ', ' + fptr + ')\n')
        f.write('    DEALLOCATE( ' + fptr + ' )\n')
        f.write('  END SUBROUTINE deallocate_' + obj.name + '\n\n')        
    f.write('END MODULE ' + fort_wrap_file + '\n')
                

def clean_directories():
    """
    Remove old files from output directories before writing new ones
    """
    files = set()
    files = files.union( set( glob.glob(code_output_dir+'/*.cpp') ) )
    files = files.union( set( glob.glob(code_output_dir+'/*.o') ) )
    files = files.union( set( glob.glob(include_output_dir+'/*.h') ) )
    files = files.union( set( glob.glob(fort_output_dir+'/*.f90') ) )
    files = files.union( set( glob.glob(fort_output_dir+'/*.o') ) )
    for f in files:
        os.remove(f)
        

def internal_error():
    sys.stderr.write('FortWrap internal error encountered\n\n')
    sys.stderr.write('Please submit a bug report to mcfarljm@gmail.com which includes the error log\n'+ERROR_FILE_NAME+' and the Fortran source code being wrapped\n')
    f = open(ERROR_FILE_NAME,'w')
    f.write('FortWrap version ' + VERSION + '\n')
    f.write('Platform: ' + sys.platform + '\n\n')
    traceback.print_exc(file=f)
    f.close()
    sys.exit(1)


# Class for parsing the configuration file
class ConfigurationFile:
    def __init__(self,fname):
        self.fname = fname
        if fname:
            try:
                self.f = open(fname)
            except:
                error("Error opening interface file: " + fname)
                return
            self.process()

    def process(self):
        global name_exclusions, name_inclusions, name_substitutions, ctor_def, dtor_def
        for line_num,line in enumerate(self.f):
            if not line.startswith('%'):
                continue
            words = [w.lower() for w in line.split()]
            if words[0] == '%ignore':
                if len(words) == 2:
                    name_exclusions.add( words[1] )
                else:
                    self.bad_decl(line_num+1)
                    continue
            elif words[0] == '%hide':
                if len(words) == 3:
                    proc_arg_exclusions.add( tuple(words[1:]) )
                else:
                    self.bad_decl(line_num+1)
                    continue
            elif words[0] == '%include':
                if len(words) == 2:
                    name_inclusions.add( words[1] )
                else:
                    self.bad_decl(line_num+1)
                    continue
            elif words[0] == '%rename':
                if len(words) == 3:
                    # Note: want new C++ name to preserve case
                    new_name = line.split()[2]
                    name_substitutions[words[1]] = new_name
                else:
                    self.bad_decl(line_num+1)
                    continue
            elif words[0] == '%ctor':
                ctor_def = re.compile(line.strip().split('%ctor ')[1], re.IGNORECASE)
            elif words[0] == '%dtor':
                dtor_def = re.compile(line.strip().split('%dtor ')[1], re.IGNORECASE)
                
    def bad_decl(self,line_num):
        error("%s, line %i <-- bad declaration" % (self.fname, line_num))


# Class for parsing command line options
class Options:
    def __init__(self):
        self.parse_args()

    def usage(self,exit_val=2):
        print "Usage:", sys.argv[0], "[options] [filenames]\n"
        print "Source files to be wrapped can be specified on the command line ([filenames]),\nby globbing the current directory (-g), or listed in a file (--file-list)\n"
        print "-v, --version\t: Print version information and exit"
        print "-h, --help\t: Print this usage information"
        print "-n\t\t: Run parser but do not generate any wrapper code (dry run)"
        print "-c <FC>\t\t: Use name mangling for Fortran compiler <FC>.  Only supports\n\t\t  g95 and gfortran.  Default: FC="+compiler
        print "-g\t\t: Wrap source files found in current directory (glob)"
        print "-d <dir>\t: Output generated wrapper code to <dir>"
        print "--file-list=<f>\t: Read list of Fortran source files to parse from file <f>.\n\t\t  The format is a newline-separated list of filenames with full\n\t\t  or relative paths"
        print "-i <f>\t\t: Read interface configuration file <f>"
        print "--no-vector\t: Wrap 1-D array arguments as C-style arrays ('[]') instead of\n\t\t  C++ std::vector containers"
        print "--no-fmat\t: Do not wrap 2-D array arguments with the FortranMatrix type"
        print "--array-as-ptr\t: Wrap 1-D arrays with '*' instead of '[]'. Implies --no-vector"
        print "--no-std-string\t: Wrap character outputs using a wrapper class instead of\n\t\t  std::string"
        print "--dummy-class=<n>: Use <n> as the name of the dummy class used to wrap\n\t\t  non-method procedures"
        print "--global\t: Wrap non-method procedures as global functions instead of\n\t\t  static methods of a dummy class"
        print "--no-orphans\t: Do not by default wrap non-method procedures.  They can still\n\t\t  be wrapped by using %include directives"
        print "--no-W-not-wrapped: Do not warn about procedures that were not wrapped"
        print "--main-header=<n>: Use <n> as name of the main header file (default FortWrap.h)"
        print "--constants-class=<n>: Use <n> as name of the class for wrapping enumerations\n\t\t  (default: {0})".format(constants_classname)
        # Not documenting, as this option could be dangerous, although
        # it is protected from "-d .":
        #print "--clean\t\t: Remove all wrapper-related files from wrapper code directory\n\t\t  before generating new code.  Requires -d.  Warning: this\n\t\t  deletes files.  Use with caution and assume it will delete\n\t\t  everything in the wrapper directory"
        sys.exit(exit_val)

    def parse_args(self):
        global code_output_dir, include_output_dir, fort_output_dir, compiler, orphan_classname, file_list, constants_classname, string_object_type
        try:
            opts, args = getopt.getopt(sys.argv[1:], 'hvc:gnd:i:', ['file-list=','clean','help','version','no-vector','no-fmat','array-as-ptr','no-std-string','dummy-class=','global','no-orphans','no-W-not-wrapped','main-header=','constants-class='])
        except getopt.GetoptError, err:
            print str(err)
            self.usage()

        for f in args:
            file_list.append(f)

        self.inputs_file = ''
        self.glob_files = False
        self.clean_code = False
        self.dry_run = False
        self.no_vector = False
        self.no_fmat = False
        self.array_as_ptr = False
        self.std_string = True
        self.global_orphans = False
        self.interface_file = ''
        self.no_orphans = False
        self.warn_not_wrapped = True
        self.main_header = 'FortWrap'

        if ('-h','') in opts or ('--help','') in opts:
            self.usage(0)
        elif ('-v','') in opts or ('--version','') in opts:
            print "FortWrap version", VERSION
            sys.exit(0)
        elif ('-g','') in opts:
            self.glob_files = True
        for o,a in opts:
            if o=='--file-list':
                self.inputs_file = a
            elif o=='-d':
                if not os.path.isdir(a):
                    error("Directory does not exist: " + a)
                    sys.exit(2)
                code_output_dir = a
                include_output_dir = a
                fort_output_dir = a
            elif o=='-c':
                compiler = a
                if a!='g95' and a!='gfortran':
                    error("Only g95 and gfortran name mangling supported")
                    sys.exit(2)
            elif o=='-i':
                self.interface_file = a
            elif o=='-n':
                self.dry_run = True
            elif o=='--clean':
                self.clean_code = True
            elif o=='--no-vector':
                self.no_vector = True
            elif o=='--no-fmat':
                self.no_fmat = True
            elif o=='--array-as-ptr':
                self.array_as_ptr = True
                self.no_vector = True
            elif o=='--no-std-string':
                self.std_string = False
                string_object_type = string_classname
            elif o=='--dummy-class':
                orphan_classname = a
            elif o=='--global':
                self.global_orphans = True
            elif o=='--no-orphans':
                self.no_orphans = True
            elif o=='--no-W-not-wrapped':
                self.warn_not_wrapped = False
            elif o=='--main-header':
                self.main_header = a.split('.h')[0]
            elif o=='--constants-class':
                constants_classname = a

        if self.clean_code and code_output_dir=='.':
            error("Cleaning wrapper code output dir requires -d")
            sys.exit(2)


# COMMANDS ==========================================

if __name__ == "__main__":

    try:

        file_list = []

        opts = Options()

        configs = ConfigurationFile(opts.interface_file)

        if opts.clean_code:
            clean_directories()

        if opts.inputs_file:
            try:
                f = open(opts.inputs_file)
            except IOError:
                error('Unable to open file list: ' + opts.inputs_file)
                sys.exit(3)
            for line in f:
                if not line.strip().startswith('#') and re.search('\w', line):
                    file_list.append( line.strip() )
            f.close()
            print "LOADED", len(file_list), 'FILES FROM LIST'

        if opts.glob_files:
            file_list += glob.glob('*.[fF]90')

        if not file_list:
            error("No source files")
            sys.exit(3)

        fcount = 0  # Counts valid files
        for f in file_list:
            fcount += parse_file(f)
        if fcount==0:
            error("No source files")
            sys.exit(3)

        if opts.warn_not_wrapped and len(not_wrapped) > 0:
            warning("Some procedures not wrapped:\n " + '\n '.join(not_wrapped))

        # Prevent writing any files if there is nothing to wrap
        if len(procedures)==0:
            error("No procedures to wrap")
            sys.exit(4)

        associate_procedures()

        if opts.dry_run:
            sys.exit(0)

        for obj in objects.itervalues():
            write_class(obj)

        write_global_header_file()
        write_misc_defs()
        write_matrix_class()
        if not opts.std_string:
            write_string_class()
        write_fortran_wrapper()

        if fort_class_used:
            warning("support for wrapping abstract types and type extension is experimental")

        sys.exit(0)

    except SystemExit:
        # Raised by sys.exit
        raise

    except:
        internal_error()
