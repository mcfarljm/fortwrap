#!/usr/bin/env python

# This program will parse a selected set of Fortran source files
# (specified in files.in) and write C++ code to wrap the Fortran
# derived types in C++ classes

# Usage: python fortwrap.py [fortran_compiler_name=g95]

# Author: John McFarland

import getopt
import re
import glob
from datetime import date
import sys
import os


VERSION = 0.7


# SETTINGS ==========================================

# Default Fortran compiler.  Can be overridden by command option
compiler = 'g95'

code_output_dir = '.'
include_output_dir = '.'
fort_output_dir = '.'

substitution_file = 'substitutions.in'
ignores_file = 'ignores.in'
includes_file = 'includes.in'
orphans_file = 'orphans.in'

HEADER_STRING = '/* This source file automatically generated on ' + str(date.today()) + ' using \n   Fortran source code parser by John McFarland */\n'

func_pointer_converter = 'convert_c_funcpointer'

misc_defs_filename = 'InterfaceDefs.h'
matrix_classname = 'FortranMatrix'

orphan_classname = 'FortFuncs'
orphan_class_comments = ['Wrapper class for Fortran routines that do not operate on a derived type']

fort_wrap_file = 'CppWrappers'
SWIG = True  # Whether or not to include preprocessor defs that will
             # be used by swig

# ===================================================


# REGULAR EXPRESSIONS ===============================

fort_type_def = re.compile(r'\s*TYPE\s+[a-zA-Z]')
fort_proc_def = re.compile(r'\s*(RECURSIVE)?\s*(SUBROUTINE|FUNCTION)\s+\S+\(')
fort_end_proc = re.compile(r'\s*END\s*(SUBROUTINE|FUNCTION)')
fort_comment = re.compile('\s*!')
fort_data_string = r'\s*(TYPE\s*\((?P<dt_spec>\S*)\)|INTEGER|REAL(\*8|\s*\(C_DOUBLE\)|\s*\(KIND=(?P<real_spec>[0-9]+)\s*\))?|LOGICAL|CHARACTER(?P<char_spec>\s*\([^,]*\))?|PROCEDURE\s*\((?P<proc_spec>\S*)\)\s*,\s*POINTER)'
fort_data = re.compile(fort_data_string,re.IGNORECASE)
fort_data_def = re.compile(fort_data_string + '.*::',re.IGNORECASE)
module_def = re.compile(r'\s*MODULE\s+\S')
# INT below is used to represent the hidden length arguments, passed by value
primitive_data = re.compile('(INTEGER|REAL|LOGICAL|CHARACTER|INT)')
fort_dox_comments = re.compile(r'\s*!\>')
fort_dox_inline = re.compile(r'\s*\w+.*!\<')
result_name_def = re.compile(r'.*RESULT\s*\(\s*(\w+)\s*\)')
intent_in_def = re.compile(r'.*INTENT\s?\(\s*IN\s*\)',re.IGNORECASE)
intent_out_def = re.compile(r'.*INTENT\s?\(\s*OUT\s*\)',re.IGNORECASE)
fort_abstract_def = re.compile(r'\s*ABSTRACT\s+INTERFACE',re.IGNORECASE)
integer_param_def = re.compile(r'\s+INTEGER,\s+PARAMETER\s+::\s+(.*)\s*=\s*([0-9]+)\s*')
# Regular expression to break up arguments.  This is needed to handle
# matrices (e.g. A(m,n)).  It uses a negative lookahead assertion to
# exclude the comma inside a matrix definition
arg_splitter = re.compile(',(?!\s*\w+\s*\))')

# ===================================================


# GLOBAL VARIABLES ==================================

objects = dict()
procedures = []
abstract_interfaces = dict()
name_substitutions = dict()
name_exclusions = set()
name_inclusions = set()
proc_arg_exclusions = set()
orphan_names = set()

# 'INT' is for the hidden name length argument
cpp_type_map = {'INTEGER':'int*', 'REAL':'float*', 'REAL*8':'double*', 'REAL(C_DOUBLE)':'double*','LOGICAL':'int*', 'CHARACTER':'char*', 'INT':'int'}

special_param_comments = set( ['OPTIONAL', 'ARRAY', 'FORTRAN_ONLY'] )

current_module = ''
module_proc_num = 1 # For keeping track of the order that the
                    # procedures are defined in the Fortran code.  Not
                    # needed now that they are stored in a list, but
                    # keep in case want to use later to achieve a
                    # different ordering than is in the source
dox_comments = []

fort_integer_params = dict()

file_pointer = -1
file_lines_list = []

PRIVATE=1
PUBLIC=0

# Indicate whether or not we will need procedure pointer wrapper code
proc_pointer_used = False
# Whether or not matrices are used
matrix_used = False

# ===================================================


class DataType:
    def __init__(self,type,array=False,matrix=False,str_len=-1,hidden=False,array_size_var='',matrix_size_vars=()):
        global proc_pointer_used, matrix_used
        self.type = type
        self.array = array
        self.matrix = matrix
        if matrix:
            matrix_used = True
        self.str_len = str_len
        self.proc_pointer = False
        self.dt = False
        self.hidden = hidden # hidden name length arg
        # For array, name of argument used to pass the array length:
        self.array_size_var = array_size_var 
        self.is_array_size = False # integer defining an array size
        # Similar bookkeeping for matrices
        self.matrix_size_vars = matrix_size_vars
        self.is_matrix_size = False
        # Handle real kinds
        if type.lower().startswith('real') and type.lower().find('kind')>=0:
            kind = type.split('=')[1].split(')')[0].strip()
            if kind=='4':
                self.type = 'REAL'
            elif kind=='8':
                self.type = 'REAL*8'
            else:
                print type, "not supported"
        if not primitive_data.match(type):
            if type.upper().find('PROCEDURE') >= 0:
                self.proc_pointer = True
                # Matching broken b/c of "::'
                m = fort_data.match(type)
                self.type = m.group('proc_spec')
                proc_pointer_used = True
            else:
                self.dt = True
                m = fort_data.match(type)
                self.type = m.group('dt_spec')

class Argument:
    def __init__(self,name,pos,type=None,optional=False,intent='inout',assumed_shape=False,comment=[]):
        self.name = name
        self.pos = pos
        self.type = type
        self.comment = []
        self.optional=optional
        self.intent = intent
        self.assumed_shape = assumed_shape
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
                self.comment.append('ARRAY')
        if self.assumed_shape:
            self.comment.append('FORTRAN_ONLY')
        if comment:
            for c in comment:
                self.comment.append(c)

    def set_type(self,type):
        self.type = type
        if type.array:
            self.comment.append('ARRAY')

    def pass_by_val(self):
        """Whether we can pass this argument by val in the C++ interface"""
        return not self.optional and not self.type.dt and self.intent=='in' and not (self.type.array or self.type.matrix) and not self.type.type=='CHARACTER' and not self.in_abstract

    def fort_only(self):
        if self.type.hidden:
            return True
        elif self.type.is_array_size or self.type.is_matrix_size:
            return True
        elif self.exclude or self.assumed_shape:
            return True
        elif self.type.dt:
            if self.type.array or self.type.matrix:
                return True
        elif self.type.proc_pointer:
            if not self.type.type in abstract_interfaces:
                return True
            elif self.intent=='out':
                return True
        elif self.type.type=='CHARACTER':
            # Note: optional strings could be handled, but need
            # protect wrapper code with if(name) statements and make
            # sure name_c is set to NULL before sent to Fortran
            if self.intent=='out' and self.type.str_len>0 and not self.type.array and not self.optional:
                return False
            else:
                return True
        elif self.type.type.lower().find('kind')>=0:
            # Supported KIND= types are translated in the DataType
            # constructor
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
        elif self.type.type in cpp_type_map:
            if self.cpp_const():
                prefix = 'const '
            else:
                prefix = ''
            return prefix + cpp_type_map[self.type.type]
        elif self.type.proc_pointer and self.type.type in abstract_interfaces:
            #print "Proc pointer not implemented yet:", self.type.type
            proc = abstract_interfaces[self.type.type]
            if proc.retval:
                string = cpp_type_map[proc.retval.type.type][:-1] + ' '
            else:
                string = 'void '
            string = string + '(*' + self.name + ')'
            string = string + '(' + c_arg_list(proc,bind=True) + ')'
            #print "String:", string
            #return "ctype"
            return string
        else:
            print "No corresponding c++ type:", self.type.type
            return "ctype*"    


class Procedure:
    def __init__(self,name,args,method,retval=None,comment=None):
        self.name = name
        self.retval = retval
        self.args = args # By name
        self.args_by_pos = dict()
        self.nargs = len(args)
        self.mod = current_module
        self.comment = comment
        self.method = method
        self.num = module_proc_num
        self.ctor = False
        self.dtor = False
        self.in_orphan_class = False # Set in associate_procedures
        if self.method:
            if name.find('_ctor')>=0:
                self.ctor = True
            elif name.find('_dtor')>=0:
                if self.nargs==1:
                    self.dtor = True
        # Check for exclusion:
        for arg in args.itervalues():
            if arg.optional and (name.lower(),arg.name.lower()) in proc_arg_exclusions:
                arg.exclude = True
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
        # Push new_* methods to the top (for class Distribution)
        if not name.lower().startswith('new'):
            self.num += 100
        self.add_hidden_strlen_args()
        # Check for integer arguments that define array sizes:
        for arg in self.args.itervalues():
            if arg.type.type=='INTEGER' and arg.intent=='in':
                if self.get_array_size_parent(arg.name):
                    arg.type.is_array_size = True
                elif self.get_matrix_size_parent(arg.name):
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
                str_length_arg = Argument(arg.name + '_len', pos, DataType('INT', str_len=arg.type.str_len ,hidden=True))
                self.args[ str_length_arg.name ] = str_length_arg
                pos = pos + 1
        for arg in self.args.itervalues():
            if arg.pos > nargs:
                self.args_by_pos[arg.pos] = arg

    def get_array_size_parent(self,argname):
        """Get the first array argument that uses the argument argname to
        define its size"""
        for arg in self.args_by_pos.itervalues():
            if arg.type.array and not arg.optional and arg.type.array_size_var==argname:
                return arg.name
        return None

    def get_matrix_size_parent(self,argname):
        """Similar to get_array_size_parent.  Returns (matname,0) if argname
        is the number of rows and (matname,1) if argname is the number of
        columns"""
        for arg in self.args_by_pos.itervalues():
            if arg.type.matrix and not arg.optional and argname in arg.type.matrix_size_vars:
                if argname == arg.type.matrix_size_vars[0]:
                    return arg.name,0
                else:
                    return arg.name,1
        return None
        
        
class DerivedType:
    def __init__(self,name,comment=None):
        self.name = name
        self.procs = []
        self.mod = current_module
        self.comment = comment
    def ctor_list(self):
        """Return list of associated CENTAUR ctors"""
        ctors = []
        for proc in self.procs:
            if proc.ctor:
                ctors.append(proc)
        return ctors


def mangle_name(mod,func):
    if compiler == 'g95':
        return mod.lower() + "_MP_" + func.lower()
    else: # gfortran
        return '__' + mod.lower() + '_MOD_' + func.lower()


def read_substitutions():
    """Read name substitutions from a file"""
    global name_substitutions
    try:
        f = open(substitution_file)
        for line in f:
            # Allow comments
            if not line.strip().startswith('#') and re.search('\w', line):
                old,new = line.split()
                name_substitutions[old.lower()] = new
        f.close()
        print "LOADED", len(name_substitutions), "NAME SUBSTITUTIONS"
    except:
        pass

def read_ignores():
    """Read names to ignore from a file"""
    global name_exclusions, proc_arg_exclusions
    try:
        f = open(ignores_file)
        for line in f:
            # Allow comments
            if not line.strip().startswith('#') and re.search('\w', line):
                if line.strip().count(' ') == 1:
                    # Add a (proc,arg) exclusion
                    proc_arg_exclusions.add( tuple(line.strip().lower().split()) )
                else:
                    name_exclusions.add( line.strip().lower() )
        f.close()
        print "LOADED", len(name_exclusions), "NAMES TO IGNORE"
        print "LOADED", len(proc_arg_exclusions), "ARGUMENTS TO IGNORE"
    except:
        pass

def read_includes():
    """Read names to include (even if theya re private) from a file"""
    global name_inclusions
    try:
        f = open(includes_file)
        for line in f:
            # Allow comments
            if not line.strip().startswith('#') and re.search('\w', line):
                name_inclusions.add( line.strip().lower() )
        f.close()
        print "LOADED", len(name_inclusions), "NAMES TO FORCE INCLUDE"
    except:
        pass

def read_orphans():
    """Read names of functions that we want to wrap even though they
    do not operate on a derived type"""
    global orphan_names
    try:
        f = open(orphans_file)
        for line in f:
            # Allow comments
            if not line.strip().startswith('#') and re.search('\w', line):
                orphan_names.add( line.strip().lower() )
        f.close()
        print "LOADED", len(orphan_names), "ORPHAN FUNCS TO WRAP"
    except:
        pass


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
    while line.find('&') > 0:
        line1 = line.strip()[:-1]
        line2 = readline(file)
        line = line1 + line2.strip()
    return line


def is_public(name):
    """Check whether a name is public and not excluded"""
    if name.lower() in name_exclusions:
        return False
#    elif name.lower() in name_inclusions or name.lower() in orphan_names:
#         return True
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
        objects[t] = DerivedType(t,dox_comments)

def parse_argument_defs(line,file,arg_list,args,retval,comments):
    count = 0

    line = join_lines(line,file)

    # Get type string [e.g. INTEGER, TYPE(ErrorType),
    # PROCEDURE(template), POINTER]
    m = fort_data.match(line)
    type_string = m.group(1)

    # Attributes
    optional = line.lower().find('optional')>=0
    # Check for DIMENSION statement (assume it means 1D array...)
    dimension = line.split('::')[0].lower().find('dimension')>=0
    intent = 'inout'
    if intent_in_def.match(line):
        intent = 'in'
    elif intent_out_def.match(line):
        intent = 'out'

    # Process character length
    char_len=-1
    if type_string.startswith('CHARACTER'):
        type_string = 'CHARACTER'
        try:
            len_spec = m.group('char_spec').split('=')[1].split(')')[0].strip()
            try:
                char_len = int(len_spec)
            except ValueError:
                # Check for string in Fortran parameter dictionary
                char_len = fort_integer_params[len_spec]
        except:
            # char_len remains =-1
            pass

    # Get name(s)
    arg_string = line.split('::')[1].split('!')[0]
    names = [name.strip() for name in arg_splitter.split(arg_string)]
    for name in names:
        array = False
        array_size_var = ''
        matrix_size_vars = ()
        if dimension:
            array = True
        matrix = False
        assumed_shape = False
        if name.find(':') > 0:
            assumed_shape = True
        if name.find('(') > 0:
            size_desc = name.split('(')[1].split(')')[0]
            if name.find(',') < 0:
                array = True
                array_size_var = size_desc.strip()
            else:
                matrix = True
                matrix_size_vars = tuple([v.strip() for v in size_desc.split(',')])
            name = name.split('(')[0]
        type = DataType(type_string,array,matrix,char_len,array_size_var=array_size_var,matrix_size_vars=matrix_size_vars)
        if name in arg_list:
            count += 1
            args[name] = Argument(name,arg_list.index(name)+1,type,optional,intent,assumed_shape=assumed_shape,comment=comments)
        elif retval and name==retval.name:
            retval.set_type(type)
    return count

def parse_proc(file,line,abstract=False):
    """
    Parse a procedure definition, given starting line
    """
    global dox_comments, abstract_interfaces, module_proc_num
    # Store in case decide to overwrite them when parsing arg comments
    proc_comments = dox_comments 

    # First check for line continuation:
    line = join_lines(line,file)
    proc_name = line.split('(')[0].split()[-1]
    arg_string = line.split('(')[1].split(')')[0]
    if re.search(r'\S',arg_string):
        arg_list = arg_string.split(',')
        arg_list = [x.strip() for x in arg_list] # Remove whitespace
    else:
        arg_list = []
    args = dict()
    #print arg_list
    retval = None
    if line.find('FUNCTION') >= 0:
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
        if line=='':
            print "Unexpected end of file in procedure"
            return
        elif fort_dox_comments.match(line):
            arg_comments = parse_comments(file,line)
            continue
        elif fort_dox_inline.match(line):
            # Note: don't CONTINUE here b/c still need to parse this arg
            arg_comments = parse_comments(file,line)
        elif fort_end_proc.match(line):
            break
        elif fort_comment.match(line):
            continue
        if fort_data_def.match(line):
            # Could check below return value to see if in local
            # variables (assuming definitions are ordered in code)
            parse_argument_defs(line,file,arg_list,args,retval,arg_comments)
            arg_comments = []
    # Check args:
    if len(args) != len(arg_list):
        print "****** Missing argument definitions:", proc_name
        print set(arg_list).difference(set(args.keys()))
        invalid = True
    for arg in args.itervalues():
        if arg.fort_only() and not arg.optional:
            # Note: the above fort_only() call depends on the
            # appropriate abstract interfaces already having been
            # defined.  Make sure the Fortran source file that
            # contains those definitions is parsed first
            invalid = True
        if arg.type.dt and arg.pos==1:
            method = True
    if retval:
        if retval.type:
            if retval.type.dt or retval.type.array or retval.type.matrix:
                invalid = True
            elif retval.type.type == 'CHARACTER':
                invalid = True
        else:
            print "******* Untyped retval:", retval.name, proc_name
            invalid = True
    if not invalid:
        proc = Procedure(proc_name,args,method,retval,proc_comments)
        # dtors automatically get added.  This way we can hide them
        # with the ignores list, but they still get used in the Class
        # dtor:
        if is_public(proc_name) or proc.dtor:
            if not abstract:
                procedures.append(proc)
                module_proc_num += 1
            else:
                abstract_interfaces[proc_name] = proc

def parse_type(file,line):
    typename = line.split()[1]
    add_type(typename)
    # Move to end of type, so don't catch PUBLIC/PRIVATE
    while True:
        line = readline(file)
        if line == '':
            print "Unexpected end of file in TYPE", typename
            return
        if line.strip().startswith('END TYPE'):
            return

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
        print "Bad line in parse_comments:", line
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
    


def parse_file(fname):
    global current_module, file_pointer, file_lines_list, dox_comments, default_protection, private_names, public_names
    f = open(fname)
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
        elif module_def.match(line) and line.find("PROCEDURE")==-1:
            current_module = line.split()[1]
            print 'MOD:', current_module
            dox_comments = []
            default_protection = PUBLIC
            module_proc_num = 1
            private_names = set()
            public_names = set()
        elif fort_type_def.match(line):
            #print line.split()[1]
            parse_type(f,line)
            dox_comments = []
        elif fort_proc_def.match(line):
            #print line.split()[1].split('(')[0]
            parse_proc(f,line)
            dox_comments = []
        elif fort_abstract_def.match(line):
            line = readline(f)
            if fort_dox_comments.match(line):
                # Handle comment if it is inside the ABSTRACT
                # INTERFACE block
                dox_comments = parse_comments(file,line)
                line = readline(f)
            parse_proc(f,line,abstract=True)
            dox_comments = []
        elif line.strip().startswith('PRIVATE'):
            if line.find('::')==-1:
                default_protection = PRIVATE
            else:
                line = join_lines(line,file)
                for name in line.split('::')[1].split(','):
                    private_names.add(name.strip().lower())
        elif line.strip().startswith('PUBLIC'):
            if line.find('::')>=0:
                line = join_lines(line,file)
                for name in line.split('::')[1].split(','):
                    public_names.add(name.strip().lower())
        elif integer_param_def.match(line):
            m = integer_param_def.match(line)
            fort_integer_params[m.group(1)] = int(m.group(2))
    f.close()

def associate_procedures():
    """
    After collecting the global objects and procedures list,
    associate procedures with objects, where applicable
    """
    def flag_native_args(proc):
        # Check for arguments to pass as native classes:
        for pos,arg in proc.args_by_pos.iteritems():
            if pos>1 and arg.type.dt and not arg.type.array and not arg.type.matrix and arg.type.type in objects:
                arg.native = True

    for proc in procedures:
        # Associate methods
        if proc.method:
            typename = proc.args_by_pos[1].type.type
            if typename in objects:
                # print "Associating procedure:", typename +'.'+proc.name
                objects[typename].procs.append(proc)
                flag_native_args(proc)
            elif typename.lower() not in name_exclusions:
                print "Unknown object in associate_procedures:", proc.name, typename
        # Associate orphan functions with a dummy class
        else:
            if not orphan_classname in objects:
                objects[orphan_classname] = DerivedType(orphan_classname,orphan_class_comments)
            objects[orphan_classname].procs.append(proc)
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
    if (not call) and (proc.nargs == 0 or (not bind and proc.nargs==1)) and proc.args_by_pos[1].type.dt:
        return 'void'
    string = ''
    # Pass "data_ptr" as first arg, if necessary. dt check is necessary to
    # handle orphan functions in the dummy class correctly
    if bind and call and proc.args_by_pos[1].type.dt:
        if proc.nargs==1:
            return 'data_ptr'
        else:
            string = 'data_ptr, '
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
        if (not arg.fort_only() and not bind and not call and definition) and (arg.type.dt and not arg.native):
            print "****** Warning, derived type argument not in interface:", proc.name, arg.type.type, arg.name
        if arg.fort_only():
            # Hide from user -- requires special treatement for three of
            # the four cases
            if bind and call:
                if arg.type.hidden:
                    string = string + str(arg.type.str_len)
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
                if not bind and arg.type.matrix:
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
                elif not arg.type.dt and arg.type.array:
                    if not bind:
                        if arg.intent=='in':
                            # const is manually removed inside <>, so
                            # add it before the type declaration
                            string = string + 'const '
                        string = string + 'std::vector<' + remove_const(arg.cpp_type()[:-1]) + '>* '
                    else:
                        # Chop of the * and add [] after the argname below
                        string = string + arg.cpp_type()[:-1] + ' '
                elif arg.type.type=='CHARACTER' and not bind:
                    # Special string handling
                    string = string + 'std::string *' # pass by ref not compat with optional
                else:
                    string = string + arg.cpp_type() + ' '
            else:
                string = string + 'ctype '
        # Change pass-by-value to reference for Fortran
        if call and bind and arg.pass_by_val():
            string = string + '&'
        # Add argument name -------------------------
        if arg.type.proc_pointer and not bind:
            # Arg name is already part of the C function pointer definition
            pass
        elif call and arg.type.proc_pointer:
            # Pass converted Fortran function pointer
            string = string + arg.name + ' ? &FORT_' + arg.name + ' : NULL'
        elif bind and not call and not arg.type.dt and arg.type.array:
            string = string + arg.name + '[]'
        elif call and not arg.type.dt and arg.type.array:
            if arg.optional:
                string = string + arg.name + ' ? &(*' + arg.name + ')[0] : NULL'
            else:
                string = string + '&(*' + arg.name + ')[0]'
        else:
            string = string + arg.name
        # -------------------------------------------
        # Special string handling
        if call and arg.type.type=='CHARACTER' and arg.intent=='out':
            string = string + '_c'
        # Special handling for matrix arguments
        if call and arg.type.matrix:
            if arg.optional:
                string = string + ' ? ' + arg.name + '->data : NULL'
            else:
                string = string + '->data'
        # Special native handling of certain types:
        if call and arg.native:
            if arg.optional and arg.cpp_optional:
                string = string + ' ? ' + arg.name + '->data_ptr : NULL'
            else:
                string = string + '->data_ptr'
        elif not call and not bind and not definition and arg.cpp_optional:
            string = string + '=NULL'
        if proc.has_args_past_pos(pos,bind):
            string = string + ', '
    return string
    
# TODO: clean up indent prefix: confusing for proc_pointer special treatment
def function_def_str(proc,bind=False,obj=None,call=False,prefix='  '):
    """
    Return a string for a function declaration/definition/call.  There
    are four cases:
    -- C prototype declaration: bind
    -- Method declaration:      none
    -- Method definition:       obj
    -- C binding call:          bind, call
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
            if arg.type.type=='CHARACTER' and not arg.fort_only() and arg.intent=='out':
                s = s + prefix + '// Declare memory to store output character data\n'
                s = s + prefix + 'char ' + arg.name + '_c[' + str(arg.type.str_len+1) + '];\n'
                s = s + prefix + arg.name + '_c[' + str(arg.type.str_len) + "] = '\\0';\n"
    # Add wrapper code for array size values
    if call:
        for arg in proc.args.itervalues():
            if arg.type.is_array_size:
                s = s + prefix + 'int ' + arg.name + ' = ' + proc.get_array_size_parent(arg.name) + '->size();\n'
            elif arg.type.is_matrix_size:
                matrix_name, i = proc.get_matrix_size_parent(arg.name)
                size_method = ('num_rows()','num_cols()')[i]
                s = s + prefix + 'int ' + arg.name + ' = ' + matrix_name + '->' + size_method + ';\n'
    s = s + prefix
    # Make dummy class members static (so class doesn't need to be
    # instantiated)
    if proc.in_orphan_class and not bind and not call and not obj:
        s = s + 'static '        
    # Now write return type:
    if proc.retval:
        if call:
            s = s + 'return '
        else:
            s = s + proc.retval.cpp_type()[:-1] + ' ' # Strip off the '*'
    elif not call:
        s = s + 'void '
    # Definition/declaration:
    if bind:
        s = s + mangle_name(proc.mod,proc.name)
    elif obj:
        s = s + obj.name + '::' + translate_name(proc.name)
    else:
        s = s + translate_name(proc.name)
    s = s + '(' + c_arg_list(proc,bind,call,obj!=None) + ')'
    return s

def write_constructor(file,object,centaur_ctor=None):
    """Write the c++ code for the constructor, possibly including a
    call to a CENTAUR ctor"""
    # Not used for dummy class with orphan functions
    if object.name==orphan_classname:
        return
    file.write('// Constructor:\n')
    file.write(object.name + '::' + object.name)# + '() {\n')
    if centaur_ctor:
        file.write('(' + c_arg_list(centaur_ctor,definition=True) + ')' )
    else:
        file.write('()')
    file.write(' {\n')
    file.write('  data_ptr = NULL;\n')
    # Allocate storage for Fortran derived type
    file.write('  ' + mangle_name(fort_wrap_file, 'allocate_' + object.name) + '(&data_ptr); // Allocate Fortran derived type\n')
    # If present, call CENTAUR ctor
    if centaur_ctor:
        file.write(function_def_str(centaur_ctor,bind=True,call=True) )
        file.write('; // CENTAUR Constructor\n')
        file.write('  initialized = true;\n')
    else:
        file.write('  initialized = false;\n')
    file.write('}\n\n')    

def write_destructor(file,object):
    """Write code for destructor"""
    if object.name==orphan_classname:
        return
    file.write('// Destructor:\n')
    file.write(object.name + '::~' + object.name + '() {\n')
    # Check for CENTAUR destructor
    for proc in object.procs:
        if proc.dtor:
            file.write('  ' + 'if (initialized) ' + mangle_name(proc.mod,proc.name) + '(data_ptr')
            file.write('); // CENTAUR Destructor\n')
    # Deallocate Fortran derived type
    file.write('  ' + mangle_name(fort_wrap_file,'deallocate_'+object.name) + '(data_ptr); // Deallocate Fortran derived type\n')
    file.write('}\n\n')    

def write_class(object):

    # Skip over objects in the exclusion list:
    if object.name.lower() in name_exclusions:
        return

    # First write header file:
    file = open( include_output_dir+'/' + object.name + '.h', 'w')
    file.write(HEADER_STRING + '\n')
    file.write('#ifndef ' + object.name.upper() + '_H_\n')
    file.write('#define ' + object.name.upper() + '_H_\n\n')
    file.write('#include <stdlib.h>\n') # Needed for NULL
    file.write('#include <string>\n') # Needed for special string handling
    file.write('#include <vector>\n')
    file.write('#include "' + misc_defs_filename + '"\n')
    includes = get_native_includes(object)
    if object.name in includes:
        includes.remove(object.name) # Remove self
    for include in includes:
        file.write('#include "' + include + '.h"\n')
    # C Bindings
    if SWIG:
        file.write('\n#ifndef SWIG // Protect declarations from SWIG')
    file.write('\nextern "C" {\n')
    # Write bindings for allocate/deallocate funcs
    if object.name!=orphan_classname:
        file.write('  void ' + mangle_name(fort_wrap_file, 'allocate_'+object.name) + '(ADDRESS *caddr);\n')
        file.write('  void ' + mangle_name(fort_wrap_file, 'deallocate_'+object.name) + '(ADDRESS caddr);\n')
    for proc in object.procs:
        file.write(function_def_str(proc,bind=True) + ';\n')
    file.write('}\n')

    if SWIG:
        file.write('#endif // SWIG\n')
    file.write('\n')
    
    write_cpp_dox_comments(file,object.comment)
    file.write('class ' + object.name + ' {\n\n')
    file.write('public:\n')
    # Constructor:
    centaur_ctors = object.ctor_list()
    if centaur_ctors:
        for centaur_ctor in centaur_ctors:
            write_cpp_dox_comments(file,centaur_ctor.comment,centaur_ctor.args_by_pos)
            file.write('  ' + object.name + '(' + c_arg_list(centaur_ctor,bind=False,call=False,definition=False) + ');\n')
    elif not object.name==orphan_classname:
        # Don't declare default constructor (or destructor, below) for
        # the dummy class
        file.write('  ' + object.name + '();\n')
    # Desctructor:
    if not object.name==orphan_classname:
        file.write('  ~' + object.name + '();\n\n')
    # Method declarations
    for proc in object.procs:
        if proc.ctor or (proc.dtor and not is_public(proc.name)):
            continue
        write_cpp_dox_comments(file,proc.comment,proc.args_by_pos)
        file.write(function_def_str(proc) + ';\n\n')
    #file.write('\nprivate:\n')
    if object.name!=orphan_classname:
        file.write('  ADDRESS data_ptr;\n')
        file.write('\nprivate:\n')
        file.write('  bool initialized;\n')
    file.write('};\n\n')
    file.write('#endif /* ' + object.name.upper() + '_H_ */\n')
    file.close()


    # Write method code to cpp file
    file = open( code_output_dir+'/' + object.name + '.cpp', 'w')
    file.write(HEADER_STRING + '\n')
    file.write('#include "' + object.name + '.h"\n\n')
    # Constructor(s):
    if centaur_ctors:
        for centaur_ctor in centaur_ctors:
            write_constructor(file,object,centaur_ctor)
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
        file.write(function_def_str(proc,bind=True,call=True,prefix='  ') + ';\n')
        for arg in proc.args.itervalues():
            # Special string handling for after the call (wrapper code
            # before the call is added by function_def_str)
            if arg.type.type=='CHARACTER' and not arg.fort_only() and arg.intent=='out':
                file.write('  // Trim trailing whitespace and assign character array to string:\n')
                file.write('  for (int i=' + str(arg.type.str_len-1) + '; ' + arg.name + "_c[i]==' '; i--) " + arg.name + "_c[i] = '\\0';\n")
                file.write('  ' + arg.name + '->assign(' + arg.name + '_c);\n')
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
    """
    includes = set()
    for proc in object.procs:
        for argname,arg in proc.args.iteritems():
            if arg.native:
                includes.add(arg.type.type)
            if arg.type.matrix:
                includes.add(matrix_classname)
    return includes

def write_global_header_file():
    f = open(include_output_dir+'/' + 'FortWrap.h','w')
    f.write(HEADER_STRING + '\n')
    for objname,obj in objects.iteritems():
        f.write('#include "' + objname + '.h"\n')
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
    f.write('#include <stdlib.h>\n#include <cassert>\n\n')
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
        if obj.name == orphan_classname:
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
        

class Options:
    def __init__(self):
        self.parse_args()

    def usage(self,exit_val=1):
        print "Usage:", sys.argv[0], "[options]\n"
        print "-v, --version\t: Print version information and exit"
        print "-h, --help\t: Print this usage information"
        print "-n\t\t: Run parser but do not generate any wrapper code (dry run)"
        print "-c FC\t\t: Use name mangling for Fortran compiler FC.  Only supports g95\n\t\t  and gfortran"
        print "-g\t\t: Wrap source files found in current directory (glob)"
        print "-d dir\t\t: Output generated wrapper code to dir"
        print "--file-list=f\t: Read list of Fortran source files to parse from file f"
        print "--clean\t\t: Remove all wrapper-related files from wrapper code directory\n\t\t  before generating new code.  Requires -d"
        sys.exit(exit_val)

    def parse_args(self):
        global code_output_dir, include_output_dir, fort_output_dir, compiler
        try:
            # -g is to glob working directory for files
            opts, args = getopt.getopt(sys.argv[1:], 'hvc:gnd:', ['file-list=','clean','help','version'])
        except getopt.GetoptError, err:
            print str(err)
            self.usage()

        if args:
            self.usage()

        self.inputs_file = ''
        self.glob_files = False
        self.clean_code = False
        self.dry_run = False
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
                code_output_dir = a
                include_output_dir = a
                fort_output_dir = a
            elif o=='-c':
                if a!='g95' and a!='gfortran':
                    print "Error, only g95 and gfortran name mangling supported"
                    sys.exit(1)
                    compiler = a
            elif o=='-n':
                self.dry_run = True
            elif o=='--clean':
                self.clean_code = True

        if self.clean_code and code_output_dir=='.':
            print "Error, cleaning wrapper code output dir requires -d"
            sys.exit(2)


# COMMANDS ==========================================

if __name__ == "__main__":

    opts = Options()

    if opts.clean_code:
        clean_directories()

    read_substitutions()
    read_ignores()
    read_includes()
    read_orphans()

    file_list = []

    if opts.inputs_file:
        # TODO: Add error handler
        f = open(opts.inputs_file)
        for line in f:
            if not line.strip().startswith('#') and re.search('\w', line):
                file_list.append( line.strip() )
        f.close()
        print "LOADED", len(file_list), 'FILES FROM LIST'

    if opts.glob_files:
        file_list += glob.glob('*.f90')
    
    if not file_list:
        print "Error: no source files"
        sys.exit(2)

    for f in file_list:
        parse_file(f)
    associate_procedures()

    if opts.dry_run:
        sys.exit(0)

    for obj in objects.itervalues():
        write_class(obj)

    write_global_header_file()
    write_misc_defs()
    write_matrix_class()
    write_fortran_wrapper()

    sys.exit(0)
