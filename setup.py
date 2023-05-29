import imp
from setuptools import setup
from pathlib import Path
from glob import glob

this_dir = Path(__file__).parent
readme = (this_dir / 'README.md').read_text()
setup(
    name='fortwrap',
    version='2.3.1',
    packages=['fortwrap'],
    description='FortWrap is a python script that parses Fortran 90/95/2003 source files and generates wrapper code for '
                'interfacing with the original Fortran code from C++. FortWrap is intended to be used with Fortran code '
                'that takes an object oriented approach and makes use of Fortran derived types. The resulting wrapper '
                'code provides a C++ interface that wraps the Fortran derived types with C++ "proxy classes"',
    long_description=readme,
    long_description_content_type='text/markdown',
    author='John McFarland',
    author_email='mcfarljm@gmail.com',
    url='https://github.com/mcfarljm/fortwrap'
)