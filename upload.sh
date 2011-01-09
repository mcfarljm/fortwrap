#!/bin/bash

if [ $# -eq 0 ]; then
    echo Usage: upload file1 [files...]
    exit
fi

echo $@

scp -r $@ mcfarljm,fortwrap@frs.sourceforge.net:/home/frs/project/f/fo/fortwrap