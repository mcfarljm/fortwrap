#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: tag_release.sh version"
    exit
fi

version=$1

svn cp . file:///home/mcfarljm/work/svn_repos/fortwrap/tags/release-$version