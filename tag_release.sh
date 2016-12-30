#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: tag_release.sh version"
    exit
fi

version=$1

svn cp ^/trunk ^/tags/release-$version -m "Tagging version $version"
