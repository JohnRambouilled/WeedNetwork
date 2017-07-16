#!/bin/bash


function scanFile {
    res=$(cat $1 | grep TODO)
    if [ ! "$res" == "" ] ; then
        echo ""
        echo $1
        echo $res
    fi
}


export -f scanFile
#find -name "*.hs" -exec sh -c"cat {} | grep TODO" \;
find -name "*.hs" -exec bash -c 'scanFile "$0"' {} \;


