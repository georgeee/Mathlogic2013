#!/bin/bash
find tests|grep '\.in$'|sed 's/\.in//' | while read testName; do
    inFile="$testName.in"
    outFile="$testName.out"
    tmp=`head -1 "$inFile" | grep '|-'`
    echo "========= $inFile =========="
    if [ "$tmp" == "" ]; then
        ./deduction_expander -c "$inFile" "$outFile" 
    else
        ./deduction_expander "$inFile" "$outFile"
    fi
done
