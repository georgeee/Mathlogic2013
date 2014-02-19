#!/bin/bash
ls tests| grep '\.in$' | sed 's/\.in$//' | while read testName; do
    testName="tests/$testName"
    ./deduction_expander "$testName.in" "$testName.out"
    echo ''
    echo ''
    echo '========================'
    echo "Test $testName"
    cat "$testName.in"
    echo ">>>>>>>>>>>>>>>>>>"
    cat "$testName.out"
done
