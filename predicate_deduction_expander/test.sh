#!/bin/bash
ls tests|grep '\.in$'|sed 's/\.in//' | while read testName; do
    testName="tests/$testName"
    ./deduction_expander "$testName.in" "$testName.out"
done
