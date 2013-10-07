#!/bin/bash
ls tests|grep \\.in$|sed -r 's/.in$//'|while read id; do
    java -jar ProveChecker.jar "tests/$id.in" out.txt
    diff_out=$(diff "tests/$id.out" out.txt) 
    if [ "$diff_out" != '' ]; then
        echo "Test $id failed!"
    fi
done
