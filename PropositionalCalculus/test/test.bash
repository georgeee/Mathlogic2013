#!/bin/bash
for mode in pc de; do
    dir="tests_$mode"
    ls $dir|grep \\.in$|sed -r 's/.in$//'|while read id; do
            java -jar ../out/artifacts/PropositionalCalculus_jar/PropositionalCalculus.jar -m $mode -i "$dir/$id.in" -o /tmp/pct_out.txt
        diff_out=$(diff "$dir/$id.out" /tmp/pct_out.txt) 
        if [ "$diff_out" != '' ]; then
            echo "Test $id failed!"
        fi
    done
done
