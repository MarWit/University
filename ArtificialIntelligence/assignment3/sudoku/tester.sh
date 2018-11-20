#!/usr/bin/env bash

function test_it() {
    in="$1"
    out="$in.out"

    ./sudoku.py<$in > "$in.pl"
    swipl -c "$in.pl" > "$in.my" 2>/dev/null
    diff -w $out "$in.my" >/dev/null 2>&1
    if [ "$?" = "0" ]; then
        echo "OK!"
    else
        echo "FAILED!"
    fi
}

tests=(test1 test2 test3)

for t in ${tests[@]}; do
    echo "$t"
    time test_it "$t"
done
