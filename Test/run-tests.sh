#!/bin/bash

DIR=$(pwd)

if [[ ! -e "$DIR/vl2c" ]]
then
    echo "The file 'vl2c' not found.  Run 'make vl2c' first."
    exit 1
fi

PASSED=0
FAILED=0

for file in Test/vl/*.vl
do
    NAME=${file%.vl}
    RECEIVED=$($DIR/vl2c $NAME.vl $NAME.c \
        && gcc $NAME.c -o $NAME.out && ./$NAME.out)
    EXPECTED=$(tail -1 $NAME.vl | tr -d ";\n ")
    if [[ "$RECEIVED" == "$EXPECTED" ]]
    then
        printf "%-50s PASSED\n" $file
        PASSED=$(($PASSED+1))
    else
        printf "%-50s FAILED\n" $file
        echo "expected: $EXPECTED"
        echo "received: $RECEIVED"
        FAILED=$(($FAILED+1))
    fi
done

TOTALLY=$(($PASSED+$FAILED))

echo "Totally $TOTALLY test(s), $PASSED passed, $FAILED failed."
