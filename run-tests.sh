#!/bin/sh

TEST_FILES=$(find ./tests -type f -name *.scm) 

if [ -z "$SCHEME_CMD" ]; then
    SCHEME_CMD="chezscheme --libdirs .. --script"
fi

echo "Running tests with" $(echo $SCHEME_CMD | cut -d' ' -f1)

START=`date "+%s%3N"`
FAILURES=0

for f in $TEST_FILES; do
    echo "> $SCHEME_CMD $f $@"
    $SCHEME_CMD $f $@
    FAILURES=$(($FAILURES + $?))
done

END=`date "+%s%3N"`

if [ $FAILURES -eq 0 ]; then
    DURATION="$(($END-$START))"
    echo "Tests passed in $DURATION milliseconds"
    echo "$(git show | head -n1): $DURATION" >> perf.txt
else
    echo "Some Tests Failed!"
fi
